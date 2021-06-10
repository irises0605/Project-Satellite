# Satellite: Interactive Data Visualization with Shiny
# Tess White, Iris Liu, Tahseen Rashid, Meredith Spencer

# packages 
library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyFeedback)
library(shinyWidgets)
library(plotly)
library(dendextend)
library(metricsgraphics)
library(rsconnect)

# datasets
satellite_data <- readr::read_csv("data/satellite_data.csv")
satellite_by_country <- readr::read_csv("data/satellite_by_country.csv")
users_data <- readr::read_csv("data/users.csv")
apogee_perigee <- readr::read_csv("data/apogee_perigee.csv")
satellite_by_year <- readr::read_csv("data/satellite_by_year.csv")
purposes_data <- readr::read_csv("data/purposes.csv")

# graph theme 
graph_theme <- theme(
  panel.grid = element_line(color = "#969696"), 
  panel.background = element_rect(fill = "#f7f7f7"), 
  text = element_text(face = "bold", family = "Helvetica", 
    color = "#252525", size = 12), 
  plot.subtitle = element_text(size = 10), 
  axis.text = element_text(size = 10),
  axis.ticks = element_line(color = "#969696")
)

# server
function(input, output) { 
  
  # satellite picture on overview page 
  output$Satellite <- renderImage({
    
    list(src = "image/satellite_image.jpg", alt = paste("Image number"))
    
  }, deleteFile = FALSE)
  
  # satellite distribution by country
  output$plot1 <- renderPlotly({
    
    # manipulate dataset
    df <- subset(satellite_by_country, YEAR == input$year)
    df$logCOUNT <- log(df$COUNT+1)
    df$hover <- with(df, paste('<br>', COUNTRY, '<br>', "Number of Satellite:", COUNT))
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    # displaying figure
    fig <- plot_geo(df, z = ~logCOUNT) %>% 
      add_trace(colors = 'Blues', color = ~logCOUNT, zmin = 0, zmax = log(1878+1),
        zmax = 1600, text = ~hover, locations = ~CODE, marker = list(line = l), 
        hoverinfo = paste('text')) %>%
      hide_colorbar() %>%
      layout(title = paste("<br> Cumulative Number of Active Satellites per Country in", input$year),
        geo = g)
  })
  
  # change in satellite by year
  satellite_by_year$contractor <- ifelse(satellite_by_year$Contractor == "SpaceX", 
    "SpaceX", "Not SpaceX") 
  
  output$state_plot1 <- renderPlotly({
    
    q <- ggplot(satellite_by_year, aes(x = launch_year, fill = contractor)) +
      geom_bar(position = position_stack(reverse = TRUE)) +
      labs(x = "Launch Year", y = "Count", 
        fill = "Contractor", title = 'Satellites Launched Per Year') +
      graph_theme

    ggplotly(q)
    
  })
  
  # expected lifetime scatterplot
  output$expected_life_time_plot2 <- renderPlotly({
    
    if(input$color_code){
      base_plot4 <- ggplot(data = satellite_data, 
        aes(x = `Launch Year`, y = `Expected Lifetime`, 
        color = Purpose)) 
    }
    else{
      base_plot4 <- ggplot(data = satellite_data, 
        aes(x = `Launch Year`, y = `Expected Lifetime`))
    }
    
    p4 <- base_plot4 + geom_jitter(width = 0.1, alpha = 0.5, aes(text = paste("Name:", Name))) + 
      geom_smooth(method = "loess", se = TRUE) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_y_continuous(breaks = seq(0, 35, 5)) + 
      scale_x_continuous(breaks = seq(1985, 2020, 5)) + 
      labs(title = "Expected Lifetime of Satellites Over Time", 
        x = "Year of Launch", 
        y = "Expected Lifetime (years) ", 
        color = "Purpose of Satellite") + 
      graph_theme
    
    if(input$facet){
      p4 <- p4 + facet_wrap(~Purpose, drop = TRUE) + 
        labs(title = "Expected Lifetime of Satellites Over Time", 
          x = paste("\n", "\n", "\n", "Year of Launch"), 
          y = "Expected Lifetime (years)") + 
        theme(legend.position = "none")
    }
    
    ggplotly(p4)
    
  })
  
  # dry/launch mass scatterplot 
  output$mass_plot2 <- renderPlotly({
    
    if(input$color_code2){
      base_plot3 <- ggplot(data = satellite_data, 
        aes_string(x = "`Launch Year`", y = input$which_mass, 
        color = "Purpose")) 
    }
    else{
      base_plot3 <- ggplot(data = satellite_data, 
        aes_string(x = "`Launch Year`", y = input$which_mass))
    }
    
    p3 <- base_plot3 + 
      geom_jitter(width = 0.3, height = 0.5, alpha = 0.5, aes(text = paste("Name:", Name))) + 
      geom_smooth(method = "loess", se = TRUE) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_x_continuous(breaks = seq(1985, 2020, 5)) + 
      labs(title = paste(input$which_mass, "Mass of Satellites Over Time"), 
        x = "Year of Satellite Launch", 
        y = paste(input$which_mass, "Mass (kg)"), 
        color = "Purpose of Satellite") + 
      graph_theme
    
    if(input$facet2) {
      p3 <- p3 + facet_wrap(~Purpose, drop = TRUE) + 
        labs(title = paste(input$which_mass, "Mass of Satellites Over Time"), 
          x = paste("\n", "\n", "\n", "Year of Launch"),
          y = paste(input$which_mass, "Mass (kg)")) + 
        theme(legend.position = "none") +
        graph_theme
    }
    
    ggplotly(p3)
    
  }) 
  
  # orbit period density plot
  output$state_plot2 <- renderPlot({
    
    p <- ggplot(data = satellite_by_year, aes(x = launch_year)) + 
      geom_density(aes(fill = RotationNumberEdited), alpha = 0.3) +
      labs(title = 'Density Plot of Satellite Rotations per Day',
        fill = 'Rotations per Day', x = "Launch Year", y = "Density") + 
      scale_colour_brewer(palette = "Pastel2") + 
      graph_theme
    
    if(input$do_facet) {
      p <- p + facet_wrap(~RotationNumberEdited, drop = TRUE)
    }
    
    p
    
  })
  
  # apogee and perigee scatterplot
  scatter <- mjs_plot(apogee_perigee, x = Apogee_km, y = Perigee_km) %>%
    mjs_point(color_accessor = Eccentricity, size_accessor = Eccentricity) %>%
    mjs_labs(x="Apogee (km)", y="Perigee (km)")
  
  output$scatter <- renderMetricsgraphics(scatter)
  
  # ellipse diagram 
  output$Ellipse <- renderImage({
    list(src = "image/ellipse_diagram.png", alt = paste("Image number"))
  }, deleteFile = FALSE)
  
  # moon orbit diagram 
  output$Moon <- renderImage({
    list(src = "image/moon_orbit.jpg", alt = paste("Image number"))
  }, deleteFile = FALSE)
  
  # apogee and perigee scatterplot 
  output$scatter2 <- renderPlot({
    
    e <- ggplot(satellite_data, 
      aes(x = Apogee_km, y = Perigee_km, 
        color = Purpose, size = Eccentricity)) +
      geom_point() +
      labs(x = "Apogee (km)", y  = "Perigee (km)") + 
      graph_theme 
    
    e
  })
  
  # proportional bar chart by user
  output$plot2 <- renderPlotly({
    
    select_user <- append(input$select_country_list, "Global")
    if (is.null(input$select_country_list)){
      select_user <- c("Global", "USA", "UK", "China", "Russia")
    }
    select_df <- subset(users_data, Country %in% select_user)

    g <- list(
      tickfont = list(size=11.7),
      overlaying = "y",
      nticks = 5,
      side = "left",
      title = "Proportion"
    )
    
    plot2 <- ggplot(select_df, aes(x = Country, fill = Use, y = Proportion)) + 
      geom_bar(position='stack', stat='identity') + 
      ggtitle ("Different Satellite Use by Country") +
      labs(x = "Selected Country", y = "Percentage of Satellite", fill = "Type of User") +
      scale_y_continuous(labels = scales::percent) + 
      theme(axis.line=element_blank(),
        axis.text.x= element_text() ,
        axis.text.y=element_text(),
        axis.ticks=element_line(),
        axis.title.x=element_text(),
        axis.title.y=element_text(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) + 
      scale_fill_manual(
        values = c(
          "Civil" = "#fa9fb5",
          "Military" = "#3182bd",
          "Commercial" = "#fdae6b",
          "Government" = "#2ca25f")) + 
      graph_theme
    
    ggplotly(plot2, toolip = c("x", "fill", "percent"))
    
  })
  
  # proportional bar chart by purpose
  output$purposePlot <- renderPlotly({
    
    select_user2 <- append(input$select_country_list2, "Global")
    if (is.null(input$select_country_list2)){
      select_user2 <- c("Global", "USA", "UK", "China", "Russia")
    }
    select_df2 <- subset(purposes_data, Country %in% select_user2)
    
    g2 <- list(
      tickfont = list(size=11.7),
      overlaying = "y",
      nticks = 5,
      side = "left",
      title = "Proportion"
    )
    
    prop_plot <- ggplot(select_df2, aes(x = Country, fill = Purpose, y = Proportion)) + 
      geom_bar(position='stack', stat='identity') + 
      ggtitle ("Different Satellite Purposes by Country") +
      labs(x = "Selected Country", y = "Percentage of Satellite", fill = "Purpose of Satellite") +
      theme(axis.line=element_blank(),
        axis.text.x= element_text() ,
        axis.text.y=element_text(),
        axis.ticks=element_line(),
        axis.title.x=element_text(),
        axis.title.y=element_text(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) + 
      graph_theme 
    
    ggplotly(prop_plot)
  
  })
}