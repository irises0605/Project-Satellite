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


# graph themes
graph_theme <- theme(panel.grid = element_line(color = "#969696"), 
  panel.background = element_rect(fill = "#f7f7f7"), 
  text = element_text(
    face = "bold", 
    family = "Helvetica", 
    color = "#252525", size = 12), 
  plot.subtitle = element_text(size = 10), 
  axis.text = element_text(size = 10),
  axis.ticks = element_line(color = "#969696")
)

dashboardPage(
  
  skin = "black", 
  
  dashboardHeader(title = "What's Up There? Satellites!", titleWidth = 300),
  
  # sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "tab1"),
      menuItem("What Countries Have Satellites?", tabName = "tab2"),
      menuItem("Satellite Changes Over Time?", tabName = "tab3"),
      menuItem("What are some Orbit Properties?", tabName = "tab4"),
      menuItem("Who uses the Satellites?", tabName = "tab5"),
      menuItem("What are Satellites used for?", tabName = "tab6")
    )
  ), 
  
  # dashboard
  dashboardBody(
    
    tabItems(
      
      # main tab 1: overview
      tabItem(

        tabName = "tab1", 

        fluidRow(
          
          box(
            width = 12, 
            height = "210px", 
            column(width = 6,
              h2("What's Up There? Satellites!"), 
              h3("Tess White, Meredith Spencer, Tahseen Rashid, and Iris Liu"),
            ),
            column(width = 6,
              imageOutput("Satellite", height = "175px"), 
              tags$a(href = "https://www.csoonline.com/article/3538771/volterra-provides-protected-distributed-clouds-for-almost-any-application-or-network.html","Picture source link.")
            )
          ),

          box(
            width = 6, 
            "This application is an overview of all the current satellites orbiting the earth launched 
            since the 1970s. It highlights which countries own satellites, how satellites have changed over
            time, characteristics of satellite orbits, who uses satellites, and the purposes of the satellites.
            We hope you enjoy using this application to learn about the current satellites in space 
            as much as we enjoyed making it!"
          ), 
          
          box(
            width = 6, 
            "The data used in this project was collected by the Union of Concerned Scientists. 
            This data set contains information about the 3,372 satellites currently 
            orbiting Earth as of January 1, 2021. The data set can be found at this", 
            tags$a(href = "https://www.ucsusa.org/resources/satellite-database#.VF_jIlPF8Wg","link.")
          )
        )
      ),
      
      # main tab 2: what countries have satellites? 
      tabItem(
        
        tabName = "tab2", 
              
        box(
          width = NULL,
          h3("World Satellite Distribution Since 1990s"),
          "Of the 3,372 active artificial satellites orbiting the Earth as of January 1, 2021, 
          3,172 were launched independently by 60+ countries in the world, and there are 5 
          satellites launched before 1990 still actively running in space. The United States 
          has 1,878 satellites, which is by far the largest number of any single country, with 
          their nearest competitor, China, accounting for only 412. Drag the slider to see the 
          distribution of satellites launched by countries worldwide from 1990 to 2020.",
          br(), br(), 
          
          box(
            width = NULL,
            background = "olive", 
            plotlyOutput("plot1"),
            fluidRow(
              shiny::column(4, offset = 4,
                sliderInput("year", "Launch Year:",
                min = 1990, max = 2020,
                sep = "", 
                step = 5,
                value = 2020,
                animate = TRUE)
              )
            )
          )
        )
      ), 
      
      # main tab 3: satellite changes over time? 
      tabItem(
        
        tabName = "tab3",
        title = "Satellites Over Time", 
        
        fluidRow(
          
          tabBox(
            
            id = "TessTabs1", 
            height = "1250px", 
            width = "250px", 
                 
            # tab panel 1 - Number of satellites overtime 
            tabPanel(
              title = "Yearly Satellite Launches",  
              column(
                width = 12,     
                h3("Yearly Satellite Launches"), 
                "The first satellite mentioned in this dataset dated back to 1974. 
                However, we can see that the number of satellites launched increased 
                significantly in recent years especially with efforts from Elon 
                Musk’s SpaceX (those labeled 'SpaceX' were satellites launched 
                by SpaceX)", 
                br(), br(), 
                box(
                  width = NULL, 
                  background = "olive", 
                  plotlyOutput("state_plot1")
                )
              )
            ),
            
            # tab panel 2 - Expected satellite lifetimes 
            tabPanel(
              title = "Expected Lifetime of Satellites",
              column(
                width = 12,
                h3("Expected Lifetime of Satellites"), 
                "The following plot shows how the expected lifetimes of satellites have changed over time. 
                Interestingly, we see that the expected lifetime of satellites remained fairly constant 
                until 2015, when the expected lifetime of satellites started to decrease. By color coding 
                by the satellite purpose, one can see that this recent overall decrease in expected lifetime comes 
                likely comes from communication satellites, which until their decrease in recent years
                had an increasing expected lifetime. This change in the trend of communication satellites expected 
                lifetimes is likely due to the large number of communication satellites from SpaceX with small 
                expected lifetimes. Included in the following plot are 296 communication satellites launched in 
                2020 by SpaceX with an expected lifetime of 4 years, which has brought down the trend in expected lifetime.", 
                br(), br(),
                "The satellite with the greatest life expectancy of 30 years was launched in 1998 
                and is used for space observation/science. This in fact is the International Space Station!", 
                br(), br(),
                box(
                  width = NULL, 
                  background = "olive", 
                  checkboxInput("color_code", label = "Color code by satellite purpose?"),
                  checkboxInput("facet", label = "Create separate graphs for each satellite purpose?"), 
                  plotlyOutput("expected_life_time_plot2")
                )
              )
            ),
            
            # tab panel 3 - Dry/Launch Mass scatterplots 
            tabPanel(
              title = "Mass of Satellites", 
              column(
                width = 12,
                h3("Mass of Satellites"), 
                "The following plot shows how the mass of satellites have changed over time.
                Users can designate whether to view launch mass or dry mass. Note that the launch 
                mass of a satellite is the mass at the time of launch (with fuel), while the dry 
                mass is the mass of the satellite during orbit (without fuel).",  
                br(), br(), 
                "From the scatterplot showing launch mass of satellites over time, we see that the launch
                mass remained fairly constant until 2005 and has been decreasing steadily since. Again, this 
                could in part be due to SpaceX satellites, as the following plot has 902 SpaceX satellites 
                with very light launch masses between 227-260kg launched in the last two years, which has 
                likely caused the decrease in the launch mass trend. This can also explain the recent decrease
                in the launch masses of communication satellites you can see when you create separate graphs for the 
                satellite purposes, as these SpaceX satellites are categorized for communication purposes. We also 
                see that while most purposes have seen recent decreases in trends, the launch mass of space 
                observation/science satellites has actually been decreasing since the 1990s.",
                br(), br(), 
                "From the scatterplot showing dry mass of satellites over time, we see that dry
                mass remained fairly constant throughout history until its recent decreasing trend in 
                satellites launched in the last 10 years. By showing the satellites by their purpose 
                in separate graphs, one can see that earth observation/science satellites have had the 
                most variation in dry mass, while space observation/science and navigation/technology 
                have had the least. Note that we have no data for the dry mass of SpaceX satellites.",
                br(), br(), 
                box(
                  width = NULL, 
                  background = "olive", 
                  checkboxInput("color_code2", label = "Color code by satellite purpose?"), 
                  checkboxInput("facet2", label = "Create separate graphs for each satellite purpose?"), 
                  radioButtons("which_mass", label = "Which mass?",
                    choices = c("Launch Mass" = "Launch", "Dry Mass" = "Dry")
                  ), 
                  plotlyOutput("mass_plot2")
                )
              )
            )
          )
        )
      ), 
      
      # main tab 4: what are some orbit properties? 
      tabItem(
        
        tabName = "tab4", 
        
        fluidRow(
          
          tabBox(
            id = "tab4-1",
            height = "2000px", 
            width = "250px",
                 
            # tab panel 1: orbit period
            tabPanel(
              title = "Satellite Orbit Period", 
              column(
                width = 12, 
                h3("Satellite Orbit Period"), 
                "The period of a satellite refers to the time it takes to make one 
                full orbit around the Earth. The dataset included the period for each 
                satellite, and so we used the number of minutes in a day to estimate the 
                number of times each satellite orbits the Earth in one day. Most satellites 
                are able to make one orbit within 96 minutes- these satellites are usually in what is 
                known as sun-synchronous orbit or in polar orbit. The number of these type of 
                satellites has increased significantly in recent years as can be seen in the 
                figure with separate graphs for each satellite period.", 
                br(), br(), 
                box(
                  width = NULL, 
                  background = "olive", 
                  checkboxInput("do_facet", label = "Create separate graphs for each satellite period?"), 
                  plotOutput("state_plot2")
                )
              )
            ),
                 
            # tab panel 2: apogee and perigee
            tabPanel(
              title = "Satellite Apogee and Perigee", 
              column(
                width = 12, 
                h3("How Does Satellite Apogee and Perigee Affect Eccentricity?"),

                h4("What is eccentricity?"),
                imageOutput("Ellipse", height = "256px"), 
                tags$a(href = "https://earthobservatory.nasa.gov/features/OrbitsCatalog","Above picture source link."),
                br(), br(), 
                
                h4("What is the apogee and perigee?"),
                imageOutput("Moon", height = "210px"),
                tags$a(href = "http://arsendarnay.blogspot.com/2013/06/no-apogee-for-perigee.html","Above picture source link."),
                br(),  br(), 
                "As the apogee increases the eccentricity also increases which makes sense 
                because if the satellite orbit is more ellipsoidal rather than spherical there 
                will be a point in the orbit where the apogee is quite large.",
                "Even if the satellite has a high perigee, it could still have a spherical 
                orbit which would me that the eccentricity isn’t necessarily very high.",
                
                box(
                  width = NULL,
                  h3("How does Apogee and Perigee Affect Eccentricity?"), 
                  metricsgraphicsOutput("scatter")
                ),
                "NOTE: In the above plot eccentricity is mapped to the size of the points.",
                br(), br(), 
                "This plot is the same as the one above except now the color of the points is based on what the 
                satellite is being used for. Satellites with a high apogee are used for space observation because 
                there will be a point in their orbit where they are further away from the Earth. Additionally, 
                navigation satellites have a low eccentricity, because it is easier to triangulate satellites with a 
                spherical orbit rather than an elliptical one.", 
                br(), br(), 
                
                box(
                  width = NULL,
                  h3("How does the Satellite's Orbit Relate to its Purpose?"), 
                  plotOutput("scatter2")
                )
              )
            )
          )
        )
      ),
      
      # main tab 5: who uses the satellites? 
      tabItem(
        tabName = "tab5",
        box(
          width = NULL, 
          h3("Compare vs. Contrast Satellite Users"), 
          "The users of satellites worldwide are categorized into 4 major types: Civil,
          Commercial, Government and Military. Check whether your country is a satellite-launching 
          country using the search bar. You can select and compare satellite use of 
          multiple countries and compare them to the global totals.",
          br(), br(), 
          box(
            width = NULL, 
            background = "olive", 
            selectizeInput(
              'select_country_list', label = "Search", choices = users_data$Country,
              values <- c("USA", "United Kingdom", "China", "Russia"),
              options = list(maxItems = 10)
            ),
            plotlyOutput("plot2")
          )
        )
      ),
      
      # main tab 6: what are satellites used for? 
      tabItem(
        tabName = "tab6",   
        box(
          width = NULL, 
          h3("Compare vs. Contrast Satellite Purposes"),
          "The purposes of the satellites are categorized into 6 major types: 
          Earth Observation/Science, Navigation/Technology, Communication,
          Space Observation/Science, Surveillance, and Other. Check whether your 
          country is a satellite-launching country using the search bar. You can 
          select and compare satellite use of multiple countries and compare them 
          to the global totals.",
          br(), br(), 

          box(
            width = NULL, 
            background = "olive", 
            selectizeInput(
              'select_country_list2', label = "Search", choices = purposes_data$Country,
              values <- c("USA", "United Kingdom", "China", "Russia"),
              options = list(maxItems = 10)
            ),
            plotlyOutput("purposePlot")
          )
        )
      )
    )
  )
)