#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(leaflet.providers)
library(leaflet.extras)
library(tidyr)
library(countrycode)


data <- read.csv('global_power_plant_database.csv', header = TRUE, na.strings = c('', 'NA'), stringsAsFactors = FALSE)
data$generation_gwh_2013[is.na(data$generation_gwh_2013)] <- 0
data$generation_gwh_2014[is.na(data$generation_gwh_2014)] <- 0
data$generation_gwh_2015[is.na(data$generation_gwh_2015)] <- 0
data$generation_gwh_2016[is.na(data$generation_gwh_2016)] <- 0
data$generation_gwh_2017[is.na(data$generation_gwh_2017)] <- 0
data$estimated_generation_gwh[is.na(data$estimated_generation_gwh)] <- 0
data$year_of_capacity_data[is.na(data$year_of_capacity_data)] <- 9999
data$commissioning_year[is.na(data$commissioning_year)] <- 9999

data <- data %>% replace_na(list(other_fuel1 = 'None', other_fuel2 = 'None', other_fuel3 = 'None', owner = 'Unknown', wepp_id = 'Unknown'))

data$continent <- countrycode(sourcevar = data[, "country_long"], origin = "country.name", destination = "continent")

northdata <- subset(data, ('CAN' == data$country) | ('MEX' == data$country) | ('USA' == data$country)
                    | ('CUB' == data$country) | ('CRI' == data$country) | ('DOM' == data$country) | ('PAN' == data$country) |
                        ('NIC' == data$country) | ('JAM' == data$country) | ('HND' == data$country) | ('GTM' == data$country) | 
                        ('SLV' == data$country) | ('TTO' == data$country))

southdata <- subset(data, ('ARG' == data$country) | ('BRA' == data$country) | ('VEN' == data$country) | ('CHL' == data$country) | 
                        ('BOL' == data$country) | ('URY' == data$country) | ('PRY' == data$country) | ('PER' == data$country) | ('COL' == data$country) | 
                        ('ECU' == data$country) | ('GUY' == data$country) | ('GUF' == data$country))


continents <- c('North America', 'South America', 'Africa', 'Europe', 'Asia', 'Australia', 'Antarctica')
sources <- c('All', 'Biomass', 'Coal', 'Cogeneration', 'Gas', 'Geothermal', 'Hydro', 'Nuclear', 'Oil', 'Other', 'Petcoke', 'Solar', 'Storage', 'Waste', 'Wave and Tidal', 'Wind')
    

# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "Power to the People"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE, 
                     # menu on the left hand side
                     sidebarMenu(
                         menuItem("North America Map", tabName = "north", icon = icon("globe-americas")),
                         menuItem("World Maps", tabName = "world", icon = icon("globe")),
                         menuItem("About", tabName = "credits", icon = icon("check"))
                     )
    ),
    dashboardBody(
        tabItems(
            # Illinois leaflet map for 2018 and checkbox
            tabItem(tabName = "north",
                    fluidRow(
                        column(12,
                               box(title = "Leaflet Map of North American Power Plants", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   leafletOutput("map1")
                               )
                        )
                    )
            ),
            # different state comparisons and different checkboxes
            tabItem(tabName = "world",
                    fluidRow(
                        column(9,
                               box(title = "Leaflet Map of the Different Continents", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   leafletOutput("map2")
                               )
                        ),
                        column(3, 
                        selectInput("Continent", "Select a continent", continents, selected = 'North America'),
                        selectInput("Source", "Select a source to visualize", sources, selected = 'All')
                        ),
                        column(12, 
                               sliderInput("slider1", 'Pick a range: ', min = 0, max = 22500, value = c(0, 22500), width = 1000 
                               ),
                               h2("Control the capacity amounts shown with this slider. ")
                        )
                    )
            ),
            #about
            tabItem(tabName = "credits",
                    h1("About"),
                    h2("Created by: Matthew Ghuneim"),
                    h3("This was an extra credit project for CS 424 Spring 2021. 
                       I did not change the data file outside of R. 
                       The original data file is located here: https://datasets.wri.org/dataset/globalpowerplantdatabase.
                       The code available to run this is located here: https://github.com/mghuneim/424projectX")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map1 <- renderLeaflet({
        colorPalette <- colorFactor(palette = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7',
                                                '#999999', '#990000', '#000000'), domain = northdata$primary_fuel)
        
        leaflet(northdata) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addCircleMarkers(lng=northdata$longitude, lat=northdata$latitude, radius = northdata$capacity_mw/300, color = ~colorPalette(northdata$primary), popup=paste('Country Name:', northdata$country_long, "<br>", 
                                                                                                                                                                        'Plant Name: ', northdata$name, '<br>',
                                                                                                                                                                        'Capacity: ', northdata$capacity_mw, '<br>',
                                                                                                                                                                        'Primary Fuel Type: ', northdata$primary_fuel, '<br>',
                                                                                                                                                                        'Other Fuel Type 1: ', northdata$other_fuel1, '<br>',
                                                                                                                                                                        'Other Fuel Type 2: ', northdata$other_fuel2, '<br>',
                                                                                                                                                                        'Other Fuel Type 3: ', northdata$other_fuel3, '<br>')) %>%
            addResetMapButton() %>%
            addLegend("bottomright", colorPalette, values = northdata$primary_fuel, title = "Energy Sources", opacity = 1) %>%
            setView(-105.2551, 54.5260, zoom = 2.3)
        
    })
    
    dropdownReactive <- reactive({
        newdata <- subset(data, data$capacity_mw >= input$slider1[1] & data$capacity_mw <= input$slider1[2])
        if (input$Source == 'All'){
            newdata
        }
        else if (input$Source == 'Biomass'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Biomass')
        }
        else if (input$Source == 'Coal'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Coal')
        }
        else if (input$Source == 'Cogeneration'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Cogeneration')
        }
        else if (input$Source == 'Gas'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Gas')
        }
        else if (input$Source == 'Geothermal'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Geothermal')
        }
        else if (input$Source == 'Hydro'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Hydro')
        }
        else if (input$Source == 'Nuclear'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Nuclear')
        }
        else if (input$Source == 'Oil'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Oil')
        }
        else if (input$Source == 'Other'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Other')
        }
        else if (input$Source == 'Petcoke'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Petcoke')
        }
        else if (input$Source == 'Solar'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Solar')
        }
        else if (input$Source == 'Storage'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Storage')
        }
        else if (input$Source == 'Waste'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Waste')
        }
        else if (input$Source == 'Wave and Tidal'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Wave and Tidal')
        }
        else if (input$Source == 'Wind'){
            newdata <- subset(newdata, newdata$primary_fuel == 'Wind')
        }
    })
    
    output$map2 <- renderLeaflet({
        data1 <- dropdownReactive()
        northdata <- subset(data1, ('CAN' == data1$country) | ('MEX' == data1$country) | ('USA' == data1$country)
                            | ('CUB' == data1$country) | ('CRI' == data1$country) | ('DOM' == data1$country) | ('PAN' == data1$country) |
                                ('NIC' == data1$country) | ('JAM' == data1$country) | ('HND' == data1$country) | ('GTM' == data1$country) | 
                                ('SLV' == data1$country) | ('TTO' == data1$country))
        if (input$Continent == 'North America'){
            colorPalette <- colorFactor(palette = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7',
                                                    '#999999', '#990000', '#000000'), domain = northdata$primary_fuel)
            
            leaflet(northdata) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addCircleMarkers(lng=northdata$longitude, lat=northdata$latitude, radius = northdata$capacity_mw/300, color = ~colorPalette(northdata$primary), popup=paste('Country Name:', northdata$country_long, "<br>", 
                                                                                                                                                                            'Plant Name: ', northdata$name, '<br>',
                                                                                                                                                                            'Capacity: ', northdata$capacity_mw, '<br>',
                                                                                                                                                                            'Primary Fuel Type: ', northdata$primary_fuel, '<br>',
                                                                                                                                                                            'Other Fuel Type 1: ', northdata$other_fuel1, '<br>',
                                                                                                                                                                            'Other Fuel Type 2: ', northdata$other_fuel2, '<br>',
                                                                                                                                                                            'Other Fuel Type 3: ', northdata$other_fuel3, '<br>')) %>%
                addResetMapButton() %>%
                addLegend("bottomright", colorPalette, values = northdata$primary_fuel, title = "Energy Sources", opacity = 1) %>%
                setView(-105.2551, 54.5260, zoom = 2.3)
        }
        else if (input$Continent == 'South America'){
            data2 <- dropdownReactive()
            southdata <- subset(data2, ('ARG' == data2$country) | ('BRA' == data2$country) | ('VEN' == data2$country) | ('CHL' == data2$country) | 
                                    ('BOL' == data2$country) | ('URY' == data2$country) | ('PRY' == data2$country) | ('PER' == data2$country) | ('COL' == data2$country) | 
                                    ('ECU' == data2$country) | ('GUY' == data2$country) | ('GUF' == data2$country))
            colorPalette <- colorFactor(palette = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7',
                                                    '#999999', '#990000', '#000000'), domain = southdata$primary_fuel)
            
            leaflet(southdata) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addCircleMarkers(lng=southdata$longitude, lat=southdata$latitude, radius = southdata$capacity_mw/300, color = ~colorPalette(southdata$primary), popup=paste('Country Name:', southdata$country_long, "<br>", 
                                                                                                                                                                            'Plant Name: ', southdata$name, '<br>',
                                                                                                                                                                            'Capacity: ', southdata$capacity_mw, '<br>',
                                                                                                                                                                            'Primary Fuel Type: ', southdata$primary_fuel, '<br>',
                                                                                                                                                                            'Other Fuel Type 1: ', southdata$other_fuel1, '<br>',
                                                                                                                                                                            'Other Fuel Type 2: ', southdata$other_fuel2, '<br>',
                                                                                                                                                                            'Other Fuel Type 3: ', southdata$other_fuel3, '<br>')) %>%
                addResetMapButton() %>%
                addLegend("bottomright", colorPalette, values = southdata$primary_fuel, title = "Energy Sources", opacity = 1) %>%
                setView(-55.4915, -20, zoom = 3)
        }
        else if (input$Continent == 'Africa'){
            data3 <- dropdownReactive()
            afdata <- subset(data3, data3$continent == 'Africa')
            colorPalette <- colorFactor(palette = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7',
                                                    '#999999', '#990000', '#000000'), domain = afdata$primary_fuel)
            
            leaflet(afdata) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addCircleMarkers(lng=afdata$longitude, lat=afdata$latitude, radius = afdata$capacity_mw/300, color = ~colorPalette(afdata$primary), popup=paste('Country Name:', afdata$country_long, "<br>", 
                                                                                                                                                                            'Plant Name: ', afdata$name, '<br>',
                                                                                                                                                                            'Capacity: ', afdata$capacity_mw, '<br>',
                                                                                                                                                                            'Primary Fuel Type: ', afdata$primary_fuel, '<br>',
                                                                                                                                                                            'Other Fuel Type 1: ', afdata$other_fuel1, '<br>',
                                                                                                                                                                            'Other Fuel Type 2: ', afdata$other_fuel2, '<br>',
                                                                                                                                                                            'Other Fuel Type 3: ', afdata$other_fuel3, '<br>')) %>%
                addResetMapButton() %>%
                addLegend("bottomright", colorPalette, values = afdata$primary_fuel, title = "Energy Sources", opacity = 1) %>%
                setView(34.5085, 0, 2.5)
        }
        else if (input$Continent == 'Europe'){
            data4 <- dropdownReactive()
            eudata <- subset(data4, data4$continent == 'Europe')
            colorPalette <- colorFactor(palette = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7',
                                                    '#999999', '#990000', '#000000'), domain = eudata$primary_fuel)
            
            leaflet(eudata) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addCircleMarkers(lng=eudata$longitude, lat=eudata$latitude, radius = eudata$capacity_mw/300, color = ~colorPalette(eudata$primary), popup=paste('Country Name:', eudata$country_long, "<br>", 
                                                                                                                                                                'Plant Name: ', eudata$name, '<br>',
                                                                                                                                                                'Capacity: ', eudata$capacity_mw, '<br>',
                                                                                                                                                                'Primary Fuel Type: ', eudata$primary_fuel, '<br>',
                                                                                                                                                                'Other Fuel Type 1: ', eudata$other_fuel1, '<br>',
                                                                                                                                                                'Other Fuel Type 2: ', eudata$other_fuel2, '<br>',
                                                                                                                                                                'Other Fuel Type 3: ', eudata$other_fuel3, '<br>')) %>%
                addResetMapButton() %>%
                addLegend("bottomright", colorPalette, values = eudata$primary_fuel, title = "Energy Sources", opacity = 1) %>%
                setView(15.2551, 54.5260, 3)
        }
        else if (input$Continent == 'Asia'){
            data5 <- dropdownReactive()
            asdata <- subset(data5, data5$continent == 'Asia')
            colorPalette <- colorFactor(palette = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7',
                                                    '#999999', '#990000', '#000000'), domain = asdata$primary_fuel)
            
            leaflet(asdata) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addCircleMarkers(lng=asdata$longitude, lat=asdata$latitude, radius = asdata$capacity_mw/300, color = ~colorPalette(asdata$primary), popup=paste('Country Name:', asdata$country_long, "<br>", 
                                                                                                                                                                'Plant Name: ', asdata$name, '<br>',
                                                                                                                                                                'Capacity: ', asdata$capacity_mw, '<br>',
                                                                                                                                                                'Primary Fuel Type: ', asdata$primary_fuel, '<br>',
                                                                                                                                                                'Other Fuel Type 1: ', asdata$other_fuel1, '<br>',
                                                                                                                                                                'Other Fuel Type 2: ', asdata$other_fuel2, '<br>',
                                                                                                                                                                'Other Fuel Type 3: ', asdata$other_fuel3, '<br>')) %>%
                addResetMapButton() %>%
                addLegend("bottomright", colorPalette, values = asdata$primary_fuel, title = "Energy Sources", opacity = 1) %>%
                setView(100.6197, 34.0479, 3)
        }
        else if (input$Continent == 'Australia'){
            data6 <- dropdownReactive()
            audata <- subset(data6, data6$continent == 'Oceania')
            colorPalette <- colorFactor(palette = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7',
                                                    '#999999', '#990000', '#000000'), domain = audata$primary_fuel)
            
            leaflet(audata) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addCircleMarkers(lng=audata$longitude, lat=audata$latitude, radius = audata$capacity_mw/300, color = ~colorPalette(audata$primary), popup=paste('Country Name:', audata$country_long, "<br>", 
                                                                                                                                                                'Plant Name: ', audata$name, '<br>',
                                                                                                                                                                'Capacity: ', audata$capacity_mw, '<br>',
                                                                                                                                                                'Primary Fuel Type: ', audata$primary_fuel, '<br>',
                                                                                                                                                                'Other Fuel Type 1: ', audata$other_fuel1, '<br>',
                                                                                                                                                                'Other Fuel Type 2: ', audata$other_fuel2, '<br>',
                                                                                                                                                                'Other Fuel Type 3: ', audata$other_fuel3, '<br>')) %>%
                addResetMapButton() %>%
                addLegend("bottomright", colorPalette, values = audata$primary_fuel, title = "Energy Sources", opacity = 1) %>%
                setView(133.7751, -25.2744, 3)
        }
        else if (input$Continent == 'Antarctica'){
            data7 <- dropdownReactive()
            ardata <- subset(data7, data7$country_long == 'Antarctica')
            colorPalette <- colorFactor(palette = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7',
                                                    '#999999', '#990000', '#000000'), domain = ardata$primary_fuel)
            
            leaflet(ardata) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addCircleMarkers(lng=ardata$longitude, lat=ardata$latitude, radius = ardata$capacity_mw/300, color = ~colorPalette(ardata$primary), popup=paste('Country Name:', ardata$country_long, "<br>", 
                                                                                                                                                                'Plant Name: ', ardata$name, '<br>',
                                                                                                                                                                'Capacity: ', ardata$capacity_mw, '<br>',
                                                                                                                                                                'Primary Fuel Type: ', ardata$primary_fuel, '<br>',
                                                                                                                                                                'Other Fuel Type 1: ', ardata$other_fuel1, '<br>',
                                                                                                                                                                'Other Fuel Type 2: ', ardata$other_fuel2, '<br>',
                                                                                                                                                                'Other Fuel Type 3: ', ardata$other_fuel3, '<br>')) %>%
                addResetMapButton() %>%
                addLegend("bottomright", colorPalette, values = ardata$primary_fuel, title = "Energy Sources", opacity = 1)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
