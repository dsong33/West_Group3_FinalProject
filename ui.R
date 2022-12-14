#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinyWidgets)
library(DT)
library(ggplot2)

library(shinydashboard)
navbarPage("Interactive Dashboard for South Bend Civic Data", id="main",
           # show a leaflet map plot
  tabPanel("Home Page",
             column(6,includeMarkdown("homepage.md")),
             column(6,img(src='https://www.gannett-cdn.com/presto/2021/12/15/NSBT/21d7319c-b8f2-42af-9e26-e8c8e5938982-civic11082021_01.JPG?width=660&height=528&fit=crop&format=pjpg&auto=webp'))
             ),
  tabPanel("Data Sets", 
             h5(),
             selectInput('datasetname', 'Select a data set', c("Parks locations and features","311 Phone Call","Abandoned houses",'Facilities','Council Districts')),
           #plotOutput('datasetDes'),
                    DT::dataTableOutput("data")),
    
  tabPanel("Abandoned Property Parcels",
                        fluidPage(
                          h5('Bar chart below shows total count abandoned properties in South BendData'),
                          h4('Abandoned Properties Overview'),
                          plotOutput('EDA1')),
                        
                        hr(),
                        
                        fluidPage(h4('Abandoned Properties by Zip Codes and Status'),
                                  h5('Interactive Pie Chart showing abandoned properties status'),
                                  selectInput("input1", "Group by", choices = c('Zip Code','Status')),
                                  conditionalPanel("input.input1=='Status'",
                                                   selectInput('input2', "Status", choices = c("Demolished","Deconstructed","Repaired","Repaired & Occupied","Occupied & Not Repaired"))),
                                  conditionalPanel("input.input1=='Zip Code'",
                                                   selectInput('input3','Zip Code',choices = c("46601", "46613", "46614", "46615", "46616", "46617", "46618", "46619", "46625", "46628"))),
                                  plotOutput('EDA2'))),
                          
                        
               
               
    
                        
                        
                        
  tabPanel("Demolished Properties Map", 
           h5('Map showing demolished properties in different districts in the South Bend area'),
             selectInput("concilinput", "Select a district", choices = c('1301','1302','1303','1304','1305','1306')),
             leafletOutput("leaflet5", height=1000)),
    #tabPanel("Map plot", leafletOutput("leaflet3", height=1000)),
    
                       
      
    
    
    
  tabPanel("Park & Public Facilities", 
             h5('Map showing parks and public facilities in the South Bend area'),
             checkboxInput("PARK","PARK(black)", TRUE),
             checkboxInput("Fire_Station", "FIRE STATION(red)", FALSE),
             checkboxInput("LIBRARY", "LIBRARY(green)", FALSE),
             checkboxInput("Police_Station", "POLICE STATION(blue)", FALSE),
             
             
             leafletOutput("park_business_map", height=1000)),
    
      
      
      # Show a plot of the generated distribution
  tabPanel("311 Phone Call",h5('This plot displays 311 phone calls reasons by each department for any date range.'),sidebarLayout(dateRangeInput("date", "Date range:",
                                                                    start = "2016-09-29",
                                                                    end   = "2017-04-14"),
                                          #h5('DescriptionDescriptionDescriptionDescriptionDescription'),
                                                     # Show a plot of the generated distribution
                                                     mainPanel(
                                                       plotOutput("distPlot",width=2160,height=3840))))
  )
       
        



