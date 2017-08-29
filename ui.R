
# Literature reader 
# Shiny app development 
# Gabriel Muñoz 
# University of Amsterdam
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(shinythemes)
path <- "articles/"
filenames<-list.files(path)
path1 <- paste("articles","/", filenames, sep = "")
names(path1) <- filenames


dictionary.path <- "dic/"
filenames.dic<-list.files(dictionary.path)
path2 <- paste("dic","/", filenames.dic, sep = "")
names(path2) <- filenames.dic

files <- c("1-s2.0-S037663571500056X-main.pdf","Diet Composition_NEBIO.pdf","02 David, Manakadan & Ganesh.pdf")


# key <- "cf0b9da7695ba68256cd61ee7fe04cbf84ae4ede" # To change this for "accept other keys"

shinyUI(
  fluidPage(  br(),br(),
    titlePanel("Ecological Events Miner"),br(),br(),
    navbarPage("", id ="nav",
                  # First Tab: 
                  
                   tabPanel("Upload your Articles",
                            fluidPage( theme = shinytheme("spacelab"), 
                              sidebarLayout(
                                sidebarPanel(
                                  p("1: Select files to mine"),
                                fileInput('file1', 'Upload your articles as PDF(s)', accept=c('.pdf'), multiple = T,
                                          placeholder = "No file(s) selected"),
                                p("2: Upload a customized dictionary (optional)"),
                                fileInput('file2', 'Upload a dictionary as .csv or .txt', accept=c('.csv', ".txt"), multiple = F,
                                          placeholder = "No file selected"),
                                p("3: Submit your MonkeyLearn API key (optional)"),
                                textInput("ApiButton", "Give your  API key here", ""),
                                actionButton("submit","Submit"),
                                br(),
                                br(),
                                p("4: Click Read then continue to the other tabs"),
                                actionButton(inputId = "GoButton",
                                             label = "Read!"),
                                br(),
                                br(),
                                img(src='logo.png', align = "left", height = 100),
                                p("This shiny app is currently in development and under the CreativeCommons CC BY-NC-SA License."),
                                p(a(shiny::icon('github fa-2x'),href='https://github.com/fgabriel1891/Ecological_event_miner',target='_blank')),
                                p(h5("Contact Person: Gabriel Muñoz"))
                                
                            ),
                            mainPanel(
                              includeMarkdown("www/01-Instructions.Rmd")
                              
                            
                              
                            )
                          )
                        )
                      ),
                  
                  # Second Tab: 
                  
                   tabPanel("Taxonomic Overview",
                            
                            column(6,p("Set context limit"),
                                   column(2,sliderInput("up", "left:", min = 0, max = 500, value = 100, step= 10)),
                                   column(2,sliderInput("down", "right:", min = 0, max = 500, value = 100, step= 10)),
                                   br(),
                                   p("Select an species name"),
                                   DT::dataTableOutput("data_table", width = "90%")),
                            column(6, DT::dataTableOutput("context"))),
               tabPanel("Ecological Event Overview", 
                            bootstrapPage(   
                              fluidRow(column(2,uiOutput("names")),
                                      column(2,selectInput(inputId = "dictionary",
                                                           label = "Choose a dictionary",
                                                           path2)),
                                      column(2, actionButton(inputId = "GoButton2",
                                                              label = "Index!"))),
                              fluidRow(
                                  tabsetPanel(
                                      tabPanel("Text Snippets",
                                               column(6, DT::dataTableOutput("Indexed.version", width = "90%")),
                                               column(6,
                                                      p("Indexed text summary"),
                                                      plotOutput("dictionary", height = 300))),
                                      tabPanel("Wordcloud",
                                               column(6, 
                                                      p("Indexed positions"),
                                                      plotOutput("plot", height = 300)), 
                                        column(6,
                                               p("Dictionary matches"),
                                               plotOutput("dictionary2", height = 300))),
                                      tabPanel("Find Tables", actionButton("renderArticle","Render Article"),
                                               verbatimTextOutput("article"))
                                      )
                                  )
                              )
                            ),
                   
                  # Third Tab
                  
                  tabPanel("Geographical Overview",
                           fluidPage(
                           fluidRow(actionButton(inputId = "GoButton3",
                                                 label = "Get Locations!"),
                             tabsetPanel(
                               tabPanel("Map",leafletOutput("map",height = 650)),
                               tabPanel("Locations Mined", DT::dataTableOutput("LocationsText")))
                              )
                           )
                          ),
                  # Fifth tab
                  
                  tabPanel("About",
                           fluidPage(
                             includeMarkdown("www/02-About.Rmd")
                           ))
  )))





                   
#                    
#     
#   sidebarPanel(
#     
#     selectInput(inputId = "key",
#                 label = "Provide a key for MonkeyLearn API" ,
#                 key),
#     
#     img(src='logo.png', align = "right"),
#     br(),

#   mainPanel(
#     h2("Ecological Events Miner"),
#     
#     tabsetPanel(
#       
#       tabPanel("Dictionary",,id="myTab1"),
#       
#     )
#   )
#  )
# )


