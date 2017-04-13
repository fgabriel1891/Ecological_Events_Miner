
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
path <- "/Users/Gabriel/Desktop/ShinyAPPS/LiteratureReader/articles/"
filenames<-list.files(path)
path1 <- paste("/Users/Gabriel/Desktop/ShinyAPPS/LiteratureReader/articles/","/", filenames, sep = "")
names(path1) <- filenames


dictionary.path <- "/Users/Gabriel/Desktop/ShinyAPPS/LiteratureReader/dic"
filenames.dic<-list.files(dictionary.path)
path2 <- paste("/Users/Gabriel/Desktop/ShinyAPPS/LiteratureReader/dic","/", filenames.dic, sep = "")
names(path2) <- filenames.dic


shinyUI(navbarPage("Ecological Event Miner", id ="nav",
                  
                   tabPanel("Instructions",
                            fluidPage( theme = shinytheme("yeti"), 
                              sidebarLayout(
                                sidebarPanel(
                                selectInput(inputId = "dataset",
                                                       label = "Select article to read",
                                                       path1),
                                selectInput(inputId = "dictionary",
                                          label = "Provide a thesaurus",
                                          path2),
                                img(src='logo.png', align = "right"),
                                p("This shiny app is currently in development and under the CreativeCommons CC BY-NC-SA License."),
                                p("Contact Person: Gabriel Muñoz | fgabriel1891@gmail.com")
                            ),
                            mainPanel(
                              includeMarkdown("www/01-Instructions.Rmd")
                            )
                          )
                        )
                      ),
                   tabPanel("Species names",
                            splitLayout(
                              plotOutput("dictionary"),
                              tableOutput("data_table")
                              )
                            ),
                   tabPanel("Indexed version",
                            fluidPage(
                              sidebarLayout(
                                sidebarPanel(
                                  plotOutput("plot"),id="myplot"),
                              mainPanel(
                                tableOutput("Indexed.version"),id="Index")
                              )
                            )
                   )
  )
)
                   
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
# 
#     
