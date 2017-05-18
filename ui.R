
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




# key <- "cf0b9da7695ba68256cd61ee7fe04cbf84ae4ede" # To change this for "accept other keys"

shinyUI(navbarPage("Ecological Event Miner", id ="nav",
                  
                   tabPanel("Instructions",
                            fluidPage( theme = shinytheme("yeti"), 
                              sidebarLayout(
                                sidebarPanel(
                                selectInput(inputId = "dictionary",
                                          label = "Select the dictionary",
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
                   tabPanel("Taxonomic summary",
                            fluidPage(
                              
                              fluidRow(
                                column(6,selectInput(inputId = "articlePath",
                                            label = "Select an article to read",
                                            path1)),
                                column(6,actionButton(inputId = "GoButton",
                                             label = "Read!"))),
                             
                              fluidRow(
                                column(5,h2("5 most matched topics")),
                                column(7, h3("Indexed snippets"))),
                              fluidRow(
                                column(5,plotOutput("dictionary", height = 300)),
                                column(7, plotOutput("plot", height = 300))),
                              
                              fluidRow(
                                column(5,h2("Taxonomic Summary")),
                                column(7, h3("Text snippets"))),
                              
                              fluidRow(
                                column(5, DT::dataTableOutput("data_table", width = "90%")),
                                column(7, DT::dataTableOutput("Indexed.version", width = "90%")))
                                
                   ))
  ))




                   
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
