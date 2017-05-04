
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
                   tabPanel("Taxonomic summary",
                            selectInput(inputId = "selector",
                                        label = "Select an article to read",
                                        path1),
                            
                              p("Taxonomic summary"),
                              DT::dataTableOutput("data_table"),
                              hr(),
                              p("10 most matched topics"),
                              plotOutput("dictionary")
                            ),
                   
                   tabPanel("Indexed version",
                            selectInput(inputId = "selector2",
                                        label = "select an article to read",
                                        path1),
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
