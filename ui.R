
# Literature reader 
# Shiny app development 
# Gabriel Muñoz 
# University of Amsterdam
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

path <- "/Users/Gabriel/Desktop/ShinyAPPS/LiteratureReader/test"
filenames<-list.files(path)
path1 <- paste("/Users/Gabriel/Desktop/ShinyAPPS/LiteratureReader/test","/", filenames, sep = "")

dictionary.path <- "/Users/Gabriel/Desktop/ShinyAPPS/LiteratureReader/dic"
filenames.dic<-list.files(dictionary.path)
path2 <- paste("/Users/Gabriel/Desktop/ShinyAPPS/LiteratureReader/dic","/", filenames.dic, sep = "")

shinyUI(pageWithSidebar(
  headerPanel(
    "Literature reader"
  ),
  sidebarPanel(
    selectInput(inputId = "dataset",
                label = "Select article to read",
                path1
    ),
    selectInput(inputId = "dictionary",
                label = "Provide a thesaurus",
                path2
    )   
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Species names",tableOutput("data_table"),id="myTab"),
      tabPanel("Dictionary",tableOutput("dictionary"),id="myTab1"),
      tabPanel("Indexed version",tableOutput("Indexed.version"),id="Index"),
      tabPanel("plot",plotOutput("plot"),id="myplot"),
      id="Plot_Data"
    )
  )
))

    
