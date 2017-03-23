
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  
read <- eventReactive(input$dataset,{
  
# Read the pdf
 fulltext::ft_extract(input$dataset)
 
  
})

# Custom fucntion to index

Index <- function(read,verbatim, dictionary) {
  
  IndexText <- list()
  for(i in 1:length(verbatim$offsetend)){
    IndexText[[i]] <- stringr::str_sub(iconv(enc2utf8(read),sub="byte"),verbatim$offsetstart[i]-50,verbatim$offsetend[i]+250)
    
  }
  matches <- c()
  ToMatch <- c()
  for ( i in 1:length(IndexText)){
    matches[[i]] <- grepl(paste(dictionary$term, collapse = "|"),IndexText[i])
  }
  df <- c()
  df$text <- IndexText[matches==TRUE]
  df$which <- which(matches == TRUE)
  return(df)
}


#return( print(IndexText[which(matches[[i]] == TRUE)]))



  

# Tab with Scientific Names found and count

  output$data_table <- renderTable({
    #     Find scientific names
    scinames.d <- taxize::scrapenames(text = read()$data, all_data_sources = TRUE)
    
    sciname <- scinames.d$data$scientificname # output 
    table(sciname,dnn = "Count")[order(table(sciname), decreasing = T)]
    
  })
  
# Tab to show the dictionary
    output$dictionary <- renderTable({
      # Read the dictionary
      dictio <- read.csv(input$dictionary, header = TRUE)
      

    })
    
# Tab to show the indexed results

     
    output$Indexed.version <- renderTable({
      dictio <- read.csv(input$dictionary, header = TRUE)
      scinames.d <- taxize::scrapenames(text = read()$data, all_data_sources = TRUE)
      verbatim <-scinames.d$data[c("verbatim","offsetend", "offsetstart")]
    table(unlist( Index(read()$data, verbatim,dictio)$text),deparse.level = 0)
    
    })
    
    output$plot <- renderPlot({
      dictio <- read.csv(input$dictionary, header = TRUE)
      scinames.d <- taxize::scrapenames(text = read()$data, all_data_sources = TRUE)
      verbatim <-scinames.d$data[c("verbatim","offsetend", "offsetstart")]
      matches <- Index(read()$data, verbatim,dictio)$which
      
      
      plot(verbatim$offsetstart[matches], 
           xlab = "String match rank", ylab = "Position in the text article (in characters)", 
           ylim = c(0,nchar(iconv(enc2utf8(read()$data),sub="byte"))),
           col = "#5ba966",pch = 16,
           main = "position on the text")
      legend("bottomright", "Position of record \n along the text",pch = 16, col = "#5ba966", bty = "y" )
    })
})
