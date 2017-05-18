# 
# # This is the server logic for a Shiny web application.
# # You can find out more about building applications with Shiny here:
# #
# # http://shiny.rstudio.com
# #
# 
# library(shiny)
# library(monkeylearn)

shinyServer(function(input, output, session) {
  

  
  # Define functions: 
  #
  # Custom function to retrieve dictionary matches
  #
  termcount <- function(dictionary, text){
    
                  count <-c()
                  for (i in 1:length(unlist(dictionary))){
                    count$term[i] <- dictionary[i,]
                    count$count[i] <- length(grep(dictionary[i,], iconv(enc2utf8(text),sub="byte")))
                  }
                  
                  b <- data.frame(count)
                  return(b[order(b$count, decreasing = T),]
                  )
                }
  #
  # Custom fucntion to index
  #
  
  #return( print(IndexText[which(matches[[i]] == TRUE)]))
  
  Index <- function(read,verbatim, dictionary) {
    
              IndexText <- list()
              for(i in 1:length(verbatim$offsetend)){
                IndexText[i] <- stringr::str_sub(iconv(enc2utf8(read),sub="byte"),
                                                 verbatim$offsetstart[i]-200,
                                                 verbatim$offsetend[i]+400)
                
              }
              matches <- c()
              ToMatch <- c()
              
              for ( i in 1:length(IndexText)){
                matches[[i]] <- grepl(paste(dictionary[,1], collapse = "|"),IndexText[i])
              }
              df <- c()
              df$text <- unlist(IndexText[matches==TRUE])
              df$where <- verbatim$offsetstart[which(matches == TRUE)]
              
              
              return(df)
            }
 
#
# Custom function to scrapenames, OCR the pdf and return a list of taxonomic entities found (including its identification at family and class) 
  # along with a list of "snippets" of the text of ~600 long that matches both the dictionary terms and the target species of the search. 
  # Requires: a dictionary, a filter and the path where the article (pdf) is stored. This function calls the "Index" function and passes the arguments
  # dictionary and verbatim = list of scientific names found with the taxize::scrapenames function.  
  
  readtext <- function(completePath2, dictionary){
    
                    texto <- fulltext::ft_extract(completePath2)
                    
                    verbatim12 <- taxize::scrapenames(text = texto$data, all_data_sources = T) # Scrape scientific names
                    
                    namew <- unique(verbatim12$data$scientificname) # Get unique scientific names detected
                   
                    families <- sapply(namew, FUN = function(x) 
                                                          taxize::tax_name(x, get = c("family","class"), 
                                                           db = "ncbi", verbose = F,ask = F)) # Look for families
                    
                    fam <- reshape2::melt(families[3,]) # Create dataframe with families
                    
                    fam$class <- reshape2::melt(families[4,])[,1] # Add class
                    
                    names(fam) = c("family", "species", "class") # Rename
                    
                    #filter = filter # Apply a filter
                    #custom.filter = c("Calamus","Palmae") # If a custom filter is necessary
                   # palms1 <- na.omit(fam$sp[fam$family == filter])
                    #palms2 <- na.omit(fam$sp[fam$sp == custom.filter])# Filter if necessary with custom.filter
                    #match <- match( palms1,verbatim12$data$scientificname) # Match to arecaceae 
                    #texto <- sapply(pdftools::pdf_text(completePath2[i]), function(x) paste0(x,collapse="")) # If the first option does not work
                    
                    # Create a return object
                    readed <- list()
                    readed <- Index(texto$data, verbatim12$data, dictionary)
                    #readed <- Index(texto$data, verbatim12$data[match,], dictionary)
                    readed$dir <- completePath2
                    readed$names <- fam
                    readed$verb <- verbatim12$data
                    readed$dic <- dictionary
                    readed$art <- texto$data
                    return(readed)
                  }
####### ----- ######
  
read <- eventReactive(input$GoButton,{
    dictio <- read.csv(input$dictionary, header = TRUE, stringsAsFactors = F)
    readtext(input$articlePath, dictio)

})



# Tab with Scientific Names found and count

  output$data_table <- DT::renderDataTable({
    sciname <- read()$verb$scientificname 
    splist <- table(sciname,dnn = "Species")[order(table(sciname), decreasing = T)]
    splist <- data.frame(splist)
    names(splist) <- c("species", "count")
    splist$family <- read()$names$family[match(splist$species, read()$names$species)]
    splist$class <- read()$names$class[match(splist$species, read()$names$species)]
    DT::datatable(splist,rownames = F)

  })
  
  # output$tax_tree <- renderPlot({
  #   
  #   plot(taxize::class2tree(list))
  # })



 
# Tab to show the dictionary
    output$dictionary <- renderPlot({
      # Read the dictionary
      c <- termcount(read()$dic, read()$art )
      wordcloud::wordcloud(freq = c$count, words = c$term, 
                           max.words = 5, main = "Common terms from dictionary" )

    })

    # Tab to show the indexed results


    output$Indexed.version <- DT::renderDataTable(
      
      matrix(read()$text), server = FALSE)

    output$plot <- renderPlot({
      #e = input$Indexed.version_rows_selected

    plot(read()$where,
           xlab = "String match rank", ylab = "Article lenght",
           ylim = c(0,nchar(iconv(enc2utf8(read()$art),sub="byte"))),
           col = "#5ba966",pch = 16,
           main = "position on the text")
           legend("topright", "Position of record \n along the text",pch = 16, 
          col = "#5ba966", bty = "y" )
           #if(length(e)) points(read()$where[e , drop = FALSE], pch = 19, cex = 2)
    })

})


# 
# 
