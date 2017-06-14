# 
# # This is the server logic for a Shiny web application.
# # You can find out more about building applications with Shiny here:
# #
# # http://shiny.rstudio.com
# #
# 
# library(shiny)
# library(monkeylearn)

library(stringi)
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
  
  # Function to split text based on positions                    
  splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos))) # from:https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position

  # Custom helper function to "correct" the Identificatons based on the first letters

  famcorrector <- function(list) {
    
    t <- replicate(100,sort(sample(1:length(list[,1]),2)))
    w = c()
    for(i in 1:length(t[1,])){
      w[[i]] <- seq(from = t[,i][1], to = t[,i][2])
    }
    
    f <- c()
    for(i in 1:length(w)) {
      f[[i]] <- list[w[[i]],]
    }
    
    
    e <-c()
    for(i in 1:length(list[,1])){
      e[[i]] <-  substr(f[[i]][,1],1,1)
    }
    
    g <-c()
    for(i in 1:length(list[,1])){
      g[[i]] <- e[[i]] %stri==% e[[i]][1]
    }
  
    
    for(i in 1:length(list[,1])){
      f[[i]][,2][g[[i]]] <- f[[i]][,2][i]
      f[[i]][,3][g[[i]]] <- f[[i]][,3][i]
    }
    
    df = dplyr::bind_rows(f)
    
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    
    
    ff <- df[!is.na(df[,2]),]
    ff <- aggregate(ff, getmode, by = list(ff[,1]))
    
    
    newlist <- ff[,c(3:4)][match(list[,1], ff[,1]),]
    newlist$sp <- list[,1]
    rownames(newlist) <- NULL
    newlist <- newlist[,c(3,1,2)]
    names(newlist) <- c("species", "family", "class")
    return(newlist)}
#
# Custom function to scrapenames, OCR the pdf and return a list of taxonomic entities found (including its identification at family and class) 
  # along with a list of "snippets" of the text of ~600 long that matches both the dictionary terms and the target species of the search. 
  # Requires: a dictionary, a filter and the path where the article (pdf) is stored. This function calls the "Index" function and passes the arguments
  # dictionary and verbatim = list of scientific names found with the taxize::scrapenames function.  
  
  readtext <- function(completePath2, dictionary){
    
                    texto <- fulltext::ft_extract(completePath2) # OCR PDF
                    verbatim12 <- taxize::scrapenames(text = texto$data, all_data_sources = T) # Scrape scientific names
                    namew <- unique(verbatim12$data$scientificname) # Get unique scientific names detected
                    families <- sapply(namew, FUN = function(x) 
                                                          taxize::tax_name(x, get = c("family","class"), 
                                                           db = "ncbi", verbose = F,ask = F)) # Look for families
                    fam <- reshape2::melt(families[3,]) # Create dataframe with families
                    fam$class <- reshape2::melt(families[4,])[,1] # Add class
                    names(fam) = c("family", "species", "class") # Rename
                    #fam <- fam[,c(2,1,3)] # reorder to match helper function
                    #fam <- famcorrector(fam) # apply helper "corrector" function
                    #filter = filter # Apply a filter
                    #custom.filter = c("Calamus","Palmae") # If a custom filter is necessary
                    #palms1 <- na.omit(fam$sp[fam$family == filter])
                    #palms2 <- na.omit(fam$sp[fam$sp == custom.filter])# Filter if necessary with custom.filter
                    #match <- match( palms1,verbatim12$data$scientificname) # Match to arecaceae 
                    #texto <- sapply(pdftools::pdf_text(completePath2[i]), function(x) paste0(x,collapse="")) # If the first option does not work
                    readed <- Index(texto$data, verbatim12$data, dictionary)
                    # Create chunks to summarize text output
                    nam <- readed$where
                    ww <- lapply(data.frame(nam), diff) # Calculate differences between matches
                    bp <- as.numeric(which(unlist(ww) > 600)) # Select breakpoints based on a difference threshold (large of snippets)
                    # create chunks 
                    lmis <- splitAt(nam,bp+1)
                    # Set limits
                    lims <- lapply(lmis, function(x) c(min(x)-200, max(x)+420))
                    # retrieve text
                    chunks <- lapply(lims, function(x) stringr::str_sub(texto$data, x[1],x[2]))
                    #readed <- Index(texto$data, verbatim12$data[match,], dictionary)
                    # create a return object 
                    retorne <- list()
                    retorne$dic <- dictionary
                    retorne$where <- nam
                    retorne$path <- completePath2
                    retorne$names <- fam
                    retorne$found <- verbatim12$data
                    retorne$article <- texto$data
                    retorne$chunks <- unlist(chunks)
      
                    return(retorne)
                      
  }
  

####### ----- ######
  
read <- eventReactive(input$GoButton,{
    dictio <- read.csv(input$dictionary, header = TRUE, stringsAsFactors = F)
    readtext(input$articlePath, dictio)

})



# Tab with Scientific Names found and count

  output$data_table <- DT::renderDataTable({
    sciname <- read()$found$scientificname 
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
      c <- termcount(read()$dic, read()$article )
      wordcloud::wordcloud(freq = c$count, words = c$term, 
                           max.words = 5, main = "Common terms from dictionary" )

    })

    # Tab to show the indexed results


    output$Indexed.version <- DT::renderDataTable({
      dat <- data.frame(read()$chunks)
      DT::datatable(dat, rownames = F) })

    output$plot <- renderPlot({
      #e = input$Indexed.version_rows_selected

    plot(read()$where,
           xlab = "String match rank", ylab = "Article lenght",
           ylim = c(0,nchar(iconv(enc2utf8(read()$article),sub="byte"))),
           col = "#5ba966",pch = 16,
           main = "position on the text")
           legend("topright", "Position of record \n along the text",pch = 16, 
          col = "#5ba966", bty = "y" )
           #if(length(e)) points(read()$where[e , drop = FALSE], pch = 19, cex = 2)
    })

})


# 
# 
