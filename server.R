# 
# # This is the server logic for a Shiny web application.
# # You can find out more about building applications with Shiny here:
# #
# # http://shiny.rstudio.com
# #
# 
library(shiny)
library(monkeylearn)
library(shinythemes)
library(DT)
library(stringi)
library(wordcloud)
library(taxize)
library(fulltext)
library(tm)
#library(RWeka)

shinyServer(function(input, output, session) {
  

  
  # Define functions: 
  #
  # Custom function to retrieve dictionary matches
  #
  termcount <- function(dictionary, text){
    
                  count <-c()
                  for (i in 1:length(unlist(dictionary))){
                    count$term[i] <- dictionary[i,]
                    count$count[i] <- length(grep(dictionary[i,], text))
                  }
                  
                  b <- data.frame(count)
                  return(b[order(b$count, decreasing = T),]
                  )
  }
  
  # Function to create a cropus from the indexed snippets
  
  wordcloudChunk <- function( chunks){ 
    
    
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2)) # from https://rstudio-pubs-static.s3.amazonaws.com/118348_a00ba585d2314b3c937d6acd4f4698b0.html
    corpus <- tm::Corpus(tm::VectorSource(chunks))
    corpus <- tm::tm_map(corpus, removeWords, stopwords('english'))
    
    #dtm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
    #freq <- sort(rowSums(as.matrix(dtm)),decreasing = TRUE)
    #freq <- data.frame("terms"=names(freq), "freq"=freq)
    return(corpus)
    }

  
    #
  # Custom function to index
  #
  
  #return( print(IndexText[which(matches[[i]] == TRUE)]))
  
  Index <- function(read,verbatim, dictionary) {
    
              IndexText <- list()
              for(i in 1:length(verbatim$offsetend)){
                IndexText[i] <- stringr::str_sub(read,
                                                 verbatim$offsetstart[i]-100,
                                                 verbatim$offsetend[i]+150)
                
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
  
  # Function to index the ocr text and give context. 
  
  giveContext <- function(text,terms, up, down) {
    indx <- unlist(gregexpr(terms, text))
    cont = sapply(indx, function(x) stringr::str_sub(text, x-up,x+down))
    names(cont) <- names(text) 
    return(cont)}
  
  

  # Function to split text based on positions                    
  splitAt <- function(x, pos){ unname(split(x, cumsum(seq_along(x) %in% pos)))} # from:https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position

  # Get Scientific and family names function
  
  getSnames <- function(path){
    
    print("initiated")
    # Initiate progress bar
    withProgress( value = 0.1 , message = "Scrapping Scientific Names", { 
  # Set an apply function to extract the scientific names of the articles uploaded. 
    scrapeRes <- lapply(path, function(x) tryCatch({scrapenames(file = x, return_content = T)}, error = function(e) NULL ))
    file <- input$file1
    content <- lapply(scrapeRes, function(x) x$meta$content) # return the text content
    names(content) <- file$name
    
    verbatim <- lapply(scrapeRes, function(x) x$data) # return the scientific names found 
    #incProgress(amount = 0.4, message = "Done!" )
    names(verbatim) <- file$name # give proper names
   incProgress(amount = 0.1, message = "Getting family and class of scientific names found" ) # update progress bar
    namew <- lapply(verbatim, function(x) unique(x$scientificname)) # list scientific names 
    namew <- reshape2::melt(namew) # arrage dataset with file
    names(namew) <- c("species", "file") # give proper names
    # Create unique pool of names to identify (Save computation time)
    spfound <- unique(namew$species)
    # Identify family names
    families <- taxize::tax_name(spfound, get = c("family","class"), db = "ncbi", verbose = F,ask = F) # Look for families
    families <- families[,c("family", "class")]
    families <- cbind(families,"species" = spfound)
   incProgress(amount = 0.4, message = "Done!")
    ret <- list()
    ret$content <- content
    ret$verbatim <- verbatim
    print(file)
    ret$file <- file$name
    ret$families <- families
    ret$namew <- namew
    return(ret)
 }) 
} 


#sasa <- getSnames(c("02 David, Manakadan & Ganesh.pdf","1-s2.0-S037663571500056X-main.pdf"))

# Custom function to scrapenames, OCR the pdf and return a list of taxonomic entities found (including its identification at family and class) 
  # along with a list of "snippets" of the text of ~600 long that matches both the dictionary terms and the target species of the search. 
  # Requires: a dictionary, a filter and the path where the article (pdf) is stored. This function calls the "Index" function and passes the arguments
  # dictionary and verbatim = list of scientific names found with the taxize::scrapenames function.  
  
  readtext <- function(text, dictionary,verbatim){
                    readed <- Index(text, verbatim, dictionary)
                    # Create chunks to summarize text output
                    nam <- readed$where
                    ww <- lapply(data.frame(nam), diff) # Calculate differences between matches
                    bp <- as.numeric(which(unlist(ww) > 250)) # Select breakpoints based on a difference threshold (large of snippets)
                    # create chunks 
                    lmis <- splitAt(nam,bp+1)
                    # Set limits
                    lims <- lapply(lmis, function(x) c(min(x)-100, max(x)+170))
                    # retrieve text
                    chunks <- lapply(lims, function(x) stringr::str_sub(text, x[1],x[2]))
                    # create a return object 
                    retorne <- list()
                    retorne$dic <- dictionary
                    retorne$where <- nam
                    retorne$chunks <- unlist(chunks)
                    return(retorne) 
                    }
                      
  
  
  
  # Function to get frequency counts of scientific names found 
  
  getFrecuencyNames <-  function(namelist){ 
    sciname <- namelist
    splist <- table(sciname$species)[order(table(sciname$species), decreasing = T)]
    splist <- data.frame(splist)
    names(splist) <- c("species", "count")
    splist$family <- namelist$family[match(splist$species, namelist$species)]
    splist$class <- namelist$class[match(splist$species, namelist$species)]
    return(splist)
  }
 
  # Scrape locations with monkeylearn 
  
  locScrap <- function(article, key){
    scrap <- list()        
    scrap <- monkeylearn::monkeylearn_extract(article, extractor_id = "ex_isnnZRbS", key=key)
    locs <- scrap$found[scrap$found$tag == "LOCATION"]
    return(scrap)
}
  
# Get the Key imput 
  
  key <- reactive({
    if (input$submit > 0) {
      df <- input$ApiButton
      return(df)
    }
  })
  
## Function to read locations
  
  
  readLocs <- function(txt, locs, len){
    rre <- lapply(locs,function(x) unlist(gregexpr(x, txt)))
    len <- len
    lapply(rre, function(x) ifelse( x > len, stringr::str_sub(txt,x-len,x+len),
                                    stringr::str_sub(txt,x,x+len )))
    
  }
  

####### ----- ######
## UI order 
####### ----- ###### 
  

## FIRST TAB 
  
# Get the name of files uploaded and display it as a dropping list
output$names <- renderUI({
  dat <- input$file1
  selectInput('articlePath', 'Select an article to read', dat$name)
})
  
  # Get the name of files uploaded and display it as a dropping list in the third tab
  output$names2 <- renderUI({
    selectInput('articlePath', 'Select an article to read', input$file2$name)
  })
  
# Reactive expression after pressing "READ" button
read <- eventReactive(input$GoButton,{
  path <- input$file1$datapath
    getSnames(path)

})


### SECOND TAB 

## Taxonomic overview: 

# Tab with Scientific Names found and count
# 
# 
# dictio <- read.csv(input$dictionary, header = TRUE, stringsAsFactors = F)
# path <- input$file1$datapath[match( input$articlePath,input$file1$name)]

  output$data_table <- DT::renderDataTable({
    names <- read()$families
    list <- getFrecuencyNames(names)
    DT::datatable(list,rownames = F)

  })
  
  
  renderContext <- eventReactive(input$data_table_rows_selected,{
      rows <- input$data_table_rows_selected
      names <- read()$families
      list <- getFrecuencyNames(names)
      sub <- list$species[rows]
      cont <- lapply(read()$content, function(x) giveContext(x,sub, input$up,input$down))
      reshape2::melt(cont)
      
  })
  
  output$context <-  DT::renderDataTable({
    DT::datatable(renderContext(), rownames = F)
  })
  
#######
  
  # THIRD TAB
  # Ecological Events Overview
  
  # Index selected articles 
  
  index <- eventReactive(input$GoButton2,{
    
    wh <- match(input$articlePath,input$file1$name)
    
    text <- read()$content[wh]
    
    verb <- read()$verbatim[wh]
    
    dictio <- read.csv(input$dictionary,header = TRUE, stringsAsFactors = F)
    readtext(text[[1]], dictio, verb[[1]])  
    
  })

# Render Article (uselful to copy tables)
  
articleRender <- eventReactive(input$renderArticle, { 
  pdftools::pdf_text(input$file1$datapath[match(input$articlePath,input$file1$name)])
  })

# Wordclouds  


    output$dictionary <- renderPlot({
      # Read the dictionary
      c <- wordcloudChunk(index()$chunks)
      wordcloud::wordcloud(c,scale = c(2,.8),
        max.words = 20, colors = brewer.pal(5,"YlOrRd"))

    })

    
    output$dictionary2 <- renderPlot({
      # Read the dictionary
      c <- termcount(index()$dic,read()$chunks)
      wordcloud::wordcloud(c$term, c$count,scale=c(8,.2), min.freq = 1,
                           max.words = 10, colors = brewer.pal(5,"YlOrRd"))
      
    })
    
    
    # Tab to show the indexed results


    output$Indexed.version <- DT::renderDataTable({
      dat <- data.frame(index()$chunks)
      print(index()$chunks)
      DT::datatable(dat, rownames = F) })

    
    
    # Plot showing the index positions in the article 
    
    output$plot <- renderPlot({
      #e = input$Indexed.version_rows_selected
      wh <- match(input$articlePath,input$file1$name)
      print(wh)
      text <- read()$content[wh]

    plot(index()$where,
           xlab = "String match rank", ylab = "Article lenght",
           ylim = c(0,nchar(text)),
           col = "#5ba966",pch = 16,
           main = "position on the text")
           legend("topright", "Position of record \n along the text",pch = 16, 
          col = "#5ba966", bty = "y" )
           #if(length(e)) points(read()$where[e , drop = FALSE], pch = 19, cex = 2)
    })
    
    
    
    ## Render article text and tables
    
    output$article <- renderText({ 
     articleRender()
    })
    
    
    
    out1 <-  eventReactive(input$GoButton,{
    locScrap(read()$article,key() )})
    
   geocoder <-  function(out1){ 
      fram <- data.frame(out1())
      fram <- fram[fram$tag == "LOCATION",] # Get only location names
      fram <- fram[,c(1,3)]
      names(fram) <- c("Count","Location")
      
      toGeoCode <- unique(fram$Location)
      tagText <- readLocs(read()$article,toGeoCode, 100)
      names(tagText) <- toGeoCode
      dfram <- reshape2::melt(tagText)
      
      
      geocode <- ggmap::geocode(toGeoCode,messaging = F)
      geocode$Location <- toGeoCode
      
      dfram$lat <- geocode$lat[match(dfram$L1,geocode$Location)]
      dfram$lon <- geocode$lon[match(dfram$L1,geocode$Location)]
      
      return(dfram)
      }
   
   out2 <- reactive({geocoder(out1())})
    
    output$LocationsText <- DT::renderDataTable({
    
                        
      DT::datatable(out2(), rownames = F)
      })
    
    
    output$map <- renderLeaflet({
      print('render map')
      leaflet(na.omit(out2())) %>% 
        addMarkers(~lon, ~lat,popup = ~value) %>%#addTiles() %>% 
        addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
        addProviderTiles("CartoDB.DarkMatter", group = "DarkMatter (CartoDB)") %>%
        addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
        addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
        addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap",'DarkMatter (CartoDB)', 'Esri.WorldImagery'),
                         options = layersControlOptions(collapsed = TRUE, autoZIndex = F))
    })

})




 
 
 
