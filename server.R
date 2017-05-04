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
  

read <- eventReactive(input$selector,{
  fulltext::ft_extract(input$selector)

})

read2 <- eventReactive(input$selector2,{
  fulltext::ft_extract(input$selector2)
  
})


scinames <- eventReactive(input$selector,{
  taxize::scrapenames(text = read()$data, all_data_sources = TRUE)})
scinames2 <- eventReactive(input$selector2,{
  taxize::scrapenames(text = read2()$data, all_data_sources = TRUE)})

dictio <- eventReactive(input$dictionary,{
  
  # Read the dictionary
  read.csv(input$dictionary, header = TRUE, stringsAsFactors = F)
})


# Custom fucntion to index
#

#return( print(IndexText[which(matches[[i]] == TRUE)]))

Index <- function(read,verbatim, dictionary) {

  IndexText <- list()
  for(i in 1:length(verbatim$offsetend)){
    IndexText[i] <- stringr::str_sub(iconv(enc2utf8(read),sub="byte"),
                                     verbatim$offsetstart[i]-150,
                                     verbatim$offsetend[i]+300)

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

# Custom function to retrieve dictionary matches

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



# Tab with Scientific Names found and count

  output$data_table <- DT::renderDataTable({
    
    #     Find scientific names
    #list <<- taxize::classification(scinames()$scientificname,db = "itis", verbose = F)
    sciname <- scinames()$data$scientificname 
    splist <- table(sciname,dnn = "Species")[order(table(sciname), decreasing = T)]
    splist <- data.frame(splist)
    splist$fam <- taxize::tax_name(splist$Species, get = "family", db ="ncbi", ask = F, verbose = F)$family
    #splist$order <- taxize::tax_name(splist$Species, get = "order", db ="ncbi", ask = F, verbose = F)$order
   # taxoi <- data.frame(table(verb$scientificname,dnn = "Species")[order(table(verb$scientificname), decreasing = T)])
   # list <- taxize::classification(taxoi$Species, db = "itis", verbose = T, rows= 1)
   #  
    
    

# 
#     dat <- data.frame(taxoi)
#     dat$fam <-taxize::tax_name(taxoi$Species, get = "family", db ="ncbi", ask = F, verbose = F)
#     dat$order <- taxize::tax_name(taxoi$Species, get = "order", db ="ncbi", ask = F, verbose = F)
#     DT::datatable(dat)
    DT::datatable(splist,rownames = F)

  })
  
  # output$tax_tree <- renderPlot({
  #   
  #   plot(taxize::class2tree(list))
  # })

#  
# names(list)
  

 
# Tab to show the dictionary
    output$dictionary <- renderPlot({
      # Read the dictionary
      dictio <- read.csv(input$dictionary, header = TRUE, stringsAsFactors = F)
      c <- termcount(dictio, read()$data)
      wordcloud::wordcloud(freq = c$count, words = c$term, 
                           max.words = 10, main = "Common terms from dictionary" )

    })

    # Tab to show the indexed results


    output$Indexed.version <- renderTable({
      verbatim <- scinames2()$data[c("verbatim","offsetend", "offsetstart")]
    table("Species" = unlist( Index(read2()$data, verbatim,dictio())$text))

    })

    output$plot <- renderPlot({

      verbatim <-scinames2()$data[c("verbatim","offsetend", "offsetstart")]
      matches <- Index(read2()$data, verbatim,dictio())$where


      plot(matches,
           xlab = "String match rank", ylab = "Article lenght",
           ylim = c(0,nchar(iconv(enc2utf8(read()$data),sub="byte"))),
           col = "#5ba966",pch = 16,
           main = "position on the text")
      legend("topright", "Position of record \n along the text",pch = 16, 
             col = "#5ba966", bty = "y" )
    })

})

# 
# #coord
# # 
# # out1 <- monkeylearn::monkeylearn_extract(texts$data, extractor_id = "ex_isnnZRbS", key=key)
# # out2 <- out1$entity[out1$tag == "LOCATION"]
# # 
# # coord <- dismo::geocode(out2)
# # leaflet(coord) %>%
# #   addMarkers(lng = coord$longitude, lat = coord$latitude)
# 
# 
# # texts <- fulltext::ft_extract(path1[1])
# # doct <- read.csv("dic/dictionary.csv", header = T, stringsAsFactors = F)
# # verb <-  taxize::scrapenames(text = texts$data, all_data_sources = TRUE)
# # verb <- verb$data
# # Index2(iconv(enc2utf8(texts$data)), verbatim = verb$data, dictionary = doct)
# # 
# # 
# # 
# # 
# # 
# # Index2 <- function(read,verbatim, dictionary) {
# # 
# #   IndexText <- list()
# #   for(i in 1:length(verbatim$offsetend)){
# #     IndexText[i] <- stringr::str_sub(iconv(enc2utf8(read),sub="byte"),
# #                                      verbatim$offsetstart[i]-150,
# #                                      verbatim$offsetend[i]+300)
# # 
# #   }
# #   matches <- c()
# #   ToMatch <- c()
# # 
# #   for ( i in 1:length(IndexText)){
# #     matches[[i]] <- grepl(paste(dictionary[,1], collapse = "|"),IndexText[i])
# #   }
# #   df <- c()
# #   df$text <- unlist(IndexText[matches==TRUE])
# #   df$where <- verbatim$offsetstart[which(matches == TRUE)]
# # 
# # 
# #   return(df)
# # }
# # 
# # 
# # 
# # ww <- stringr::str_sub(iconv(enc2utf8(texts$data),sub="byte"),
# #                        verb$offsetstart-150,
# #                        verb$offsetend+300)
# # 
# # wwe <- c("", "ripen", "noripen", "fika ret", "wood rail, , National, , Platalea ajaja, , Roseate spoonbill, , International, , Burhinus bistriatus, , Double-striped thick-knee, , International, , Thalasseus elegans, , Elegant tern, , International, , Ardea alba, , Great egret, , International, , Ardea herodias, , Great blue heron, , International, , Mesembrinbis cayennensis, , Green ibis, , International, , 4 BioScience <b7> XXXX XXXX / Vol. XX No. X, , http://bioscience.oxfordjournals.org, , Overview Artic")
# # match <- grepl(paste(doct[,1], collapse = "|"),wwe)
# 
# 
# 
# 
# 
