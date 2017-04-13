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


read <- eventReactive(input$dataset,{

# Read the pdf
 fulltext::ft_extract(input$dataset)


})

# Custom fucntion to index
#

#return( print(IndexText[which(matches[[i]] == TRUE)]))

Index <- function(read,verbatim, dictionary) {

  IndexText <- list()
  for(i in 1:length(verbatim$offsetend)){
    IndexText[i] <- stringr::str_sub(iconv(enc2utf8(read()$data),sub="byte"),
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

  output$data_table <- renderTable({
    #     Find scientific names
    scinames.d <- taxize::scrapenames(text = read()$data, all_data_sources = TRUE)

    sciname <- scinames.d$data$scientificname # output
    table(sciname,dnn = "Count")[order(table(sciname), decreasing = T)]

  })

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
      dictio <- read.csv(input$dictionary, header = TRUE, stringsAsFactors = F)
      scinames.d <- taxize::scrapenames(text = read()$data, all_data_sources = TRUE)
      verbatim <- scinames.d$data[c("verbatim","offsetend", "offsetstart")]
    table("Species" = unlist( Index(read()$data, verbatim,dictio)$text))

    })

    output$plot <- renderPlot({
      dictio <- read.csv(input$dictionary, header = TRUE)
      scinames.d <- taxize::scrapenames(text = read()$data, all_data_sources = TRUE)
      verbatim <-scinames.d$data[c("verbatim","offsetend", "offsetstart")]
      matches <- Index(read()$data, verbatim,dictio)$where


      plot(matches,
           xlab = "String match rank", ylab = "Article lenght",
           ylim = c(0,nchar(iconv(enc2utf8(read()$data),sub="byte"))),
           col = "#5ba966",pch = 16,
           main = "position on the text")
      legend("bottomright", "Position of record \n along the text",pch = 16, 
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
