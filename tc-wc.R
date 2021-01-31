library("tm")
library("SnowballC")
library("textclean")
library("ggwordcloud")


crWordCloudDf <- function(inText) {

    corp <- Corpus(VectorSource(inText))

    corp <- tm_map(corp, content_transformer(tolower))
    corp <- tm_map(corp, removePunctuation)
    corp <- tm_map(corp, removeNumbers)
    corp <- tm_map(corp, removeWords, stopwords("english"))
    # the transcript has an oddity to remove after we have stripped punctuation 
    corp <- tm_map(corp, removeWords, c("weve"))


    tdm <- TermDocumentMatrix(corp)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m),decreasing=TRUE)
    df <- data.frame(word = names(v),freq=v)

    return(df)
}

text <- readLines("data.txt")
text <- replace_non_ascii(text)

ourDf <- crWordCloudDf(text)
tidyDf <- subset(ourDf, ourDf$freq > 3)

gw <- ggplot(data = tidyDf, 
        aes(label = word, size = freq, col = as.character(freq))) + 
        geom_text_wordcloud() +
        scale_size_area(max_size = 8) +
        theme_void() +
        ggtitle(" WordCloud: Tim Cook Speech\n Computers, Privacy & Data Protection 2021") +
        theme(  plot.title = element_text(size = 15, hjust=0.5, face = "bold", colour="navy"),
                plot.caption = element_text(size=7, colour="navy")) +
        labs(caption = "@fintanr      \nSource: https://sixcolors.com/post/2021/01/cook-companies-like-facebook-dont-deserve-praise-they-deserve-reform/      \n")

ggsave("tc-wordcloud.png", gw)