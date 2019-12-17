


setwd("D:/Rproject/data minig task")

getwd()

library(dplyr)
library(ggplot2)
library(stringr)
library(udpipe)
library(lattice)

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

news=read.csv("news.csv", header = T)
news %>% group_by(publish_date) %>% count() %>% arrange(desc(n))


news_more <- news %>% mutate(year = str_sub(publish_date,1,4),
                             month = str_sub(publish_date,5,6),
                             date = str_sub(publish_date,7,8))
news_more %>% group_by(year) %>% count()  %>% ggplot() + geom_bar(aes(year,n), stat ='identity')

#install.packages("udpipe")

#udpipe_download_model(language = "english")
#model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(udpipe_download_model(language = "english"))
news_more_2008 <- news_more %>% filter(year == 2013 & month == 12)

s <- udpipe_annotate(udmodel_english, news_more_2008$headline_text)

x <- data.frame(s)

stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")



## NOUNS
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "Blue", 
         main = "Most occurring nouns", xlab = "Freq")

## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")

## Verb
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = brewer.pal(8,"Set1"), 
         main = "Most occurring Verbs", xlab = "Freq")

## Using RAKE
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")

## Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "magenta", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")

#wordcloud



#tex=read.csv("D:/Rproject/news.csv", header = T)
#text <- readLines("D:/Rproject/news.csv")
docs <- Corpus(VectorSource(news_more_2008$headline_text))
inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

tdm <- TermDocumentMatrix(docs)

inspect(tdm)
m <- as.matrix(tdm) 
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1000)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Set1"))


# frequency words

termFrequency <- rowSums(m)
termFrequency <- subset(termFrequency, termFrequency>=10)
findAssocs(tdm, terms = "pakistan", corlimit = 0.3)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col =brewer.pal(8, "Set1"), main ="Most frequent words",
        ylab = "Word frequencies")

