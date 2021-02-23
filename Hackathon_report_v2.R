
#Install all the packages

if(!require("NLP")) install.packages("NLP");
if(!require("translateR")) install.packages("translateR");

##Libraries installed (if not present , then please use install.packages(<library>))
library(NLP)
library(tokenizers)
library(textstem)
library("tm")
library(tm)
library(wordcloud)
library(RColorBrewer)
library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(reshape2)
library(tidyverse)
library(quanteda)
library(stopwords)
library(topicmodels)
library(dplyr)
library(translateR)
library(sqldf)


# getGoogleLanguages()
# getMicrosoftLanguages()

# setwd("~/Desktop/Germany/Education/Sem_1/Text_Mining/Hackathon/")

### Loading the hotels_hackathon rda

load("~/Desktop/Germany/Education/Sem_1/Text_Mining/Hackathon/hotels_hackathon.rda")
View(Hotel_reviews)
class(Hotel_reviews)
nrow(Hotel_reviews)
#1399 records are there

## SAVE THE REVIEWTEXT-COLUMN FROM THE DATA-FRAME AS A CHARACTER-VECTOR AND CONVERT TO STRING
## String conversion is required for tokenisation

reviews_hotel_cleaned <- Hotel_reviews$reviews.text
View(reviews_hotel_cleaned)
typeof(reviews_hotel_cleaned)
nrow(reviews_hotel_cleaned)

reviews_hotel_cleaned <- as.data.frame(reviews_hotel_cleaned)
nrow(reviews_hotel_cleaned)

class(reviews_hotel_cleaned)


#Adding title also as one of the columns

reviews_hotel_cleaned$title <- Hotel_reviews$reviews.title
colnames(reviews_hotel_cleaned)[1] <- "Description"

#merging both title and text into single column

reviews_hotel_cleaned$mergedTextColumn <- paste(reviews_hotel_cleaned$Description, reviews_hotel_cleaned$title)

# R loop to check if the language is english or not

# creating a new column in reviews_hotel_Cleaned

View(reviews_hotel_cleaned)

#Below library will help us to identify the language used in the dataset

library("textcat")

#Below will help us to get the col names
colnames(reviews_hotel_cleaned)

# creating column names identical to first column
reviews_hotel_cleaned$LanguageUsed_before <- NA
reviews_hotel_cleaned$LanguageUsed_before <- as.character(reviews_hotel_cleaned$LanguageUsed)
reviews_hotel_cleaned$LanguageUsed_before <- as.character(reviews_hotel_cleaned$LanguageUsed)

# reviews_hotel_cleaned$reviews_hotel_cleaned <- as.character(reviews_hotel_cleaned$reviews_hotel_cleaned)


#reviews_hotel_cleaned$TextsPresent <- as.character(reviews_hotel_cleaned$reviews_hotel_cleaned)
# reviews_hotel_cleaned$ChangedLanguage <- NA
# reviews_hotel_cleaned$ChangedText <- NA

# in order to identify the different type of data types in side data frame,
# below command is used

sapply(reviews_hotel_cleaned, class)
nrow(reviews_hotel_cleaned)


## Below will classify the text based on language
for (row in 1:nrow(reviews_hotel_cleaned)) {
  reviews_hotel_cleaned$LanguageUsed_before <-  textcat(c(
    reviews_hotel_cleaned$mergedTextColumn))
}

summary(reviews_hotel_cleaned)


#before language change

#####
#Using SQL package to identify the type of text present

install.packages("sqldf")
library(sqldf)


countandLanguage <- sqldf("select count(LanguageUsed_before) as count, LanguageUsed_before from reviews_hotel_cleaned group by LanguageUsed_before order by count desc")

countandLanguage_temp <- sqldf("select mergedTextColumn, LanguageUsed_before from reviews_hotel_cleaned where LanguageUsed_before!= 'english' and LanguageUsed_before!= 'scots'")

view(countandLanguage_temp)
countandLanguage_temp$temp <- NA

class(countandLanguage_temp)
typeof(countandLanguage_temp)
nrow(countandLanguage_temp)


######## viewing and counting the language used

View(countandLanguage)

library(tidyverse)
library(ggplot2)

ggplot(data = countandLanguage, mapping = aes(x = count, y = LanguageUsed_before)) +
  geom_boxplot(alpha = 10) +
  geom_jitter(alpha = 10, color = "tomato")

#
#
# #changing the language - trial 1
#
# reviews_hotel_cleaned$mergedTextColumn[2] <- translate(text = reviews_hotel_cleaned$mergedTextColumn[2], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "de")
# reviews_hotel_cleaned$mergedTextColumn[3] <- translate(text = reviews_hotel_cleaned$mergedTextColumn[3], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "fr")
#
#


# #####testing the sample change of data
# translate(text = reviews_hotel_cleaned$TextsPresent[2], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "de")

##trying to get the google languages into the table
## capture.output will direct the console output into a character vector
results_test1 <- capture.output(getGoogleLanguages())
typeof(results_test1)
class(results_test1)

results_test1 <- as.data.frame(results_test1)

#below library will help us to split the text file in rows into columns
library(splitstackshape)

#below command cSplit will help us to split the contents from row to columns
LangaugeandCode <- cSplit(results_test1, 'results_test1', sep=", ", type.convert=FALSE)
View(LangaugeandCode)

#removing first two unnecessary columns (note its repeated twice as index will
# be same after removing)

LangaugeandCode <- LangaugeandCode[-c(1),]
LangaugeandCode <- LangaugeandCode[-c(1),]

#renaming the table to be more meaningful
colnames(LangaugeandCode)[1] <- "Language"
colnames(LangaugeandCode)[2] <- "code"
View(LangaugeandCode)



## creating custom function to use for translating language
library(translateR)

#translate is a package in the google dictonary

translate <- function(text,
                      API_Key,
                      target = "en",
                      source = "de") {
  b <- paste0(
    '{
    "q": [
    "',
    text,
    '"
    ],
    "target": "',
    target,
    '",
    "source": "',
    source,
    '",
    "format": "text"
}'
  )
  url <-
    paste0("https://translation.googleapis.com/language/translate/v2?key=",
           API_Key)
  x <- httr::POST(url, body = b)
  x <- jsonlite::fromJSON(rawToChar(x$content))
  x <- x$data$translations
  return(x$translatedText[1])
}

## converting text as per the most popluar languages used
##converting text data to Spanish

df_total<- NA
df_total <- data.frame()




#####
###trying to convert language to new language

rm(reviews_hotel_LanguageChange)
reviews_hotel_LanguageChange <- cbind(reviews_hotel_cleaned$mergedTextColumn, reviews_hotel_cleaned$LanguageUsed)

class(reviews_hotel_LanguageChange)
reviews_hotel_LanguageChange <- as.data.frame(reviews_hotel_LanguageChange)
colnames(reviews_hotel_LanguageChange)[1] <- "mergedTextColumn"
colnames(reviews_hotel_LanguageChange)[2] <- "LanguageUsed"

reviews_hotel_LanguageChange$newLanguage <- NA

reviews_hotel_LanguageChange$newLanguage <- as.character.factor(reviews_hotel_LanguageChange$newLanguage)

nrow(reviews_hotel_LanguageChange)


for (i in 1:nrow(reviews_hotel_LanguageChange)) {
  if(reviews_hotel_LanguageChange$LanguageUsed[i] == "german") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "de")
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "spanish") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "es")
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "french") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "fr")
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "swedish") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "sv")
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "czech-iso8859_2") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "cs")
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "slovenian-iso8859_2") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "fr")
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "serbian-ascii") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "sl")
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "catalan") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "ca")
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "slovak-ascii") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "sk")
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "portuguese") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "pt")
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "dutch") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "nl")
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "danish") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "da")
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "afrikaans") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "af")
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "swedish") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "sv")
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "catalan") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "ca")
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "italian") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "it")
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "welsh") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "cy")
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "romanian") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "ro")
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "norwegian") {
    reviews_hotel_LanguageChange$newLanguage[i] <- translate(text = reviews_hotel_LanguageChange$mergedTextColumn[i], API_Key = 'AIzaSyDdz21DBttX9BIALL6NdIS97cu6PH6M2mE', target = "en", source = "no")
  }else {
    reviews_hotel_LanguageChange$newLanguage[i] <- 1
  }

}

#changeing the already changed value to 0 so that it can help us merge

reviews_hotel_LanguageChange$temp <- reviews_hotel_LanguageChange$mergedTextColumn

reviews_hotel_LanguageChange$temp <- as.character(reviews_hotel_LanguageChange$temp)

for (i in 1:nrow(reviews_hotel_LanguageChange)) {
  if(reviews_hotel_LanguageChange$LanguageUsed[i] == "german") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "spanish") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "french") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "swedish") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "czech-iso8859_2") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "slovenian-iso8859_2") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "serbian-ascii") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "catalan") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "slovak-ascii") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }
  else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "portuguese") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "dutch") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "danish") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "afrikaans") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "swedish") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "catalan") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "italian") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "welsh") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "romanian") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }else if(reviews_hotel_LanguageChange$LanguageUsed[i] == "norwegian") {
    reviews_hotel_LanguageChange$temp[i] <- 1
  }

}

reviews_hotel_LanguageChange$mergeChangedLanguage <- NA


reviews_hotel_LanguageChange$mergeChangedLanguage <- paste(reviews_hotel_LanguageChange$newLanguage, reviews_hotel_LanguageChange$temp)


## # pasting the converted language(mergeChangedLanguage) column back to original column
reviews_hotel_cleaned$mergedTextColumn <- NA
reviews_hotel_cleaned$mergedTextColumn <- reviews_hotel_LanguageChange$mergeChangedLanguage


##############Language check again #########

## Classify the text based on language again after doing language change


#Below library will help us to identify the language used in the dataset
library("textcat")

#Below will help us to get the col names
colnames(reviews_hotel_cleaned)

# creating column names identical to first column
reviews_hotel_cleaned$LanguageUsed_afterchange <- as.character(reviews_hotel_cleaned$LanguageUsed)

## Classify the text based on language again after doing language change

for (row in 1:nrow(reviews_hotel_cleaned)) {
  reviews_hotel_cleaned$LanguageUsed_afterchange <-  textcat(c(
    reviews_hotel_cleaned$mergedTextColumn))
}

summary(reviews_hotel_cleaned)

#####
#Using SQL package to identify the type of text present
install.packages("sqldf")
library(sqldf)
sqldf("select count(LanguageUsed_afterchange) as count, LanguageUsed_afterchange from reviews_hotel_cleaned group by LanguageUsed_afterchange order by count desc")
countandLanguage <- sqldf("select count(LanguageUsed_afterchange) as count, LanguageUsed_afterchange from reviews_hotel_cleaned group by LanguageUsed_afterchange order by count desc")

countandLanguage_temp <- sqldf("select mergedTextColumn, LanguageUsed_afterchange from reviews_hotel_cleaned where LanguageUsed_afterchange!= 'english' and LanguageUsed_afterchange!= 'scots'")

view(countandLanguage_temp)
countandLanguage_temp$temp <- NA

class(countandLanguage_temp)
typeof(countandLanguage_temp)
nrow(countandLanguage_temp)

##App
#Combining both columns
Hotel_reviews$cleaned <- NA
# Hotel_reviews$cleaned <- paste(reviews_hotel_cleaned)

library("NLP")

#reviews_hotel_text <- reviews_hotel_cleaned$reviews_hotel_cleaned
rm(reviews_hotel_text)
reviews_hotel_text <- reviews_hotel_cleaned$mergedTextColumn
reviews_hotel_text<-as.String(reviews_hotel_text)

typeof(reviews_hotel_text)

### TASK 2: TOKENIZATION
# USE THE "TOKENIZERS"-PACKAGE. TOKENIZE YOUR TEXT INTO
# (1) SENTENCES
# (2) SINGLE WORDS
# (3) STEMM THE SINGLE WORDS.

library(tokenizers)

# FUNCTION NEEDS STRING-VARIABLE: reviews_hotel_text
# (1) TOKENIZE SENTENCES

View(reviews_hotel_text)
reviews_hotel_sentences <- tokenize_sentences(reviews_hotel_text)
typeof(reviews_hotel_sentences)
View(reviews_hotel_sentences)

# (2) TOKENIZE WORDS
reviews_hotel_words <- tokenize_words(reviews_hotel_text)
View(reviews_hotel_words)
typeof(reviews_hotel_words)

# (3) STEMMING
reviews_hotel_stemmed <- tokenize_word_stems(reviews_hotel_text)
View(reviews_hotel_stemmed)
typeof(reviews_hotel_stemmed)
reviews_hotel_stemmed_char <- as.character(reviews_hotel_stemmed)
View(reviews_hotel_stemmed_char)


##  TASK 3: LEMMATIZATION
library(textstem)

# FUNCTION NEEDS A CHARACTER VECTOR, NO STRING!

reviews_hotel_text_vector <- reviews_hotel_cleaned$mergedTextColumn
reviews_hotel_lemma <- lemmatize_strings(reviews_hotel_text_vector)
# reviews_hotel_lemma <- lemmatize_strings(reviews_hotel_stemmed_char)

View(reviews_hotel_lemma)

### TASK 4: COMPARE STEMMING AND LEMMATIZATION.
##NA

### TASK 5: LANGUAGE CLEANING
# PERFORM LANGUAGE CLEANING:
# (1) SET TO LOWER CASE,
# (2) REMOVE NUMBERS,
# (3) REMOVE PUNCTUATION,
# (4) STRIP WHITESPACE,
# (5) REMOVE STOPWORDS.
# FIND, INSTALL AND USE R-PACKAGES ON YOUR OWN. CONVERT DATA STRUCTURES IF NEEDED. PROVIDE A CLEANED TEXT.


library("tm")
View(reviews_hotel_lemma)
reviews_hotel_text_cleaned <- tolower(reviews_hotel_lemma)

reviews_text_cleaned <- removeNumbers(reviews_hotel_lemma)
View(reviews_hotel_text_cleaned)

reviews_text_cleaned <- removePunctuation(reviews_hotel_lemma)
reviews_text_cleaned <- stripWhitespace(reviews_hotel_lemma)
View(reviews_hotel_text_cleaned)
Hotel_reviews$cleaned <- reviews_hotel_text_cleaned

# (5) REMOVE STOPWORDS
# CONVERT TO CORPUS, BECAUSE FUNCTION NEED CORPUS AS INPUT
reviews_hotel_corpus <- Corpus(VectorSource(reviews_hotel_text_cleaned))
typeof(reviews_hotel_corpus)
class(reviews_hotel_corpus)

reviews_hotel_corpus <- tm_map(reviews_hotel_corpus, removeWords, stopwords("english"))
# reviews_hotel_corpus <- tm_map(reviews_hotel_corpus, removeWords, stopwords("german"))
# reviews_hotel_corpus <- tm_map(reviews_hotel_corpus, removeWords, stopwords("spanish"))
# reviews_hotel_corpus <- tm_map(reviews_hotel_corpus, removeWords, stopwords("french"))

View(reviews_hotel_corpus)

# CONVERT BACK TO CHARACTER-VECTOR
reviews_hotel_text_cleaned <- unlist(reviews_hotel_corpus$content)
View(reviews_hotel_text_cleaned)


# STRIP WHITESPACE AGAIN, NECESSARY AFTER STOPWORD-REMOVAL
reviews_hotel_text_cleaned <- stripWhitespace(reviews_hotel_text_cleaned)
reviews_hotel_text_cleaned <- removeNumbers(reviews_hotel_text_cleaned)
reviews_hotel_text_cleaned <- removePunctuation(reviews_hotel_text_cleaned)
reviews_hotel_text_cleaned <- stripWhitespace(reviews_hotel_text_cleaned)


View(reviews_hotel_text_cleaned)
class(reviews_hotel_text_cleaned)

Hotel_reviews$cleaned <- reviews_hotel_text_cleaned
Hotel_reviews$rating <- NA
View(Hotel_reviews)

library(textclean)

### doing the word cloud
# AFTER PRE-PROCESSING A WORD CLOUD BECOMES MORE FUNCTIONAL:
install.packages ("tm", "wordcloud", "RColorBrewer")
library(tm)
library(wordcloud)
library(RColorBrewer)

# BEFORE PRE-PROCESSING
reviews_hotel_text_vector <- Hotel_reviews$reviews.text
reviews_hotel_corpus <- Corpus(VectorSource(reviews_hotel_text_vector))
tdm_review <- TermDocumentMatrix(reviews_hotel_corpus)
m <- as.matrix(tdm_review)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(d$word,d$freq, scale=c(4,0.4), random.order=FALSE, rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

# AFTER PRE-PROCESSING
View(reviews_hotel_text_cleaned)
reviews_hotel_corpus <- Corpus(VectorSource(reviews_hotel_text_cleaned))
tdm_review <- TermDocumentMatrix(reviews_hotel_corpus)
m <- as.matrix(tdm_review)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
# wordcloud(d$word,d$freq, scale=c(5,0.5), random.order=FALSE, rot.per=0.1, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(d$word,d$freq, scale=c(4,0.4), random.order=FALSE, rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))


################## Second attempt to capture frequent used words ###########

install.packages("qdap")
install.packages("dplyr")
install.packages("tm")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("quanteda")
library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(reshape2)
library(quanteda)


View(Hotel_reviews)
class(Hotel_reviews)

## Make a vector source and a corpus

reviews_hotel_cleaned$cleanedColumn <- NA
reviews_hotel_cleaned$cleanedColumn <- Hotel_reviews$cleaned
View(reviews_hotel_cleaned)

#reviews_hotel_cleaned$mergedTextColumn

corpus_review=Corpus(VectorSource(reviews_hotel_cleaned$cleanedColumn))
View(corpus_review)

##preprocessing
#Convert to lower case

corpus_review=tm_map(corpus_review, tolower)

#Remove punctuation
corpus_review=tm_map(corpus_review, removePunctuation)

#Remove stopwords
corpus_review=tm_map(corpus_review, removeWords, stopwords("english"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("spanish"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("german"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("dutch"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("japanese"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("chinese"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("french"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("welsh"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("swedish"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("italian"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("portuguese"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("danish"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("afrikaans"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("swedish"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("catalan"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("romanian"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("norwegian"))


# Remove context specific stop words
corpus_review=tm_map(corpus_review, removeWords,c("one", "i", "1"))

## Stem document
corpus_review=tm_map(corpus_review, stemDocument)

##Viewing the corpus content
corpus_review[[8]][1]
class(corpus_review)

##
# CONVERT corpus back TO CHARACTER-VECTOR
Word_review_vector <- unlist(corpus_review)
View(Word_review_vector)


# Find the 20 most frequent terms: term_count
term_count <- freq_terms(corpus_review, 20)
class(term_count)

# Plot 20 most frequent terms
plot(term_count)

# Create the DTM & TDM from the corpus
review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review)
View(review_dtm)
View(review_tdm)
class(review_tdm)
## Using the TDM to identify frequent terms

# Convert TDM to matrix
review_m <- as.matrix(review_tdm)
# Sum rows and frequency data frame
review_term_freq <- rowSums(review_m)
# Sort term_frequency in descending order
review_term_freq <- sort(review_term_freq, decreasing = T)
# View the top 10 most common words
review_term_freq[1:10]

review_term_freq <- as.data.frame(review_term_freq)
class(review_term_freq)


#most commonly used term frequencies
View(review_term_freq)

class(review_term_freq)

# Plot a barchart of the 20 most common words
barplot(review_term_freq[1:10], col = "steel blue", las = 0.4)


#word clouds
review_word_freq <- data.frame(term = names(review_term_freq),
                               num = review_term_freq)

# # Create a wordcloud for the values in word_freqs
# wordcloud(review_word_freq$term, review_word_freq$num,
#           max.words = 60, colors = "red")
#
#
# # Print the word cloud with the specified colors
# wordcloud(review_word_freq$term, review_word_freq$num,
#           max.words = 60, colors = c("aquamarine","darkgoldenrod","tomato"))

################## Third attempt going through LDA approach for clustering which involves creating
################## TDM, DFM and DTM ###########

library(tidyverse)
library(quanteda)

### TASK 2: LOAD THE "hotels_hackathon".
typeof(Hotel_reviews)
class(Hotel_reviews)

##below is of no use
# CREATE VERFIFIED-SUBSET
reviews_verified <- subset(Hotel_reviews, is.null(Hotel_reviews$cleaned)==F)
View(reviews_verified)

##the above is of no use

#Word_review_vector

# INSTALL PACKAGES
install.packages("quanteda")
install.packages("stopwords")

# LOAD PACKAGES
library(quanteda)
library(stopwords)

# CREATE CORPUS FROM SAMPLE-REVIEW-TEXTS
View(corpus_review)
class(corpus_review)

# CREATE EXTENDED STOPWORD-LIST
# extended_stopwords <- c("fitbit", "charge", "love", "one", "like", "get", "use", "can", unique(reviews_sample$product))
#term_count <- freq_terms(corpus_review, 20)

## Original one
##extended_stopwords <- c(term_count, unique(Hotel_reviews$name))

##Modified on 29th Feb
extended_stopwords <- c(term_count, "and", "the", unique(Hotel_reviews$name))
view(extended_stopwords)
class(extended_stopwords)

# # CREATE DOCUMENT-FEATURE-MATRIX
# myDfm <- dfm(corpus_review,
#              tolower = TRUE,
#              stem = TRUE,
#              remove_numbers = TRUE,
#              remove_punct = TRUE,
#              remove_symbols = TRUE,
#              remove = c(stopwords("english"), extended_stopwords))


### TASK 6: INSTALL AND LOAD THE "topicmodels"-package. USE IT TO CREATE A LDA-MODEL. DELIBERATE A SUITING NUMBER OF TOPICS.
#           SET THE SEED TO "2019".

# INSTALL PACKAGES
install.packages("topicmodels")

# LOAD PACKAGES
library(topicmodels)

# Create the DTM & TDM from the corpus
review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review) ##have used this

view(review_tdm)

#trying to create feature matrix
mycorpus <- corpus(Hotel_reviews$cleaned)
# extended_stopwords_new <- c("via","the","and","very","back", "from", "to" , unique(Hotel_reviews$name))


myDfm <- dfm(mycorpus,
             tolower = TRUE,
             stem = TRUE,
             remove_numbers = TRUE,
             remove_punct = TRUE,
             remove_symbols = TRUE,
             remove = c(stopwords("english"), extended_stopwords))

view(myDfm)

## LDA cannot be done as we are not using unsupervised algoritm and there is no need to train the data
#
# # CREATE A LDA-MODEL - using tdm
# reviews_lda <- LDA(review_tdm, k = 5, control = list(seed = 2019))
#
# # CREATE A LDA-MODEL - using tdm
# reviews_lda <- LDA(myDfm, k = 5, control = list(seed = 2019))
#
# view(reviews_lda)
#
# ### TASK 7: INSTALL AND LOAD THE "tidytext"-package. ADJUST AND USE THE FOLLOWING CODE TO VISUALIZE THE RESULTS.
# #           INSTALL FURTHER LIBRARIES, IF NEEDED.
#
# # INSTALL PACKAGES
# install.packages("tidytext")
#
# # LOAD PACKAGES
# library(tidytext)
#
# view(reviews_lda)
#
# # CONVERT INTO TIDY-FORMAT TO VISUALIZE RESULTS
# reviews_lda_td <- tidy(reviews_lda@documents)
# reviews_lda_td_names <- tidy_names(reviews_lda_td)
# view(reviews_lda_td_names)
#
# # EXTRACT 10 TOP-TERMS PER TOPIC
# top_terms <- reviews_lda_td %>%
#   group_by(topic) %>%
#   top_n(5, beta) %>%
#   ungroup() %>%
#   arrange(topic, -beta)
#
# # VISUALIZE THE TOP-TERMS AND THEIR LOADINGS
# top_terms %>%
#   mutate(term = reorder(term, beta)) %>%
#   ggplot(aes(term, beta, fill = factor(topic))) +
#   geom_bar(alpha = 0.8, stat = "identity", show.legend = T) +
#   facet_wrap(~ topic, scales = "free") +
#   coord_flip()



################## Third attempt going thorugh sentiment analysis ###########

# LAOD PACKAGES
library(tidyverse)
library(quanteda)


### TASK 3: CREATE A SUBSET OF THE REVIEWS WITH ONLY .
###  CHOOSE ONE PRODUCT MODEL WITH AT LEAST 1500 REVIEWS AND
#           CALCULATE THE AMOUNT OF WORDS PER REVIEW. KEEP ONLY THOSE WITH > 100 WORDS.

View(Hotel_reviews)

# CREATE VERFIFIED-SUBSET (Actually not required)
# hotel_reviews_verified <- subset(Hotel_reviews, Hotel_reviews$country == "US")
# View(hotel_reviews_verified)
# typeof(hotel_reviews_verified)
# class(hotel_reviews_verified)

# CREATE NEW COLUMN FOR TEXT LENGTH
# hotel_reviews_verified$text_length <- NA

library(dplyr)

Hotel_reviews <- as.data.frame(Hotel_reviews)
mycorpus <- corpus(Hotel_reviews$cleaned)


#reviews_hotel_corpus <- Corpus(VectorSource(mycorpus))
typeof(reviews_hotel_corpus)
class(reviews_hotel_corpus)
View(reviews_hotel_text_cleaned)

#
# # CREATE EXTENDED STOPWORD-LIST
# extended_stopwords <- c("fitbit", "charge", "love", "one", "like", "get", "use", "can", unique(reviews_hotel_corpus$))
#
#

dict <- read.csv2("/Users/Vinudeep/Desktop/Germany/Education/Sem_1/Text_Mining/Others/custom_dictionary.csv", stringsAsFactors = F)

# EXTRACT TERMS AND STRIP EMPTY VECTOR ELEMENTS
dict_positive <- dict$positive
dict_positive <- dict_positive[dict_positive != ""]
dict_negative <- dict$negative
mydict <- dictionary(list(positive = dict_positive, negative = dict_negative))
class(mydict)

# CREATE DOCUMENT-FEATURE-MATRIX WITH CUSTOM-DICTIONARY TERMS
myDfm <- dfm(mycorpus, dictionary = mydict)
View(myDfm)


# CREATE DATA-FRAME WITH TEXTS AND DICTIONARY-VALUES

df_sentiment <- cbind(Hotel_reviews$cleaned, convert(myDfm, to = "data.frame"))
view(df_sentiment)

### TASK 4: BASED ON THE TERM FREQUENCY, LABEL THE DOCUMENTS AUTOMATICALLY. CREATE A BARPLOT OF THE LABELS.

# LABEL  DOCUMENTS
df_sentiment$tflabel <- NA

View(df_sentiment)

for (i in 1:nrow(df_sentiment)) {
  if (df_sentiment$positive[i] > df_sentiment$negative[i]) {
    df_sentiment$tflabel[i] <- "positive"
  }

  if (df_sentiment$negative[i] > df_sentiment$positive[i]) {
    df_sentiment$tflabel[i] <- "negative"
  }

  if (df_sentiment$positive[i] == df_sentiment$negative[i]) {
    df_sentiment$tflabel[i] <- "neutral"
  }
}

View(df_sentiment)


#barplot(table(df_sentiment$tflabel))

### TASK 5: ANALYSE THE reviews WITH YOUR DICTIONARY
### (TF-IDF). BASED ON THE TF-IDF, LABEL THE DOCUMENTS AUTOMATICALLY.

View(myDfm)


# CREATE TF-IDF MATRIX
myDfm_tfidf <- dfm_tfidf(myDfm, scheme_tf = "count", scheme_df = "inverse")

myDfm_tfidf_matrix <- as.matrix(myDfm_tfidf)
View(myDfm_tfidf_matrix)

df_sentiment <- cbind(df_sentiment, myDfm_tfidf_matrix)
View(df_sentiment)



#renaming the column names so that we can easily identiy
colnames(df_sentiment)[6] <- "tf_idf_positive"
colnames(df_sentiment)[7] <- "tf_idf_negative"



#Creating a new label
df_sentiment$label_tf_idf <- NA

for (i in 1:nrow(df_sentiment)) {
  if (df_sentiment[i,6] > df_sentiment[i,7]) {
    df_sentiment$label_tf_idf[i] <- "positive"
  }

  if (df_sentiment[i,6] < df_sentiment[i,7]) {
    df_sentiment$label_tf_idf[i] <- "negative"
  }

  if (df_sentiment[i,6] == df_sentiment[i,7]) {
    df_sentiment$label_tf_idf[i] <- "neutral"
  }
}

View(df_sentiment)


### TASK 6: COMPARE TEXTS WITH DIFFERENT LABELS. EXTRACT THEM TO A NEW DATA-FRAME.

# CREATE NEW COMPARISON-COLUMN
df_sentiment$comparison <- NA

# COMPARE LABELS
for (i in 1:nrow(df_sentiment)) {
  if(df_sentiment$tflabel[i] == df_sentiment$label_tf_idf[i]) {
    df_sentiment$comparison[i] <- "same"
  } else {
    df_sentiment$comparison[i] <- "different"
  }
}

View(df_sentiment)

#Hotel_reviews$cleaned <- df_sentiment$`Hotel_reviews$cleaned`

Hotel_reviews <- cbind(Hotel_reviews, df_sentiment)
view(Hotel_reviews)

#dropping unwanted column

Hotel_reviews$`Hotel_reviews$cleaned` <- NULL

#adding two more columns
Hotel_reviews$avgReview <- NA
Hotel_reviews$tmpNumber <- NA

#assigning positive 1, negative -1 and neutral - 0

#unique(Hotel_reviews$name)

sapply(Hotel_reviews, class)

for (i in 1:nrow(Hotel_reviews)) {
  if(Hotel_reviews$label_tf_idf[i] == "positive") {
    Hotel_reviews$tmpNumber[i] <- 1
  }

  if(Hotel_reviews$label_tf_idf[i] == "negative") {
    Hotel_reviews$tmpNumber[i] <- -1
  }

  if(Hotel_reviews$label_tf_idf[i] == "neutral") {
    Hotel_reviews$tmpNumber[i] <- 0
  }
}

########## Till here intermediatory results ############

tmp_recommendation_table <- unique(Hotel_reviews$name)
View(tmp_recommendation_table)

library(dplyr)
# Hotel_reviews %>%
#   group_by(Hotel_reviews$name) %>%
#   summarise(Hotel_reviews$tmpNumber == sum(Hotel_reviews$tmpNumber))

#aggregating best hotel based on reviews

tmp_table <- aggregate(Hotel_reviews$tmpNumber, by=list(name=Hotel_reviews$name),FUN=sum)
view(tmp_table)

#ranking as per the reviews of tf-idf
#------ this gives the best ranked hotels based on reviews but not precise

ranked_hotels <- tmp_table[order(-tmp_table$x),]
view(ranked_hotels)

max(tmp_table$x)
min(tmp_table$x)

#renaming tmpno columns
# colnames(Hotel_reviews)[20] <- "number_basedon_label_tf_idf"
# Hotel_reviews$number_basedon_label_tf_idf <- NULL

#creating temp columns
Hotel_reviews$tmpNumber <- NA

#Combining uo tfidf positive and negative
for (i in 1:nrow(Hotel_reviews)) {
  Hotel_reviews$tmpNumber[i] = Hotel_reviews$tf_idf_positive[i]-Hotel_reviews$tf_idf_negative[i]
}

max(Hotel_reviews$tmpNumber)
min(Hotel_reviews$tmpNumber)


###Calculate following key figures of the reviewRatings.

# (1) sum
sum(Hotel_reviews$tmpNumber)
# (2) mean
mean(Hotel_reviews$tmpNumber)
# (3) median
median(Hotel_reviews$tmpNumber)
# (4) variance
var(Hotel_reviews$tmpNumber)
# (5) standard deviation
sd(Hotel_reviews$tmpNumber)

range(Hotel_reviews$tmpNumber)

##Getting a range

tmpTable_ranking <- split(Hotel_reviews$tmpNumber, sample(1:5, nrow(Hotel_reviews), replace=T))
View(tmpTable_ranking)

tmp_ord <- Hotel_reviews[order(Hotel_reviews$tmpNumber),]
tmpTable_ranking <- split(tmp_ord$tmpNumber, sample(1:5, nrow(tmp_ord), replace=T))

View(tmpTable_ranking)

tmp_ord$rank <- NA
tmp_ord$rank2 <- as.factor( cut(tmp_ord$tmpNumber,5, labels=F))

#renaming tmp_ord dataframe to look more meaninigful
Review_ranking_hotel <- tmp_ord

# cleaning the table Review_ranking_hotel
Review_ranking_hotel$avgReview <- NULL
Review_ranking_hotel$rank <- NULL
Review_ranking_hotel$rating <- NULL

colnames(Review_ranking_hotel)[20] <- "rating"
colnames(Review_ranking_hotel)[19] <- "deg_of_sentiment"

sapply(Review_ranking_hotel, class)


library(sqldf)
top20Hotels <- NA
view(top20Hotels)


### TASK 46: Export reviews as a .csv-file - Review_ranking_hotel
write.csv2(Review_ranking_hotel, "~/Desktop/Germany/Education/Sem_1/Text_Mining/Hackathon/Review_ranking_hotel.csv")

### TASK 46: Export reviews as a .csv-file - ranked_hotels
write.csv2(ranked_hotels, "~/Desktop/Germany/Education/Sem_1/Text_Mining/Hackathon/ranked_hotels.csv")

### TASK 46: Export reviews as a .csv-file - Original hotel reviews
write.csv2(Hotel_reviews, "~/Desktop/Germany/Education/Sem_1/Text_Mining/Hackathon/Hotel_reviews_backup.csv")

### Visualisation ######
reviews_imported <- read.csv2("~/Desktop/Germany/Education/Sem_1/Text_Mining/Hackathon/Review_ranking_hotel.csv")
class(reviews_imported)

# rankings
barplot(table(reviews_imported$rating),
        col = c("#FF3333", "#FF9933", "#FFFF99", "#CCFF00", "#006600"),
        main = "Rating and Ranking against the number of hotels",
        xlab = "Ranking",
        ylab = "Amount of hotels")

# City most rated
barplot(table(reviews_imported$city),
        col = c("#FF3333", "#FF9933", "#FFFF99", "#CCFF00", "#006600"),
        main = "Amount of Reviews per city",
        xlab = "City",
        ylab = "Reviews")

# province
barplot(table(reviews_imported$province),
        col = c("#FF3333", "#FF9933", "#FFFF99", "#CCFF00", "#006600"),
        main = "Amount of Reviews per Province",
        xlab = "province",
        ylab = "Amount of Ratings")

## Checking the number of reviews and hotel

library(data.table)
DT <- data.table(reviews_imported)
view(DT)
DT[, .(No_of_reviews_per_hotel = length(unique(cleaned))), by = name]
class(DT[, .(No_of_reviews_per_hotel = length(unique(cleaned))), by = name])

Hotel_name_Review_count <- DT[, .(No_of_reviews_per_hotel = length(unique(cleaned))), by = name]
View(Hotel_name_Review_count)


## Prerequisite - no of reviews per hotel

#creating vector for name and reviews
name_hotel <- as.vector(Hotel_name_Review_count$name)
typeof(name_hotel)
noofreviews <- as.vector((Hotel_name_Review_count$No_of_reviews_per_hotel))
view(noofreviews)

# barchart with added parameters

summary(Review_ranking_hotel$name)


#barchart of mapping the number of hotels and reviews
barplot(table(Hotel_name_Review_count$name, Hotel_name_Review_count$No_of_reviews_per_hotel),
        col = c( "#FF9933"),
        main = "Map of number of reviews vs hotels",
        xlab = "Reviews",
        ylab = "Hotel Numbers",
        horiz = T)

#Selecting the most reviewed hotels

library(dplyr)
topHotel <- Hotel_name_Review_count %>% top_n(20)
#
# #barchart of mapping the number of hotels and reviews
# barplot(table(topHotel$name, topHotel$No_of_reviews_per_hotel),
#         col = c( "#FF9933"),
#         main = "Top reviewed hotels",
#         xlab = topHotel$No_of_reviews_per_hotel,
#         ylab = topHotel$name,
#         horiz = F)
#

library(ggplot2)

ggplot(data = topHotel, mapping = aes(x = No_of_reviews_per_hotel, y = name)) +
  geom_boxplot(alpha = 0.1) +
  geom_jitter(alpha = 10, color = "blue")


barplot(table(reviews_imported$rating),
        col = c("#FF3333", "#FF9933", "#FFFF99", "#CCFF00", "#006600"),
        main = "Rating and hotels count",
        xlab = "Hotels",
        ylab = "Ratings",
        horiz = TRUE)


##plotting the scatter plot based on rank


library(ggplot2)
ggplot(reviews_imported,
       aes(x = reviews_imported$rating,
           y = reviews_imported$deg_of_sentiment,
           color = reviews_imported$rating)) +
  geom_jitter() +
  labs(x = "Rating",
       y = "Degree of Sentiment",
       color = "rating")


#
rm(list=ls()) #remove all tables
dev.off() #clear plots

cat("\014") #clear console
