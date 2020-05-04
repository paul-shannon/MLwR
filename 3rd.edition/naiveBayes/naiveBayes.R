library(tm)
packageVersion("tm")
tbl.sms <- read.table("sms_spam.csv", sep=",", quote='"', header=TRUE)
head(tbl.sms)
corpus <- VCorpus(VectorSource(tbl.sms$text))
length(corpus)
as.character(corpus[[1]])

lapply(corpus[3:8], as.character)

corpus <- tm_map(corpus, content_transformer(tolower))
lapply(corpus[3:8], as.character)

getTransformations()
    # "removeNumbers"     "removePunctuation" "removeWords"       "stemDocument"      "stripWhitespace"

corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords())


# next up: page 110 of the Lantz book
