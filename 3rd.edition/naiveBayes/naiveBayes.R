library(tm)
packageVersion("tm")
tbl.sms <- read.table("sms_spam.csv", sep=",", quote='"', header=TRUE)
head(tbl.sms, n=3)
#   type                                              text
# 1  ham Hope you are having a good week. Just checking in
# 2  ham                           K..give back my thanks.
# 3  ham       Am also doing in cbe only. But have to pay.

dim(tbl.sms)   # 5559 2

# create a "volatile" corpus, that is, stored in memory. contrast with PCorpus
corpus <- VCorpus(VectorSource(tbl.sms$text))  # the text column is a string vector
corpus.orig <- corpus
length(corpus)
as.character(corpus[[1]])  #  "Hope you are having a good week. Just checking in"

lapply(corpus[3:8], as.character)

corpus <- tm_map(corpus, content_transformer(tolower))
lapply(corpus[3:8], as.character)

getTransformations()
    # "removeNumbers"     "removePunctuation" "removeWords"       "stemDocument"      "stripWhitespace"

corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords())

print(corpus)
  # Metadata:  corpus specific: 0, document level (indexed): 0
  # Content:  documents: 5559

tm::inspect(corpus[1:2])
   # <<VCorpus>>
   # Metadata:  corpus specific: 0, document level (indexed): 0,    # Content:  documents: 2
   # [[1]] <<PlainTextDocument>>,    Metadata:  7,  Content:  chars: 34
   # [[2]] <<PlainTextDocument>>,    Metadata:  7,  Content:  chars: 21


# now get rid of punctuation characters, replacing them with a string:

replacePunctuation <- function(x) gsub("[[:punct:]]+", " ", x);
 # replacePunctuation("hello....world")  [1] "hello world"


corpus2 <- tm_map(corpus, content_transformer(replacePunctuation))

strings <- unlist(lapply(corpus, as.character))
length(grep("!", strings, fixed=TRUE))   # 913
strings <- unlist(lapply(corpus2, as.character))
length(grep("!", strings, fixed=TRUE))   # 0

#--------------------------------------------------------------------------------
# R interface to the libstemmer library
#--------------------------------------------------------------------------------
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns", "relearn"))
         # "learn"   "learn"   "learn"     "learn"   "relearn"

corpus <- tm_map(corpus2, stemDocument)
lapply(head(corpus.orig, n=3), as.character)
# "Hope you are having a good week. Just checking in"
# "K..give back my thanks."
# "Am also doing in cbe only. But have to pay."

lapply(head(corpus, n=3), as.character)
# "hope good week just check"
# "k give back thank"
# "also cbe pay"

stripWhitespace(" as    j    ")  # [1] " as j "

corpus <- tm_map(corpus, stripWhitespace)

# now create a document term matrix, document rows, word count columns

dtm <- DocumentTermMatrix(corpus)
dim(dtm)   # 5559 5980
as.matrix(dtm[1:10, 1001:1010])   # all zeros, as probabilities would predict

#-----------------------------------------------------------------------
# p 113 shows how to do all these tesps in one DocumentTermMatrix call
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# prepare for training the model, split 3/4 and 1/4
#-----------------------------------------------------------------------
round(3 * nrow(dtm)/4)  # 4169
dtm.train <- dtm[1:4169,]
dtm.test  <- dtm[4170:5559,]

#-----------------------------------------------------------------------
# retrain the labels also
#-----------------------------------------------------------------------
labels.train <- as.character(tbl.sms$type[1:4169])
labels.test  <- as.character(tbl.sms$type)[4170:5559]
                                    #  ham  spam
table(as.character(tbl.sms$type))   # 4812   747    13.4%
table(labels.train)                 # 3605   564    13.5%
table(labels.test)                  # 1207   183    13.1%

round(prop.table(table(as.character(tbl.sms$type))), digits=2)     # 0.86, 0.14
round(prop.table(table(labels.train)), digits=2)                   # 0.86, 0.14
round(prop.table(table(labels.test)), digits=2)                    # 0.87, 0.13
