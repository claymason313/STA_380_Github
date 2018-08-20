##############################
# STA 380, Part 2: Exercises 2
# 
# Consider the data in ABIA.csv, which contains information on every commercial flight in 2008 that either departed from or landed at Austin-Bergstrom Interational Airport. The variable codebook is as follows:
#   
#   Year all 2008
# Month 1-12
# DayofMonth 1-31
# DayOfWeek 1 (Monday) - 7 (Sunday)
# DepTime actual departure time (local, hhmm)
# CRSDepTime scheduled departure time (local, hhmm)
# ArrTime actual arrival time (local, hhmm)
# CRSArrTime scheduled arrival time (local, hhmm)
# UniqueCarrier unique carrier code
# FlightNum flight number
# TailNum plane tail number
# ActualElapsedTime in minutes
# CRSElapsedTime in minutes
# AirTime in minutes
# ArrDelay arrival delay, in minutes
# DepDelay departure delay, in minutes
# Origin origin IATA airport code
# Dest destination IATA airport code
# Distance in miles
# TaxiIn taxi in time, in minutes
# TaxiOut taxi out time in minutes
# Cancelled was the flight cancelled?
# CancellationCode reason for cancellation (A = carrier, B = weather, C = NAS, D = security)
# Diverted 1 = yes, 0 = no
# CarrierDelay in minutes
# WeatherDelay in minutes
# NASDelay in minutes
# SecurityDelay in minutes
# LateAircraftDelay in minutes
# Your task is to create a figure, or set of related figures, that tell an interesting story about flights into and out of Austin. You can annotate the figure and briefly describe it, but strive to make it as stand-alone as possible. It shouldn't need many, many paragraphs to convey its meaning. Rather, the figure should speak for itself as far as possible. 


#For example, you might consider one of the following questions:
# 
# What is the best time of day to fly to minimize delays?
# What is the best time of year to fly to minimize delays?

# How do patterns of flights to different destinations or parts of the country change over the course of the year?
# What are the bad airports to fly to?

# But anything interesting will fly. If you want to try your hand at mapping or looking at geography, you can cross-reference the airport codes here: https://github.com/datasets/airport-codes. Combine this with a mapping package like ggmap, and you should have lots of possibilities!
# 

# Import library
library(ggplot2)
rm(list=ls())

ABIA = read.csv("/Users/claytonmason/GitHub/STA_380_Clay/Data/ABIA.csv")

head(ABIA, 5)


# Departure time - Extract Hour of Day
ABIA$Departure_hour = as.numeric(substr(ABIA$DepTime, 1, nchar(ABIA$DepTime)-2))
ABIA$Departure_hour

#unique flights column
ABIA$unique_flights <- paste(ABIA$UniqueCarrier,ABIA$FlightNum, ABIA$Month, ABIA$DayofMonth)


# Create delay column
ABIA$Delay <- ifelse(ABIA$ArrDelay > 0, 1, ifelse(ABIA$ArrDelay <= 0, "0", ifelse(ABIA$ArrDelay <= NA, NA,NA)))
ABIA$Delay
ABIA$Delay <- as.numeric(as.character(ABIA$Delay))

#view null data
colSums(!is.na(ABIA))

No_delay = sum(!is.na(ABIA$Delay[ABIA$Delay==0]))
Delay2 = sum(!is.na(ABIA$Delay[ABIA$Delay==1]))
Delay_NA = sum(is.na(ABIA$Delay))
No_delay
Delay2
Delay_NA

Total_Delay_Col = No_delay + Delay2 + Delay_NA
Total_Delay_Col


#42.7% of flights are delayed
Delay2 / Total_Delay_Col

#new delay data frame
ABIA_delay_df = ABIA[ABIA$Delay==1,]
ABIA_delay_df

#new Outbound df
ABIA_Outbound_df = ABIA[ABIA$Dest!="AUS",]
ABIA_Outbound_df = ABIA_Outbound_df[ABIA_Outbound_df$Delay!="NA",]
ABIA_Outbound_df

#new Inbound df
ABIA_Inbound_df = ABIA[ABIA$Dest=="AUS",]
ABIA_Inbound_df = ABIA_Inbound_df[ABIA_Inbound_df$Delay!="NA",]
ABIA_Inbound_df



#average delay by carrier
Carrier_mean = aggregate(ABIA_delay_df$ArrDelay, by=list(ABIA_delay_df$UniqueCarrier), FUN=mean)
Carrier_mean


library(dplyr)
ABIA %>%
  group_by(UniqueCarrier) %>%
  summarise(n_distinct(UniqueCarrier))



#average delay by carrier - plot
library(ggplot2)
ggplot(ABIA_delay_df) + 
  stat_summary(aes(x = UniqueCarrier, y = ArrDelay), 
               fun.y = function(x) mean(x), 
               geom = "bar")




#% of delays by carrier - plot
library(ggplot2)
ggplot(ABIA) + 
  stat_summary(aes(x = UniqueCarrier, y = Delay), 
               fun.y = function(x) sum(x)/length(x), 
               geom = "bar")


#% of delays by origin
library(ggplot2)
ggplot(ABIA) + 
  stat_summary(aes(x = Origin, y = Delay), 
               fun.y = function(x) sum(x)/length(x), 
               geom = "bar")


#% of delays by origin
library(ggplot2)
ggplot(ABIA) + 
  stat_summary(aes(x = Dest, y = Delay), 
               fun.y = function(x) sum(x)/length(x), 
               geom = "bar")





#number of flights by Destination
#install.packages("data.table")
library(data.table)
DT2 <- data.table(ABIA_Outbound_df)
DT2[, .(outbound_flights = length(unique(unique_flights)),delays = sum(Delay), delay_percent = sum(Delay)/length(unique(unique_flights))), by = Dest]

#number of flights by Origin
#install.packages("data.table")
library(data.table)
DT <- data.table(ABIA_Inbound_df)
DT[, .(inbound_flights = length(unique(unique_flights)),delays = sum(Delay), delay_percent = sum(Delay)/length(unique(unique_flights))), by = Origin]




#count of delays by hour
ggplot(data = ABIA_delay_df, aes(ABIA_delay_df$Departure_hour, ABIA_delay_df$Delay<-1 )) + stat_summary(fun.y = sum, geom = "bar") + xlim(0,24) + scale_y_continuous("sum of delays")

#mean arrival delays by hour of the day
ggplot(data = ABIA_delay_df, aes(ABIA_delay_df$Departure_hour, ABIA_delay_df$ArrDelay)) + stat_summary(fun.y = mean, geom = "bar") + xlim(0,24) + scale_y_continuous("mean of delays")


# Plot average arrival delays by day of week
ggplot(data = ABIA_delay_df, aes(ABIA_delay_df$DayOfWeek, ABIA_delay_df$ArrDelay)) + stat_summary(fun.y = mean, geom = "bar")  + xlim(0,8) + scale_y_continuous("Mean Arrival Delay (mins)")



# Plot average arrival delays by month
ggplot(data = ABIA_delay_df, aes(ABIA_delay_df$Month, ABIA_delay_df$ArrDelay)) + stat_summary(fun.y = mean, geom = "bar")





##############################
##Author attribution
# Revisit the Reuters C50 corpus that we explored in class. Your task is to build two separate models (using any combination of tools you see fit) for predicting the author of an article on the basis of that article's textual content. 
#Describe clearly what models you are using, how you constructed features, and so forth. 
#Yes, this is a supervised learning task, but it potentially draws on a lot of what you know about unsupervised learning, since constructing features for a document might involve dimensionality reduction.
# In the C50train directory, you have ~50 articles from each of 50 different authors (one author per directory). Use this training data (and this data alone) to build the two models. 
#Then apply your model to the articles by the same authors in the C50test directory, which is about the same size as the training set. How well do your models do at predicting the author identities in this out-of-sample setting? 
#Are there any sets of authors whose articles seem difficult to distinguish from one another? Which model do you prefer?
# Note: you will need to figure out a way to deal with words in the test set that you never saw in the training set. This is a nontrivial aspect of the modeling exercise. You might, for example, consider adding a pseudo-word to the training set vocabulary, corresponding to "word not seen before," 
#and add a pseudo-count to it so it doesn't look like these out-of-vocabulary words have zero probability on the testing set.




rm(list=ls())
## The tm library and related plugins comprise R's most popular text-mining stack.
## See http://cran.r-project.org/web/packages/tm/vignettes/tm.pdf



## tm has many "reader" functions.  Each one has
## arguments elem, language, id
## (see ?readPlain, ?readPDF, ?readXML, etc)
## This wraps another function around readPlain to read
## plain text documents in English.
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

# Import libraries
library(tm)
library(SnowballC)
library(plyr)
library(tm) 
library(magrittr)
library(slam)
library(proxy)
library(glmnet)
library(nnet)

# Roll directories together into a single corpus
## "globbing" = expanding wild cards in filename paths
setwd("/Users/claytonmason/GitHub/STA_380_Clay/Data")
file_list_import_train = Sys.glob('../data/ReutersC50/C50train/*')

# Loop through file_list_import_train to get the files and the authors
file_list_train = c()
labels_train = c()
for(author_train in file_list_import_train) {
  author_name_train = substring(author_train, first=29)
  files_to_add_train = Sys.glob(paste0(author_train, '/*.txt'))
  file_list_train = append(file_list_train, files_to_add_train)
  labels_train = append(labels_train, rep(author_name_train, length(files_to_add_train)))
}
file_list_train

# Read in file_list and remove .txt from the file name
all_docs_train = lapply(file_list_train, readerPlain) 
names(all_docs_train) = file_list_train
names(all_docs_train) = sub('.txt', '', names(all_docs_train))

## once you have documents in a vector, you 
## create a text mining 'corpus' with:
my_corpus_train = Corpus(VectorSource(all_docs_train))

#ugh - https://stackoverflow.com/questions/40462805/names-function-in-r-not-working-as-expected
#https://stackoverflow.com/questions/10566473/names-attribute-must-be-the-same-length-as-the-vector
length(my_corpus_train)
length(labels_train)

names(my_corpus_train) = labels_train
labels_train


## Some pre-processing/tokenization steps.
## tm_map just maps some function to every document in the corpus
my_corpus_train = tm_map(my_corpus_train, content_transformer(tolower)) # make everything lowercase
my_corpus_train = tm_map(my_corpus_train, content_transformer(removeNumbers)) # remove numbers
my_corpus_train = tm_map(my_corpus_train, content_transformer(removePunctuation)) # remove punctuation
my_corpus_train = tm_map(my_corpus_train, content_transformer(stripWhitespace)) # remove excess white-space

## Remove stopwords.  Always be careful with this: one person's trash is another one's treasure.
stopwords("en")
stopwords("SMART")
?stopwords
my_corpus_train = tm_map(my_corpus_train, content_transformer(removeWords), stopwords("SMART")) # remove stop words
my_corpus_train = tm_map(my_corpus_train, stemDocument) # combine stem words

## create a doc-term-matrix
DTM_train = DocumentTermMatrix(my_corpus_train)
DTM_train # some basic summary statistics

# a special kind of sparse matrix format
class(DTM_train)

## You can inspect its entries...
inspect(DTM_train[1:10,1:20])

## ...find words with greater than a min count...
findFreqTerms(DTM_train, 100)

## ...or find words whose count correlates with a specified word.
findAssocs(DTM_train, "fed", .5)

## Finally, drop those terms that only occur in one or two documents
## This is a common step: the noise of the "long tail" (rare terms)
##	can be huge, and there is nothing to learn if a term occured once.
## Below removes those terms that have count 0 in >94% of docs.  
# Remove sparse terms
DTM_train = removeSparseTerms(DTM_train, 0.94)
DTM_train

# Create a dense matrix
X_train = as.matrix(DTM_train)
X_train




########### repeat steps for test data

file_list_import_test = Sys.glob('../data/ReutersC50/C50test/*')

#  get the files and the authors
file_list_test = c()
labels_test = c()
for(author_test in file_list_import_test) {
  author_name_test = substring(author_test, first=29)
  files_to_add_test = Sys.glob(paste0(author_test, '/*.txt'))
  file_list_test = append(file_list_test, files_to_add_test)
  labels_test = append(labels_test, rep(author_name_test, length(files_to_add_test)))
}
file_list_test

# Read in file_list and remove .txt from the file name
all_docs_test = lapply(file_list_test, readerPlain) 
names(all_docs_test) = file_list_test
names(all_docs_test) = sub('.txt', '', names(all_docs_test))
all_docs_test



## once you have documents in a vector, you 
## create a text mining 'corpus' with:
my_corpus_test = Corpus(VectorSource(all_docs_test))

#ugh - https://stackoverflow.com/questions/40462805/names-function-in-r-not-working-as-expected
#https://stackoverflow.com/questions/10566473/names-attribute-must-be-the-same-length-as-the-vector
length(my_corpus_test)
length(labels_test)
names(my_corpus_test) = labels_test






## Some pre-processing/tokenization steps.
## tm_map just maps some function to every document in the corpus
my_corpus_test = tm_map(my_corpus_test, content_transformer(tolower)) # make everything lowercase
my_corpus_test = tm_map(my_corpus_test, content_transformer(removeNumbers)) # remove numbers
my_corpus_test = tm_map(my_corpus_test, content_transformer(removePunctuation)) # remove punctuation
my_corpus_test = tm_map(my_corpus_test, content_transformer(stripWhitespace)) # remove excess white-space
## Remove stopwords.  Always be careful with this: one person's trash is another one's treasure.
stopwords("en")
stopwords("SMART")
?stopwords
my_corpus_test = tm_map(my_corpus_test, content_transformer(removeWords), stopwords("SMART")) # remove stop words
my_corpus_test = tm_map(my_corpus_test, stemDocument) # combine stem words




## create a doc-term-matrix
DTM_test = DocumentTermMatrix(my_corpus_test)
DTM_test # some basic summary statistics

# a special kind of sparse matrix format
class(DTM_test)

## You can inspect its entries...
inspect(DTM_test[1:10,1:20])

## ...find words with greater than a min count...
findFreqTerms(DTM_test, 50)

## ...or find words whose count correlates with a specified word.
findAssocs(DTM_test, "fed", .5)


## Finally, drop those terms that only occur in one or two documents
## This is a common step: the noise of the "long tail" (rare terms)
##	can be huge, and there is nothing to learn if a term occured once.
## Below removes those terms that have count 0 in >95% of docs.  
# Remove sparse terms
DTM_test = removeSparseTerms(DTM_test, 0.94)
DTM_test


# Create a dense matrix
X_test = as.matrix(DTM_test)
X_test






# Get the list of words in the training set
X_words = colnames(X_train)
X_words


# Get the list of words in the test set
X_test_words = colnames(X_test)
X_test_words

# Create 2 empty vectors to store words to add to test and words to drop from test
test_add = vector(length=0)
test_drop = vector(length=0)

# Loop through the test words and add those not in the train to the vector test_drop
for (test_word in X_test_words) {
  if (!test_word %in% X_words) {
    test_drop <- c(test_drop, test_word)
  }
}
test_drop



# Loop through the train words and add those not in test to the vector test_add
for (word in X_words) {
  if (!word %in% X_test_words) {
    test_add <- c(test_add, word)
  }
}
test_add



# Create a matrix of 0's to insert into the test matrix

zero <- matrix(0, nrow = nrow(X_train), ncol=length(test_add))

# Name the columns using the words in test_add
colnames(zero) <- test_add

# Add the zero matrix to the test matrix
X2_test = cbind(X_test, zero)


# Sort the columns alphabetically so they match the X2
X2_test = X2_test[,order(colnames(X2_test))]
X2_test

# Drop the words in test_drop from the test matrix
X2_test = X2_test[,!colnames(X2_test) %in% test_drop]

X2_test




# Create a dense matrix
X = as.matrix(DTM_train)

# Calculate the smoothing factor
smooth_count = 1/nrow(X)
nrow(X)
smooth_count

colnames(X)
labels_train


#ugh I believe this is where my issue lies
# Add the smoothing factor and aggregate the word counts + smoothing factor for each author
by_word_wc = rowsum(X + smooth_count, labels_train)
by_word_wc
smooth_count
X


# Sum the word counts + smoothing factor for each word for each author
total_wc = rowSums(by_word_wc)
total_wc


#  multinomial probability vector
w = by_word_wc / total_wc
w

# Log the vector for easier interpretability
w = log(w)
w
# Set X2 equal to the multinomial probability vector w
X2 = w


# Transpose the multinomial probability vector for matrix multiplication
X2 = t(X2)
X2

# Multiply the test matrix by X2
log_prob = X2_test %*% X2
colnames(log_prob)

# Get the prediction by return the column name of the max value for each document 
predict = colnames(log_prob)[max.col(log_prob)]
predict

# Add the prediction the the matrix
log_prob = cbind(log_prob, predict)
head(log_prob,10)
rownames(log_prob) #these are numbers. I need 
colnames(log_prob)
log_prob[,51]


### i cannot figure out how to check the accuracy of my predictions
# Create a column that checks the prediction against the actual

accurate = as.integer(rownames(log_prob) == log_prob[,51])
accurate




###PCA
# construct TF IDF weights
tfidf_train = weightTfIdf(DTM_train)

####
# Compare documents
####

inspect(tfidf_train[1,])
inspect(tfidf_train[2,])
inspect(tfidf_train[3,])

# could go back to the raw corpus
content(my_corpus_train[[1]])
content(my_corpus_train[[2]])
content(my_corpus_train[[3]])

# cosine similarity
i = 1
j = 3
sum(tfidf_train[i,] * (tfidf_train[j,]))/(sqrt(sum(tfidf_train[i,]^2)) * sqrt(sum(tfidf_train[j,]^2)))


# the full set of cosine similarities
# two helper functions that use some linear algebra for the calculations
cosine_sim_docs = function(dtm) {
  crossprod_simple_triplet_matrix(t(dtm))/(sqrt(col_sums(t(dtm)^2) %*% t(col_sums(t(dtm)^2))))
}

# use the function to compute pairwise cosine similarity for all documents
cosine_sim_mat = cosine_sim_docs(tfidf_train)
# Now consider a query document
content(my_corpus_train[[17]])
cosine_sim_mat[17,]

# looks like document 16 has the highest cosine similarity
sort(cosine_sim_mat[18,], decreasing=TRUE)
content(my_corpus_train[[18]])
content(my_corpus_train[[19]])

#####
# Cluster documents
#####

# define the cosine distance
cosine_dist_mat = proxy::dist(as.matrix(tfidf_train), method='cosine')
tree_simon = hclust(cosine_dist_mat)
plot(tree_simon)
clust5 = cutree(tree_simon, k=5)

# inspect the clusters
which(clust5 == 1)
content(my_corpus_train[[1]])
content(my_corpus_train[[4]])
content(my_corpus_train[[5]])



####
# Dimensionality reduction
####

# Now PCA on term frequencies
X = as.matrix(tfidf_train)
summary(colSums(X))
scrub_cols = which(colSums(X) == 0)
X = X[,-scrub_cols]

pca_train = prcomp(X, scale=TRUE)
plot(pca_train) 

# Look at the loadings
pca_train$rotation[order(abs(pca_train$rotation[,1]),decreasing=TRUE),1][1:25]
pca_train$rotation[order(abs(pca_train$rotation[,2]),decreasing=TRUE),2][1:25]


## Look at the first two PCs..
# We've now turned each document into a single pair of numbers -- massive dimensionality reduction
pca_train$x[,1:2]

plot(pca_train$x[,1:2], xlab="PCA 1 direction", ylab="PCA 2 direction", bty="n",
     type='n')
text(pca_train$x[,1:2], labels = 1:length(simon), cex=0.7)



# Conclusion: even just these two-number summaries still preserve a lot of information


# Now look at the word view
# 5-dimensional word vectors
word_vectors = pca_train$rotation[,1:5]

word_vectors[982,]

d_mat = dist(word_vectors)





# # ###################################
# # Now PCA on term frequencies
# X_train2 = as.matrix(DTM_train)
# X_train2 = X_train2/rowSums(X_train2)
# 
# 
# 
# pca_X_train2 = prcomp(X_train2, scale=TRUE)
# #plot(pca_X_train2)
# 
# # Look at the loadings
# pca_X_train2$rotation[order(abs(pca_X_train2$rotation[,1]),decreasing=TRUE),1]
# pca_X_train2$rotation[order(abs(pca_X_train2$rotation[,2]),decreasing=TRUE),2]
# 
# ## Look at the first two PCs..
# # We've now turned each document into a single pair of numbers -- massive dimensionality reduction
# pca_X_train2$x[,1:2]
# 
# 
# 
# # Plot the first two PCs..
# plot(pca_X_train2$x[,1:2], xlab="PCA 1 direction", ylab="PCA 2 direction", bty="n",
#     type='n')
# text(pca_X_train2$x[,1:2], labels = 1:length(all_docs_train), cex=0.7)
# identify(pca_X_train2$x[,1:2], n=4)
# 
# 
# # Now look at the word view
# # 5-dimensional word vectors
# word_vectors = pca_X_train2$rotation[,1:5]
# 
# word_vectors[982,]
# 
# d_mat = dist(word_vectors)
# 
# 
# 
# 
# 
# 
# ###################
# # Now PCA on term frequencies
# X_test2 = as.matrix(DTM_test)
# X_test2 = X_test2/rowSums(X_test2)
# 
# 
# 
# pca_X_test2 = prcomp(X_test2, scale=TRUE)
# plot(pca_X_test2)
# 
# # Look at the loadings
# pca_X_test2$rotation[order(abs(pca_X_test2$rotation[,1]),decreasing=TRUE),1][1:25]
# pca_X_test2$rotation[order(abs(pca_X_test2$rotation[,2]),decreasing=TRUE),2][1:25]
# 
# 
# ## Plot the first two PCs..
# plot(pca_X_test2$x[,1:2], xlab="PCA 1 direction", ylab="PCA 2 direction", bty="n",
#      type='n')
# text(pca_X_test2$x[,1:2], labels = 1:length(all_docs_test), cex=0.7)
# identify(pca_X_test2$x[,1:2], n=4)
# 





###################











##############################
# Practice with association rule mining
#Revisit the notes on association rule mining, and walk through the R example on music playlists: playlists.R and playlists.csv. Then use the data on grocery purchases in groceries.txt and find some interesting association rules for these shopping baskets. 
#The data file is a list of baskets: one row per basket, with multiple items per row separated by commas -- you'll have to cobble together a few utilities for processing this into the format expected by the "arules" package. 
#Pick your own thresholds for lift and confidence; just be clear what these thresholds are and how you picked them. Do your discovered item sets make sense? Present your discoveries in an interesting and concise way.


#https://github.com/jgscott/STA380/blob/master/notes/association_rules.pdf


#Association Rule Mining - find rules that will predict the occurrence of an item based on the occurrences of other items in the transaction
# goal of association rule mining is to find all rules having
# – support 􏰆 minsup threshold ( Fraction of transactions that contain both X and Y
# – confidence 􏰆 minconf threshold (c = Measures how often items in Y appear in transactions that contain X)

#install.packages("arulesViz")
rm(list=ls())
    
library(arulesViz)
library(arules)
library(reshape2)
library(plyr)
setwd("/Users/claytonmason/GitHub/STA_380_Clay/Data")
groceries = read.transactions("groceries.txt", format="basket", sep=",")
dim(groceries)

#9,835 rows and 169 variables


# #Cast this variable as a special arules transactions class
groceries_transaction <- as(groceries, "transactions")

# Now run the 'apriori' algorithm
# Look at rules with support > .01 & confidence >.5 & length(# of items) <= 4
groceries_rules1 <- apriori(groceries_transaction, parameter=list(support=.01, confidence=.5, maxlen=4))

#Top 10 Support
top.support <- sort(groceries_rules1, decreasing = TRUE, na.last = NA, by = "support")
inspect(sort(top.support)[1:10])

#Top 10 Confidence
top.confidence <- sort(groceries_rules1, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 10))


## Choose a subset
inspect(subset(groceries_rules1, subset=lift > 3))
inspect(subset(groceries_rules1, subset=confidence > 0.5))
inspect(subset(groceries_rules1, subset=support > .02 & confidence > 0.5))









# Look at rules with support > .02 & confidence >.2 & length(# of items) <= 3
groceries_rules2 <- apriori(groceries_transaction, parameter=list(support=.02, confidence=.2, maxlen=4))

#Top 10 Support
top.support <- sort(groceries_rules2, decreasing = TRUE, na.last = NA, by = "support")
inspect(sort(top.support)[1:10])

#Top 10 Confidence
top.confidence <- sort(groceries_rules2, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 10))

inspect(subset(groceries_rules2, subset=lift > 2))
inspect(subset(groceries_rules2, subset=confidence > 0.5))
inspect(subset(groceries_rules2, subset=support > .02 & confidence > 0.5))








# Look at rules with support > .0001 & confidence >.6 & length(# of items) <= 4
groceries_rules3 <- apriori(groceries_transaction, parameter=list(support=.0001, confidence=.6, maxlen=4))

#Top 10 Support
top.support <- sort(groceries_rules3, decreasing = TRUE, na.last = NA, by = "support")
inspect(sort(top.support)[1:10])

#Top 10 Confidence
top.confidence <- sort(groceries_rules3, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 10))



















# Association rule mining
# Adapted from code by Matt Taddy
library(arules)  # has a big ecosystem of packages built around it


# Read in playlists from users
playlists_raw <- read.csv("../data/playlists.csv")

str(playlists_raw)
summary(playlists_raw)

# Turn user into a factor
playlists_raw$user <- factor(playlists_raw$user)

# First create a list of baskets: vectors of items by consumer
# Analagous to bags of words

# apriori algorithm expects a list of baskets in a special format
# In this case, one "basket" of songs per user
# First split data into a list of artists for each user
playlists <- split(x=playlists_raw$artist, f=playlists_raw$user)

## Remove duplicates ("de-dupe")
playlists <- lapply(playlists, unique)

## Cast this variable as a special arules "transactions" class.
playtrans <- as(playlists, "transactions")

# Now run the 'apriori' algorithm
# Look at rules with support > .01 & confidence >.5 & length (# artists) <= 4
musicrules <- apriori(playtrans, 
                      parameter=list(support=.01, confidence=.5, maxlen=4))

# Look at the output
inspect(musicrules)

## Choose a subset
inspect(subset(musicrules, subset=lift > 5))
inspect(subset(musicrules, subset=confidence > 0.6))
inspect(subset(musicrules, subset=support > .02 & confidence > 0.6))
