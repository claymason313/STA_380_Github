library(mosaic)

gonefishing = read.csv('gonefishing.csv', header=TRUE)
summary(gonefishing)



hist(gonefishing$weight, breaks=20)
mean_weight_pop = mean(gonefishing$weight)
abline(v=mean_weight_pop, lwd=4, col='blue')

mean_weight_pop

n_fish = 30
# Take a random sample from the population of fish in the lake
fishing_trip = sample(gonefishing, n_fish)
# Look at the measurements of the first five fish we caught
head(fishing_trip, 5)



#     length height width weight orig.ids
## 533   11.7    4.7   1.8    387      533
## 35    12.9    5.1   1.9    572       35
## 539   10.8    4.3   1.7    400      539
## 605    9.6    3.8   1.4    333      605
## 214   12.4    4.9   1.9    537      214


mean_weight_sample = mean(fishing_trip$weight)
mean_weight_sample

## [1] 500.4667

fishing_trip = sample(gonefishing, n_fish)
mean_weight_sample = mean(fishing_trip$weight)
mean_weight_sample


do(25)*{
  fishing_trip = sample(gonefishing, n_fish)
  mean_weight_sample = mean(fishing_trip$weight)
  mean_weight_sample
}

# Save the Monte Carlo output
my_fishing_year = do(365)*{
  fishing_trip = sample(gonefishing, n_fish)
  mean_weight_sample = mean(fishing_trip$weight)
  mean_weight_sample
}
# Examine the first several entries
head(my_fishing_year)

##     result
## 1 504.8333
## 2 497.6667
## 3 519.2667
## 4 542.2000
## 5 587.9000
## 6 551.2333

hist(my_fishing_year$result, breaks=20)




sd(my_fishing_year$result)


# Define the volume variable and add it to the original data frame
gonefishing$volume = gonefishing$height * gonefishing$length * gonefishing$width
# Model weight versus volume
plot(weight ~ volume, data=gonefishing)
lm0 = lm(weight ~ volume, data=gonefishing)
abline(lm0)

coef(lm0)


Plot the population
plot(weight ~ volume, data=gonefishing)
abline(lm0)
# Take a sample, show the points, and fit a straight line
n_fish = 30
fishing_trip = sample(gonefishing, n_fish)
lm1 = lm(weight ~ volume, data=fishing_trip)
points(weight ~ volume, data=fishing_trip, pch=19, col='orange')
abline(lm1, lwd=3, col='orange')


my_fishing_year = do(365)*{
  fishing_trip = sample(gonefishing, n_fish)
  lm1 = lm(weight ~ volume, data=fishing_trip)
  coef(lm1)
}
# Look at the first few lines of the outpout
head(my_fishing_year)

##    Intercept   volume
## 1  52.908723 4.256654
## 2  63.129530 4.174433
## 3  67.565998 4.164029
## 4 -22.938851 4.817717
## 5  22.948715 4.716953
## 6   8.077959 4.735984

hist(my_fishing_year$volume)

sd(my_fishing_year$volume)

n_fish = 30
ghost_grey = rgb(0.1, 0.1, 0.1, 0.2)
ghost_red = rgb(0.8, 0.1, 0.1, 0.2)
plot(weight ~ volume, data=gonefishing, pch=19, col=ghost_grey, las=1)
abline(lm0, lwd=3, col='darkgrey')
# Take 100 samples and fit a straight line to each one
for(i in 1:100) {
  fishing_trip = sample(gonefishing, n_fish)
  lm1 = lm(weight ~ volume, data=fishing_trip)
  abline(lm1, col=ghost_red)
}





#################################################################
install.packages("quantmod")
yes
library(mosaic)
library(quantmod)
library(foreach)

# Import a few stocks
mystocks = c("MRK", "JNJ", "SPY")
getSymbols(mystocks)



# Adjust for splits and dividends
MRKa = adjustOHLC(MRK)
JNJa = adjustOHLC(JNJ)
SPYa = adjustOHLC(SPY)

# Look at close-to-close changes
plot(ClCl(MRKa))

# Combine close to close changes in a single matrix
all_returns = cbind(ClCl(MRKa),ClCl(JNJa),ClCl(SPYa))
head(all_returns)
all_returns = as.matrix(na.omit(all_returns))

# These returns can be viewed as draws from the joint distribution
pairs(all_returns)
plot(all_returns[,1], type='l')



# Look at the market returns over time
plot(all_returns[,3], type='l')

# An autocorrelation plot: nothing there
acf(all_returns[,3])

# The sample correlation matrix
cor(all_returns)


#### Now use a bootstrap approach
#### With more stocks

mystocks = c("WMT", "TGT", "XOM", "MRK", "JNJ")
myprices = getSymbols(mystocks, from = "2001-01-01")


# A chunk of code for adjusting all stocks
# creates a new object addind 'a' to the end
# For example, WMT becomes WMTa, etc
for(ticker in mystocks) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}

head(WMTa)

# Combine all the returns in a matrix
all_returns = cbind(	ClCl(WMTa),
                     ClCl(TGTa),
                     ClCl(XOMa),
                     ClCl(MRKa),
                     ClCl(JNJa))
head(all_returns)
all_returns = as.matrix(na.omit(all_returns))

# Compute the returns from the closing prices
pairs(all_returns)

# Sample a random return from the empirical joint distribution
# This simulates a random day
return.today = resample(all_returns, 1, orig.ids=FALSE)

# Update the value of your holdings
# Assumes an equal allocation to each asset
total_wealth = 10000
my_weights = c(0.2,0.2,0.2, 0.2, 0.2)
holdings = total_wealth*my_weights
holdings = holdings*(1 + return.today)

# Compute your new total wealth
total_wealth = sum(holdings)


# Now loop over two trading weeks
total_wealth = 10000
weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
holdings = weights * total_wealth
n_days = 10
wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
for(today in 1:n_days) {
  return.today = resample(all_returns, 1, orig.ids=FALSE)
  holdings = holdings + holdings*return.today
  total_wealth = sum(holdings)
  wealthtracker[today] = total_wealth
}
total_wealth
plot(wealthtracker, type='l')


# Now simulate many different possible scenarios  
initial_wealth = 10000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial_wealth
  weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
  holdings = weights * total_wealth
  n_days = 10
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}

head(sim1)
hist(sim1[,n_days], 25)

# Profit/loss
mean(sim1[,n_days])
hist(sim1[,n_days] - initial_wealth, breaks=30)

# Calculate 5% value at risk
quantile(sim1[,n_days], 0.05) - initial_wealth
initial_wealth - quantile(sim1[,n_days], 0.05)


##############
library(tm) 
library(magrittr)

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

#install.packages('tm')
## Test it on Adam Smith
adam = readerPlain("division_of_labor.txt")
adam
content(adam)

## apply to all of Simon Cowell's articles
## (probably not THE Simon Cowell: https://twitter.com/simoncowell)
## "globbing" = expanding wild cards in filename paths
file_list = Sys.glob('../data/ReutersC50/C50train/SimonCowell/*.txt')
simon = lapply(file_list, readerPlain) 

# The file names are ugly...
file_list

# Clean up the file names
# This uses the piping operator from magrittr
# See https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html
mynames = file_list %>%
{ strsplit(., '/', fixed=TRUE) } %>%
{ lapply(., tail, n=2) } %>%
{ lapply(., paste0, collapse = '') } %>%
  unlist

mynames
names(simon) = mynames

## once you have documents in a vector, you 
## create a text mining 'corpus' with: 
my_documents = Corpus(VectorSource(simon))

## Some pre-processing/tokenization steps.
## tm_map just maps some function to every document in the corpus

my_documents = tm_map(my_documents, content_transformer(tolower)) # make everything lowercase
my_documents = tm_map(my_documents, content_transformer(removeNumbers)) # remove numbers
my_documents = tm_map(my_documents, content_transformer(removePunctuation)) # remove punctuation
my_documents = tm_map(my_documents, content_transformer(stripWhitespace)) ## remove excess white-space

## Remove stopwords.  Always be careful with this: one man's trash is another one's treasure.
stopwords("en")
stopwords("SMART")
?stopwords
my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("en"))


## create a doc-term-matrix
DTM_simon = DocumentTermMatrix(my_documents)
DTM_simon # some basic summary statistics

class(DTM_simon)  # a special kind of sparse matrix format

## You can inspect its entries...
inspect(DTM_simon[1:10,1:20])

## ...find words with greater than a min count...
findFreqTerms(DTM_simon, 50)

## ...or find words whose count correlates with a specified word.
findAssocs(DTM_simon, "market", .5) 

## Finally, drop those terms that only occur in one or two documents
## This is a common step: the noise of the "long tail" (rare terms)
##	can be huge, and there is nothing to learn if a term occured once.
## Below removes those terms that have count 0 in >95% of docs.  
## Probably a bit stringent here... but only 50 docs!
DTM_simon = removeSparseTerms(DTM_simon, 0.95)
DTM_simon # now ~ 1000 terms (versus ~3000 before)

# Now PCA on term frequencies
X = as.matrix(DTM_simon)
X = X/rowSums(X)  # term-frequency weighting

pca_simon = prcomp(X, scale=TRUE)
plot(pca_simon) 

# Look at the loadings
pca_simon$rotation[order(abs(pca_simon$rotation[,1]),decreasing=TRUE),1][1:25]
pca_simon$rotation[order(abs(pca_simon$rotation[,2]),decreasing=TRUE),2][1:25]


## Plot the first two PCs..
plot(pca_simon$x[,1:2], xlab="PCA 1 direction", ylab="PCA 2 direction", bty="n",
     type='n')
text(pca_simon$x[,1:2], labels = 1:length(simon), cex=0.7)
identify(pca_simon$x[,1:2], n=4)

# Both about "Scottish Amicable"
content(simon[[46]])
content(simon[[48]])

# Both about genetic testing
content(simon[[25]])
content(simon[[26]])

# Both about Ladbroke's merger
content(simon[[10]])
content(simon[[11]])
