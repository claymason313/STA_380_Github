# STA 380, Part 2: Exercises 1
# Part A.
# Here's a question a friend of mine was asked when he interviewed at Google.
# Visitors to your website are asked to answer a single survey question before they get access to the content on the page. Among all of the users, there are two categories: Random Clicker (RC), and Truthful Clicker (TC). There are two possible answers to the survey: yes and no. Random clickers would click either one with equal probability. You are also giving the information that the expected fraction of random clickers is 0.3.
# After a trial period, you get the following survey results: 65% said Yes and 35% said No.
# What fraction of people who are truthful clickers answered yes?


rm(list=ls())
Random_choser =.3
truthful_choser= .7
Random_choser_no = .5
Random_choser_yes = .5

Total_yes = .65
Total_no = .35

#truthful_choser_yes = 

truthful_choser_yes = (Total_yes - (Random_choser_yes*Random_choser))/ (truthful_choser)
truthful_choser_yes

truthful_choser_no = 1 - truthful_choser_yes
truthful_choser_no
print("Overall probabability table is shown below")

prob_problem = matrix(c(15,50,15,20), ncol=2)
colnames(prob_problem) <- c('Yes', 'No')
rownames(prob_problem) <- c('Random', 'Truthful')
prob_problem.table <- as.table(prob_problem)
prob_problem.table



print("(.5*.3)+(.7*truthful_choser_yes)=.65")
print(truthful_choser_yes)
print("truthful choosers who answered 'yes' represent 5/(5+2) or 71% of the Truthful population")












###################################
# Part B.
# Imagine a medical test for a disease with the following two attributes:
# •	The sensitivity is about 0.993. That is, if someone has the disease, there is a probability of 0.993 that they will test positive.
# •	The specificity is about 0.9999. This means that if someone doesn't have the disease, 
#there is probability of 0.9999 that they will test negative.
# In the general population, incidence of the disease is reasonably rare: 
#about 0.0025% of all people have it (or 0.000025 as a decimal probability).

# Suppose someone tests positive. What is the probability that they have the disease? 
#In light of this calculation, do you envision any problems in implementing a 
#universal testing policy for the disease?
rm(list=ls())

  
print("Seeking true_positives_P(D|PT). Find PT first")

sensitivity = 0.993 #P(PT|D)
specificity = 0.9999 #P(NT|ND)
Disease = 0.000025 #P(D)
No_Disease = (1-Disease) #P(ND)
No_Disease
false_positives = (1-specificity) #P(PT|ND)

Positive_Test = (sensitivity * Disease) + (false_positives*No_Disease) #P(PT)
Positive_Test

true_positives = sensitivity*Disease / Positive_Test #P(D|PT) 
true_positives


# sensitivity  = true_positives / (true_positives + false_negatives) #P(PT|D)
# specificity = true_negatives / (true_negatives + false_positives) #P(NT|ND)
# Negative_Test  = #P(NT)
# true_negatives  = #P(ND|NT)
# true_positives = #P(D|PT)
# false_negatives = #P(NT|D)
# false_positives = #P(PT|ND) 

print("Suppose someone tests positive. What is the probability that they have the disease? 
In light of this calculation, do you envision any problems in implementing a 
universal testing policy for the disease?")

print("Given that someone tests positive on the test, there is approximately 19.88%
chance that the person will have the disease. The odds are pretty low. Depending on the disease and treatment, 
it could be detrimental to treat a healthy person")





##########################################################
#   Exploratory analysis: green buildings
# The case
# Over the past decade, both investors and the general public have paid increasingly close attention to the benefits of environmentally conscious buildings. There are both ethical and economic forces at work here. In commercial real estate, issues of eco-friendliness are intimately tied up with ordinary decisions about how to allocate capital. In this context, the decision to invest in eco-friendly buildings could pay off in at least four ways.
# 1.	Every building has the obvious list of recurring costs: water, climate control, lighting, waste disposal, and so forth. Almost by definition, these costs are lower in green buildings.
# 2.	Green buildings are often associated with better indoor environments—the kind that are full of sunlight, natural materials, and various other humane touches. Such environments, in turn, might result in higher employee productivity and lower absenteeism, and might therefore be more coveted by potential tenants. The financial impact of this factor, however, is rather hard to quantify ex ante; you cannot simply ask an engineer in the same way that you could ask a question such as, “How much are these solar panels likely to save on the power bill?”
# 3.	Green buildings make for good PR. They send a signal about social responsibility and ecological awareness, and might therefore command a premium from potential tenants who want their customers to associate them with these values. It is widely believed that a good corporate image may enable a firm to charge premium prices, to hire better talent, and to attract socially conscious investors.
# 4.	Finally, sustainable buildings might have longer economically valuable lives. For one thing, they are expected to last longer, in a direct physical sense. (One of the core concepts of the green-building movement is “life-cycle analysis,” which accounts for the high front-end environmental impact of ac- quiring materials and constructing a new building in the first place.) Moreover, green buildings may also be less susceptible to market risk—in particular, the risk that energy prices will spike, driving away tenants into the arms of bolder, greener investors.
# Of course, much of this is mere conjecture. At the end of the day, tenants may or may not be willing to pay a 
#premium for rental space in green buildings. We can only find out by carefully examining data on the commercial real-estate market.
# The file greenbuildings.csv contains data on 7,894 commercial rental properties from across the United States. Of these, 685 properties have been awarded either LEED or EnergyStar certification as a green building. You can easily find out more about these rating systems on the web, e.g. at www.usgbc.org. The basic idea is that a commercial property can receive a green certification if its energy efficiency, carbon footprint, site selection, and building materials meet certain environmental benchmarks, as certified by outside engineers.
# A group of real estate economists constructed the data in the following way. Of the 1,360 green-certified buildings listed as of December 2007 on the LEED or EnergyStar websites, current information about building characteristics and monthly rents were available for 685 of them. In order to provide a control population, each of these 685 buildings was matched to a cluster of nearby commercial buildings in the CoStar database. Each small cluster contains one green-certified building, and all non-rated buildings within a quarter-mile radius of the certified building. On average, each of the 685 clusters contains roughly 12 buildings, for a total of 7,894 data points.

# The columns of the data set are coded as follows:
# 	CS.PropertyID: the building's unique identifier in the CoStar database.
# •	cluster: an identifier for the building cluster, with each cluster containing one green-certified building and at least one other non-green-certified building within a quarter-mile radius of the cluster center.
# •	size: the total square footage of available rental space in the building.
# •	empl.gr: the year-on-year growth rate in employment in the building's geographic region.
# •	Rent: the rent charged to tenants in the building, in dollars per square foot per calendar year.
# •	leasing.rate: a measure of occupancy; the fraction of the building's available space currently under lease.
# •	stories: the height of the building in stories.
# •	age: the age of the building in years.
# •	renovated: whether the building has undergone substantial renovations during its lifetime.
# •	class.a, class.b: indicators for two classes of building quality (the third is Class C). These are relative classifications within a specific market. Class A buildings are generally the highest-quality properties in a given market. Class B buildings are a notch down, but still of reasonable quality. Class C buildings are the least desirable properties in a given market.
# •	green.rating: an indicator for whether the building is either LEED- or EnergyStar-certified.
# •	LEED, Energystar: indicators for the two specific kinds of green certifications.
# •	net: an indicator as to whether the rent is quoted on a ``net contract'' basis. Tenants with net-rental contracts pay their own utility costs, which are otherwise included in the quoted rental price.
# •	amenities: an indicator of whether at least one of the following amenities is available on-site: bank, convenience store, dry cleaner, restaurant, retail shops, fitness center.
# •	cd.total.07: number of cooling degree days in the building's region in 2007. A degree day is a measure of demand for energy; higher values mean greater demand. Cooling degree days are measured relative to a baseline outdoor temperature, below which a building needs no cooling.
# •	hd.total07: number of heating degree days in the building's region in 2007. Heating degree days are also measured relative to a baseline outdoor temperature, above which a building needs no heating.
# •	total.dd.07: the total number of degree days (either heating or cooling) in the building's region in 2007.
# •	Precipitation: annual precipitation in inches in the building's geographic region.
# •	Gas.Costs: a measure of how much natural gas costs in the building's geographic region.
# •	Electricity.Costs: a measure of how much electricity costs in the building's geographic region.
# •	cluster.rent: a measure of average rent per square-foot per calendar year in the building's local market.


# The assignment
# An Austin real-estate developer is interested in the possible economic impact of "going green" in her latest project: a new 15-story mixed-use building on East Cesar Chavez, just across I-35 from downtown. Will investing in a green building be worth it, from an economic perspective? The baseline construction costs are $100 million, with a 5% expected premium for green certification.
# The developer has had someone on her staff, who's been described to her as a "total Excel guru from his undergrad statistics course," run some numbers on this data set and make a preliminary recommendation. Here's how this person described his process.
# I began by cleaning the data a little bit. In particular, I noticed that a handful of the buildings in the data set had very low occupancy rates (less than 10% of available space occupied). I decided to remove these buildings from consideration, on the theory that these buildings might have something weird going on with them, and could potentially distort the analysis. Once I scrubbed these low-occupancy buildings from the data set, I looked at the green buildings and non-green buildings separately. The median market rent in the non-green buildings was $25 per square foot per year, while the median market rent in the green buildings was $27.60 per square foot per year: about $2.60 more per square foot. (I used the median rather than the mean, because there were still some outliers in the data, and the median is a lot more robust to outliers.) Because our building would be 250,000 square feet, this would translate into an additional $250000 x 2.6 = $650000 of extra revenue per year if we build the green building.
# Our expected baseline construction costs are $100 million, with a 5% expected premium for green certification. Thus we should expect to spend an extra $5 million on the green building. Based on the extra revenue we would make, we would recuperate these costs in $5000000/650000 = 7.7 years. Even if our occupancy rate were only 90%, we would still recuperate the costs in a little over 8 years. Thus from year 9 onwards, we would be making an extra $650,000 per year in profit. Since the building will be earning rents for 30 years or more, it seems like a good financial move to build the green building.
# The developer listened to this recommendation, understood the analysis, and still felt unconvinced. She has therefore asked you to revisit the report, so that she can get a second opinion.
# Do you agree with the conclusions of her on-staff stats guru? If so, point to evidence supporting his case. If not, explain specifically where and why the analysis goes wrong, and how it can be improved. (For example, do you see the possibility of confounding variables for the relationship between rent and green status?)
# Note: this is intended mainly as an exercise in visual and numerical story-telling. Tell your story primarily in plots, and while you can run a regression model if you want, that's not the goal here. Keep it concise.


library(mosaic)

green = read.csv('greenbuildings.csv')
summary(green)

# Extract the buildings with green ratings
green_only = subset(green, green_rating==1)
dim(green_only)

# Not a normal distribution at all
hist(green_only$Rent, 25)
mean(green_only$Rent)

# Normal-based confidence interval for the sample mean
xbar = mean(green_only$Rent)
sig_hat = sd(green_only$Rent)
se_hat = sig_hat/sqrt(nrow(green_only))
xbar + c(-1.96,1.96)*se_hat

# Using R's lm function
model1 = lm(Rent ~ 1, data=green_only)
confint(model1, level=0.95)


### Compare with bootstrapping

# a single bootstrapped sample (repeat a few times)
green_only_boot = resample(green_only)
mean(green_only_boot$Rent)

# Get a feel for what it is in the green_only_boot object
head(green_only_boot)

# Now repeat 2500 times
boot1 = do(2500)*{
  mean(resample(green_only)$Rent)
}
head(boot1)
hist(boot1$result, 30)
sd(boot1$result)

# Extract the confidence interval from the bootstrapped samples
confint(boot1, level=0.95)
xbar + c(-1.96,1.96)*se_hat


####
# Bootstrap the median
####

median(green_only$Rent)
# Now repeat 2500 times
boot2 = do(2500)*{
  median(resample(green_only)$Rent)
}
head(boot2)

# Ugly!
hist(boot2$result, 30)

# But we still get a confidence interval
confint(boot2)







###################################
# Bootstrapping
# Consider the following five asset classes, together with the ticker symbol for an exchange-traded fund that represents each class:
# •	US domestic equities (SPY: the S&P 500 stock index)
# •	US Treasury bonds (TLT)
# •	Investment-grade corporate bonds (LQD)
# •	Emerging-market equities (EEM)
# •	Real estate (VNQ)
# If you're unfamiliar with exchange-traded funds, you can read a bit about them here.
# Download several years of daily data on these ETFs, using the functions in the quantmod package, as we used in class. Go back far enough historically so that you get both good runs and bad runs of stock-market performance. Now explore the data and come to an understanding of the risk/return properties of these assets. Then consider three portfolios:
#   •	the even split: 20% of your assets in each of the five ETFs above.
# •	something that seems safer than the even split, comprising investments in at least three classes. You choose the allocation, and you can certainly invest in more than three assets if you want. (You can even choose different ETFs if you want.)
# •	something more aggressive (again, you choose the allocation) comprising investments in at least two classes/assets. By more aggressive, I mean a portfolio that looks like it has a chance at higher returns, but also involves more risk of loss.
# Suppose there is a notional $100,000 to invest in one of these portfolios. Write a brief report that:
# •	marshals appropriate evidence to characterize the risk/return properties of the five major asset classes listed above.
# •	outlines your choice of the "safe" and "aggressive" portfolios.
# •	uses bootstrap resampling to estimate the 4-week (20 trading day) value at risk of each of your three portfolios at the 5% level.
# •	compares the results for each portfolio in a way that would allow the reader to make an intelligent decision among the three options.
# You should assume that your portfolio is rebalanced each day at zero transaction cost. That is, if you're aiming for 50% SPY and 50% TLT, you always redistribute your wealth at the end of each day so that the 50/50 split is retained, regardless of that day's appreciation/depreciation.


rm(list=ls())

library(mosaic)
library(quantmod)
library(foreach)

# Import a few stocks
mystocks = c("SPY", "TLT", "LQD","EEM","VNQ")
getSymbols(mystocks, from = "2005-01-01")

# Adjust for splits and dividends



for(ticker in mystocks) {
  expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
  eval(parse(text=expr))
}

# Look at close-to-close changes

plot(ClCl(SPYa))
plot(ClCl(TLTa))
plot(ClCl(LQDa))
plot(ClCl(EEMa))
plot(ClCl(VNQa))

# make sure we have returns for the full sample. 2005 was firt year for all 5 indices to have their returns published
head(SPYa)
head(TLTa)
head(LQDa)
head(EEMa)
head(VNQa)



# Combine close to close changes in a single matrix
all_returns = cbind(
  ClCl(SPYa), 
  ClCl(TLTa), 
  ClCl(LQDa), 
  ClCl(EEMa), 
  ClCl(VNQa)
  )


head(all_returns,2)
all_returns = as.matrix(na.omit(all_returns))





dev.off()

# These returns can be viewed as draws from the joint distribution
# Compute the returns from the closing prices
pairs(all_returns)




print ("Look at the market/S&P 500 returns over time")
print ("Annual Standard Deviation: ") 
sd(all_returns[,1])*sqrt(250)
plot(all_returns[,1], type='l')


# An autocorrelation plot: nothing there
# acf(all_returns[,1])



print ("Look at US Treasury bonds (TLT) returns over time")
print ("Annual Standard Deviation: ") 
sd(all_returns[,2])*sqrt(250)
plot(all_returns[,2], type='l')

# An autocorrelation plot: nothing there
# acf(all_returns[,2])





print ("Look at Investment-grade corporate bonds (LQD)returns over time")
print ("Annual Standard Deviation: ") 
sd(all_returns[,3])*sqrt(250)
plot(all_returns[,3], type='l')

# An autocorrelation plot: nothing there
# acf(all_returns[,3])






print ("Look at the Emerging-market equities (EEM) returns over time")
print ("Annual Standard Deviation: ") 
sd(all_returns[,4])*sqrt(250)

plot(all_returns[,4], type='l')

# An autocorrelation plot: nothing there
# acf(all_returns[,4])


print ("Look at the	Real estate (VNQ) returns over time")
print ("Annual Standard Deviation: ") 
sd(all_returns[,5])*sqrt(250)
plot(all_returns[,5], type='l')

# An autocorrelation plot: nothing there
#acf(all_returns[,5])

print ("The sample correlation matrix")
print (cor(all_returns))





print("Sample a random return from the empirical joint distribution")
print("one day analysis - event weighting")
#"This simulates a random day"
return.today = resample(all_returns, 1, orig.ids=FALSE)
print("random return today =")
print(return.today)

# Update the value of your holdings
# Assumes an equal allocation to each asset
total_wealth = 100000
my_weights = c(0.2,
               0.2,
               0.2, 
               0.2, 
               0.2)
holdings = total_wealth*my_weights
holdings = holdings*(1 + return.today)

# Compute your new total wealth
total_wealth = sum(holdings)
sum(holdings)




#
print("two trading week simulation")
total_wealth_2 = 100000
weights_2 = c(0.2, 
            0.2, 
            0.2, 
            0.2, 
            0.2)
holdings_2 = weights_2 * total_wealth_2
n_days_2 = 10
wealthtracker_2 = rep(0, n_days_2) # Set up a placeholder to track total wealth
for(today in 1:n_days_2) {
  return.today = resample(all_returns, 1, orig.ids=FALSE)
  holdings_2 = holdings_2 + holdings_2*return.today
  total_wealth_2 = sum(holdings_2)
  wealthtracker_2[today] = total_wealth_2
}
print("Total Wealth")
total_wealth_2
plot(wealthtracker_2, type='l')






###################Even Split Portfolio
print ("simulate many different possible scenarios over a one year period")
print ("Even Split Portfolio")
initial_wealth_even = 100000
sim_even = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth_even = initial_wealth_even
  weights_even = c(0.2, 
              0.2, 
              0.2, 
              0.2, 
              0.2)
  holdings_even = weights_even * total_wealth_even 
  n_days_even = 250
  wealthtracker_even= rep(0, n_days_even)
  for(today in 1:n_days_even) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings_even = holdings_even + holdings_even*return.today
    total_wealth_even = sum(holdings_even)
    wealthtracker_even[today] = total_wealth_even
  }
  wealthtracker_even
}

head(sim_even,2)
hist(sim_even[,n_days_even], 25)

# Profit/loss
mean(sim_even[,n_days_even])
hist(sim_even[,n_days_even] - initial_wealth_even, breaks=30)

print(" 5% value at risk for 20 days")
quantile(sim_even[,20], 0.05) - initial_wealth_even
Var_even = initial_wealth_even - quantile(sim_even[,20], 0.05)
print(Var_even)






########aggressive
spy_w_a = .40
tlt_w_a = .05
lqd_w_a = .05
eem_w_a = .30
vnq_w_a = .4

print(cat(paste("Aggressive Split Portfolio", 
         "US domestic equities (SPY: the S&P 500 stock index)", spy_w_a, 
         "US Treasury bonds (TLT)",tlt_w_a,
        "Investment-grade corporate bonds (LQD)",lqd_w_a,
        "Emerging-market equities (EEM)",eem_w_a,
        "Real estate (VNQ)", vnq_w_a
          ,sep="\n")))



initial_wealth_aggressive = 100000
sim_aggressive = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth_aggressive = initial_wealth_aggressive
  weights_aggressive = c(spy_w_a, 
                         tlt_w_a, 
                         lqd_w_a, 
                         eem_w_a, 
                         vnq_w_a)
  holdings_aggressive = weights_aggressive * total_wealth_aggressive 
  n_days_aggressive = 250
  wealthtracker_aggressive= rep(0, n_days_aggressive)
  for(today in 1:n_days_aggressive) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings_aggressive = holdings_aggressive + holdings_aggressive*return.today
    total_wealth_aggressive = sum(holdings_aggressive)
    wealthtracker_aggressive[today] = total_wealth_aggressive
  }
  wealthtracker_aggressive
}

head(sim_aggressive,2)
hist(sim_aggressive[,n_days_aggressive], 25)

# Profit/loss
mean(sim_aggressive[,n_days_aggressive])
hist(sim_aggressive[,n_days_aggressive] - initial_wealth_aggressive, breaks=30)

print(" 5% value at risk for 20 days")
quantile(sim_aggressive[,20], 0.05) - initial_wealth_aggressive
Var_aggressive = initial_wealth_aggressive - quantile(sim_aggressive[,20], 0.05)
print(Var_aggressive)





########conservative
spy_w_c = .05
tlt_w_c = .40
lqd_w_c = .40
eem_w_c = .05
vnq_w_c = .10

print(cat(paste("Conservative Split Portfolio", 
                "US domestic equities (SPY: the S&P 500 stock index)", spy_w_c, 
                "US Treasury bonds (TLT)",tlt_w_c,
                "Investment-grade corporate bonds (LQD)",lqd_w_c,
                "Emerging-market equities (EEM)",eem_w_c,
                "Real estate (VNQ)", vnq_w_c
                ,sep="\n")))

initial_wealth_conservative = 100000
sim_conservative = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth_conservative = initial_wealth_conservative
  weights_conservative = c(spy_w_c, 
                         tlt_w_c, 
                         lqd_w_c, 
                         eem_w_c, 
                         vnq_w_c)
  holdings_conservative = weights_conservative * total_wealth_conservative 
  n_days_conservative = 250
  wealthtracker_conservative= rep(0, n_days_conservative)
  for(today in 1:n_days_conservative) {
    return.today = resample(all_returns, 1, orig.ids=FALSE)
    holdings_conservative = holdings_conservative + holdings_conservative*return.today
    total_wealth_conservative = sum(holdings_conservative)
    wealthtracker_conservative[today] = total_wealth_conservative
  }

}

head(sim_conservative,2)
hist(sim_conservative[,n_days_conservative], 25)
plot(wealthtracker_conservative,type = "l",  main = "Conservative Portfolio")

# Profit/loss
mean(sim_conservative[,n_days_conservative])
hist(sim_conservative[,n_days_conservative] - initial_wealth_conservative, breaks=30)

print(" 5% value at risk for 20 days")
quantile(sim_conservative[,20], 0.05) - initial_wealth_conservative
Var_conservative = initial_wealth_conservative - quantile(sim_conservative[,20], 0.05)
print(Var_conservative)



print("# •marshals appropriate evidence to characterize the risk/return properties of the five major asset classes listed above.")




print('# •	outlines your choice of the ""safe"" and ""aggressive"" portfolios.')
print(cat(paste("Aggressive Split Portfolio", 
                "US domestic equities (SPY: the S&P 500 stock index)", spy_w_a, 
                "US Treasury bonds (TLT)",tlt_w_a,
                "Investment-grade corporate bonds (LQD)",lqd_w_a,
                "Emerging-market equities (EEM)",eem_w_a,
                "Real estate (VNQ)", vnq_w_a
                ,sep="\n")))
print("The aggressive portfolio is heavy biased towards equities. Equity biased portfolios have a higher variance/ standard deviation. ")
print(cat(paste("Conservative Split Portfolio", 
                "US domestic equities (SPY: the S&P 500 stock index)", spy_w_c, 
                "US Treasury bonds (TLT)",tlt_w_c,
                "Investment-grade corporate bonds (LQD)",lqd_w_c,
                "Emerging-market equities (EEM)",eem_w_c,
                "Real estate (VNQ)", vnq_w_c
                ,sep="\n")))
print("The more conservative portfolio is primarily invested in treasuries (considered 'risk free') and Investment grade bonds")

print("# •	uses bootstrap resampling to estimate the 4-week (20 trading day) value at risk of each of your three portfolios at the 5% level.")
print("Aggressive")
print(Var_aggressive)
print()
print("Even Weight")
print(Var_even)
print()
print("Conservative")
print(Var_conservative)
print()
print("# •	compares the results for each portfolio in a way that would allow the reader to make an intelligent decision among the three options.")

print("Aggressive Portfolio")
plot(wealthtracker_aggressive,type = "l",  main = "Aggressive Portfolio")
cat("One year total estimate wealth ",tail(wealthtracker_aggressive,n=1))
cat("20 day VAR at 95% confidence", Var_aggressive)
cat("Projected One Year Performance: ",((tail(wealthtracker_aggressive,n=1))-100000)/100000*100,"%")


print("Even Portfolio")
plot(wealthtracker_even,type = "l",  main = "Even Portfolio")
cat("One year total estimate wealth ",tail(wealthtracker_even,n=1))
cat("20 day VAR at 95% confidence", Var_even)
cat("Projected One Year Performance: ",((tail(wealthtracker_even,n=1))-100000)/100000*100,"%")



print("Conservative Portfolio")
plot(wealthtracker_conservative,type = "l",  main = "Conservative Portfolio")
cat("One year total estimate wealth ",tail(wealthtracker_conservative,n=1))
cat("20 day VAR at 95% confidence", Var_conservative)
cat("Projected One Year Performance: ",((tail(wealthtracker_conservative,n=1))-100000)/100000*100,"%")



###################################
# Market segmentation
# Consider the data in social_marketing.csv. This was data collected in the course of a market-research study using followers of the 
#Twitter account of a large consumer brand that shall remain nameless---let's call it "NutrientH20" just to have a label. 
#The goal here was for NutrientH20 to understand its social-media audience a little bit better, 
#so that it could hone its messaging a little more sharply.
# A bit of background on the data collection: the advertising firm who runs NutrientH20's online-advertising 
#campaigns took a sample of the brand's Twitter followers. They collected every Twitter post ("tweet") by 
#each of those followers over a seven-day period in June 2014. Every post was examined by a human annotator contracted 
#through Amazon's Mechanical Turk service. Each tweet was categorized based on its content using a pre-specified scheme of 
#36 different categories, each representing a broad area of interest (e.g. politics, sports, family, etc.) 
#Annotators were allowed to classify a post as belonging to more than one category. For example, a hypothetical post such as "I'm really excited to see grandpa go wreck shop in his geriatic soccer league this Sunday!" might be categorized as both "family" and "sports." You get the picture.
# Each row of social_marketing.csv represents one user, labeled by a random (anonymous, unique) 9-digit alphanumeric code. 
#Each column represents an interest, which are labeled along the top of the data file. The entries are the number of posts by a given user that fell into the given category. 

#Two interests of note here are "spam" (i.e. unsolicited advertising) and "adult" (posts that are pornographic, salacious, or explicitly sexual). There are a lot of spam and pornography "bots" on Twitter; 
#while these have been filtered out of the data set to some extent, there will certainly be some that slip through. 
#There's also an "uncategorized" label. Annotators were told to use this sparingly, but it's there to capture posts that don't fit at all into any of the listed interest categories. 
#(A lot of annotators may used the "chatter" category for this as well.) Keep in mind as you examine the data that you cannot expect perfect annotations of all posts. Some annotators might have simply been asleep at the wheel some, or even all, of the time! 
#Thus there is some inevitable error and noisiness in the annotation process.
# Your task to is analyze this data as you see fit, and to prepare a (short!) report for NutrientH20 that identifies any 
#interesting market segments that appear to stand out in their social-media audience. 
#You have complete freedom in deciding how to pre-process the data and how to define "market segment." 
#(Is it a group of correlated interests? A cluster? A latent factor? Etc.) 
#Just use the data to come up with some interesting, well-supported insights about the audience.
# 
# social_marketing.csv





## The tm library and related plugins comprise R's most popular text-mining stack.
## See http://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
rm(list=ls())
library(tm) 
library(magrittr)
library(corrplot)





social = read.csv('social_marketing.csv', header=TRUE)

attach(social)

#7882 rows and 37 variables



#cut porn 'bots'
#501 with more than one adult. 
#426 with 3 or more
#cut 3 or more in case of error
social_clean1 = social[social["adult"]<3,]
dim(social_clean1)


#cut spam 'bots'
#cut 2 or more in case of error
social_clean2 = social_clean1[social_clean1["spam"]<2,]
# dim(social_clean2)
# social_clean2
summary(social_clean2)
social_clean3 = social_clean2

str(social_clean3)
colnames(social)

#count total amount in each column so we can plot
sum_columns = data.frame(value_columns = apply(social_clean3[,-1],2,sum))
sum_columns$key = rownames(sum_columns)
sum_columns2 = sum_columns[order(-(sum_columns$value)),]

#plot the correlation chart to see any relationships between variables
cor_social_clean3 = cor(social_clean3[,-1]) 
corrplot(cor_social_clean3)



#bar plot to see the most common types
barplot(sum_columns2$value_columns, col = 2,names.arg = sum_columns2$key,xlab="cagegories")

print (sum_columns2$key)

library(ggplot2)
library(LICORS)  # for kmeans++
library(foreach)
library(mosaic)
library(LICORS)
# Center and scale the data
X = social_clean3[,-(1:2)]
X = scale(X, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

# Run k-means with 6 clusters and 25 starts
clust1 = kmeans(X, 6, nstart=25)

# What are the clusters?
clust1$center[1,]*sigma + mu
clust1$center[2,]*sigma + mu
clust1$center[3,]*sigma + mu
clust1$center[4,]*sigma + mu
clust1$center[5,]*sigma + mu
clust1$center[6,]*sigma + mu

# Which comments are in which clusters?
which(clust1$cluster == 1)
which(clust1$cluster == 2)
which(clust1$cluster == 3)
which(clust1$cluster == 4)
which(clust1$cluster == 5)

# A few plots with cluster membership shown
# qplot is in the ggplot2 library
qplot(current_events, travel, data=social_clean3, color=factor(clust1$cluster))
qplot(food, family, data=social_clean3, color=factor(clust1$cluster))


# Using kmeans++ initialization
clust2 = kmeanspp(X, k=6, nstart=25)

clust2$center[1,]*sigma + mu
clust2$center[2,]*sigma + mu
clust2$center[4,]*sigma + mu

# Which cars are in which clusters?
which(clust2$cluster == 1)
which(clust2$cluster == 2)
which(clust2$cluster == 3)

# Compare versus within-cluster average distances from the first run
clust1$withinss
clust2$withinss
sum(clust1$withinss)
sum(clust2$withinss)
clust1$tot.withinss
clust2$tot.withinss
clust1$betweenss
clust2$betweenss


#Below are the groupings i would have assumed without doing a statistical analysis. I considered grouping these columns into  forced cluster. 

#Arts Grouping: craft, photo_sharing, fashion, art, crafts, beauty, tv_film, music
#Hobbies: sports_fandom, sports_playing, online_gaming, computers, automotive, shopping
#Home: cooking, food, parenting, home_and_garden, travel, outdoors, dating, family, religion
#Health: health_nutrition, personal_fitness
#Business and Education: politics, small_business, eco, college_uni, current_events, news, business, school
#Random: chatter, spam, adult, uncategorized

