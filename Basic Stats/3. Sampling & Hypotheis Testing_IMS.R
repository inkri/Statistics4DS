data() # get information of all the available data 
data(package = .packages(all.available = TRUE))
#install.packages("MASS")
library(MASS)
survey <- survey
??survey
summary(survey)
str(survey)
survey <- na.omit(survey)

summary(survey)
names(survey)

?survey
height.survey= survey$Height
      # summary function
mean(height.survey)

#mean(height.survey, na.rm = TRUE)


#mean(height.survey)
#mean(height.survey, na.rm = FALSE)

''' or

library(MASS)
# The function na.omit() returns the object with listwise deletion of missing values
height.response = na.omit(survey$Height)
mean(height.response)

mean(height.response, na.rm = TRUE)
'''

# Interval Estimate of Population Mean-Known Variance



n = length(height.survey)
sigma = 9.48    # Population standard deviation (Given/known) in PPT
SEmean = sigma/sqrt(n)  # Population error of mean
SEmean

# Interval Estimate of Population Mean-Known Variance
 # value at the p percentile of normal distribution  qnorm(.9) is 1.28 # 90th percentile

z = qnorm(0.975)
z
z = qnorm(0.025)
z


E = qnorm(0.975)*SEmean
E   # margin of error

xbar = mean (height.survey) # sample mean
xbar + c(-E,E)

# Interval Estimate of Population Mean  -  Unknown Variance
library(MASS)

height.response = na.omit(survey$Height)
n= length(survey$Height)
s = sd(survey$Height) # sample standard deviation 
SE = s/sqrt(n)          # standard error estimate
SE

# Interval Estimate of Population Mean-Unknown Variance

 # E = qt(0.975, 400) # one tail t test of 400 DF and 97.5% ( check with table)

qt(0.975, n-1)
E = qt(0.975, n-1)*SE # margin of error for two tail test at 95% confidence
E


xbar = mean(height.response) # sample mean
xbar+ c(-E,E)
# [1] 170.9593 173.9933


####### R code
# roll a die
sample(1:6,10,replace = TRUE) # putting back for throw

# [1] 1 5 2 5 5 2 6 4 5 6 # any random number may come
sample(1:6,4,replace = FALSE)
sample(1:6,10,replace = FALSE) # some error will come ( can't take 10 elements from 6 elements without replacement)

# Toss a coin
sample(c("H","T"),10,replace=TRUE)
# [1] "H" "T" "T" "H" "H" "T" "H" "H" "H" "T"

# pick 6 of 54 ( a lottery)
sample(1:54,40)
sample(1:54,40, replace=TRUE)
 

# pick a card

##### experiment

xx <- c(2:10)
x <- c("A",2:10,"J","Q","K")

# repeat function
?rep
y <- rep(c("A",2:10,"J","Q","K"),4)

rep(1:4, 2)
rep(1:4, each = 2)       # not the same.
rep(1:4, c(2,2,2,4))     # same as second.
rep(1:4, c(2,1,2,1))     # ununiform repeatation
rep(1:4, each = 2, len = 4)    # first 4 only.
rep(1:4, each = 2, len = 10)   # 8 integers plus two recycled 1's.
rep(1:4, each = 2, times = 3)  # length 24, 3 complete replications

?rep

z <- c("H","D","S","C")
cards1= paste(c("A",2:10,"J","Q","K"),c("H","D","S","C"))


cards= paste(rep(c("A",2:10,"J","Q","K"),4),c("H","D","S","C"))
sample(cards,5) # a pair of 5 without replacement

help(outer)

# Roll 2 Dice


# The outer product of the arrays X and Y is an  array 

 ### Experiment
'''
x <- 1:9
x

y <- 2:8
y
nnn <- outer(y, x, "*") # outer product multiplication
TTT <- outer(y, x, "^") # 8**9
abc <- outer(y, x, paste)

'''

dice2 = outer(1:6,1:6,paste)

sample(dice2,5,replace= TRUE)

dice3 <- as.data.frame(dice2) # run dice2 and dice3 and compare ( dataframe with matrix)
sample(dice3,5,replace= TRUE) # it is not suggested to pick sample from dataframe

dice = as.vector(outer(1:6,1:6,paste))
sample(dice,5,replace= TRUE)



# Lower Tail Test of Population Mean-Known Variance
'''
The null hypothesis is that μ GE 10000. We begin with computing the test statistic.
'''
xbar = 9900 # sample mean
mu0 = 10000 # Hypothesized value
sigma = 120 # Population standard deviation
n = 30      # sample size
z= (xbar-mu0)/(sigma/sqrt(n))
z           # Test statistics

# we then compute the critical value at .05 significance level
alpha = 0.05
z.alpha = qnorm(1-alpha)
-z.alpha    # critical value multiply withe negative for lower tail
# Interpreting Output

# Instead of using the critical value, we apply the pnorm function to  compute the 
  #lower tail p-value of the test statistic
 # As it turns out to be  less than the .05 significance level, we reject the null hypothesis that μ Š 10000

pval = pnorm(z)
pval
[1] 2.505166e-06

#  Upper Tail Test of Population Mean-Known Variance
'''
The null hypothesis is that μ LE􀑇 2. We begin with computing the test
statistic.
'''
xbar = 2.1 # sample mean
mu0 = 2    # Hypothesized value
sigma = 0.25 # Pupulation standard deviation
n = 35      # sample size
z = (xbar-mu0)/(sigma/sqrt(n))
z            # test statistics
# [1] 2.366432
# We then compute the critical value at .05 significance level.
alpha = 0.05
z.alpha = qnorm(1- alpha)
z.alpha
# [1] 1.644854

# Interpreting Output

''' Instead of using the critical value, we apply the pnorm function to  compute the upper tail 
p-value of the test statistic. As it turns out to be  less than the .05 significance level, 
we reject the null hypothesis that μ Š 2.
'''

pval = pnorm(z, lower.tail= FALSE)
pval
# [1] 0.008980239
## Two-Tailed Test of Population Mean-Known Variance
# The null hypothesis is that μ = 15.4. We begin with computing the test statistic.

xbar = 14.6       # sample mean
mu0 = 15.4        # hypothesized value
sigma = 2.5       # Population standard deviation
n = 35            # sample size
z = (xbar-mu0)/(sigma/sqrt(n))
z                 # Test statistics
# [1] -1.893146
# We then compute the critical values at .05 significance level.

alpha = 0.05
z.half.alpha= qnorm(1-alpha/2)
c(-z.half.alpha, z.half.alpha)
# [1] -1.959964  1.959964
# Interpreting Output
pval = 2*pnorm(z) # lower tail
pval              # two tailed p value 

## Lower Tail Test of Population Mean-Unknown Variance
pval = 2*pnorm(z)
pval
# [1] 0.05833852




# The null hypothesis is that μ GE 10000. We begin with computing the test statistic

xbar = 9900      # sample mean
mu0 = 10000      # hypothesized value 
s = 125          # sample standard deviation
n = 30           # sample size
t= (xbar-mu0)/(s/sqrt(n))
t                # test statistics
# [1] -4.38178
# We then compute the critical value at .05 significance level.

alpha = 0.05
t.alpha = qt(1- alpha, df = n-1)
- t.alpha    # critocal value 
# [1] -1.699127 # reject the calim that bulb life is above 1000 hrs

# Interpreting Output
pval = pt(t,df = n-1)
pval
# [1] 7.035026e-05

## Lower Tail Test of Population Proportion
'''
The null hypothesis is that p GE 0.6. We begin with computing the test
statistic.
'''
pbar = 85/148 # sample proportion
p0 = 0.6  # hypothesized value 
n = 148

z=(pbar-p0)/(sqrt(p0*(1-p0)/n))
z=(pbar-p0)/sqrt(n*p0*(1-p0))

z
# [1] -0.6375983

# We then compute the critical value at .05 significance level.
alpha = 0.05
z.alpha = qnorm(1-alpha)
-z.alpha
# [1] -1.644854 # , we do not reject the null hypothesis  that the proportion of voters in the population is above 60% this year

## Interpreting Output
pval = pnorm(z)  # lower tail p value 
pval
# [1] 0.2618676


T

