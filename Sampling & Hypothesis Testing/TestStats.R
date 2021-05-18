library(MASS)
height.survey= survey$Height
mean(height.survey)
is.na(height.survey)
mean(height.survey, na.rm=TRUE)

library(MASS)
height.response = na.omit(survey$Height)
mean(height.response)
n=length(height.response)
n
sigma=9.48
SEmean=sigma/sqrt(n)
SEmean
E=qnorm(0.975)*SEmean
E
xbar=mean(height.response)
xbar+ c(-E,E)


library(MASS)
height.response = na.omit(survey$Height)

# Then we compute the sample standard deviation

n= length(height.response)
s = sd(height.response) # sample standard deviation 
SE = s/sqrt(n)          # standard error estimate
SE
e=qt(0.975,n-1)
e
xbar=mean(survey$Height,na.rm=TRUE)
xbar+c(-e,e)



xbar = 9900 # sample mean
mu0 = 10000 # Hypothesized value
sigma = 120 # Population standard deviation
n = 30      # sample size
z= (xbar-mu0)/(sigma/sqrt(n))
z

alpha = 0.05
z.alpha = qnorm(1-alpha)
-z.alpha    # multiply withe negative for lower tail

pval = pnorm(z)
pval


xbar = 2.1 # sample mean
mu0 = 2    # Hypothesized value
sigma = 0.25 # Population standard deviation
n = 35      # sample size
z = (xbar-mu0)/(sigma/sqrt(n))
z            # test statistics

alpha = 0.05
z.alpha = qnorm(1- alpha)
z.alpha

pval = pnorm(z, lower.tail= FALSE)
pval


xbar = 14.6       # sample mean
mu0 = 15.4        # hypothesized value
sigma = 2.5       # Population standard deviation
n = 35            # sample size
z = (xbar-mu0)/(sigma/sqrt(n))
z                 # Test statistics

alpha = 0.05
z.half.alpha= qnorm(1-alpha/2)
c(-z.half.alpha, z.half.alpha)

pval = 2*pnorm(z)
pval


xbar = 9900      # sample mean
mu0 = 10000      # hypothesized value 
s = 125          # sample standard deviation
n = 30           # sample size
t= (xbar-mu0)/(s/sqrt(n))
t                # test statistics

alpha = 0.05
t.alpha = qt(1- alpha,n-1)
- t.alpha    # critical value 

pval = pt(t,df = n-1)
pval


pbar = 85/148 # sample proportion
p0 = 0.6  # hypothesized value 
n = 148 # sample size
z=(pbar-p0)/(sqrt(p0*(1-p0)/n))
z

alpha = 0.05
z.alpha = qnorm(1-alpha)
-z.alpha

pval = pnorm(z)  # lower tail p value 
pval


data("mtcars")
summary(mtcars)
names(mtcars)
hist(mtcars$mpg)
boxplot(mtcars$mpg,col = "red")

plot(mtcars$mpg,mtcars$disp,col = "blue")
boxplot(mtcars$disp~mtcars$gear, col = "red") # col = blue
counts= table (mtcars$vs,mtcars$gear)
counts
barplot(counts, main="Distribution", xlab = "no of g", col=c("darkblue","red"),legend = rownames(counts))


