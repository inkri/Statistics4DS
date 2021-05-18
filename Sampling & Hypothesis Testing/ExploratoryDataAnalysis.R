##Exploratory Data Analysis
#Introductiono to R

######################Univariate Analysis
#Continuous(boxplot or Histogram) and Categorical(Frequency/Bar Plots).
data("mtcars")
summary(mtcars)
names(mtcars)
hist(mtcars$mpg)
boxplot(mtcars$mpg,col = "red")
attach(mtcars)

##########################Bivariate Analysis 
#Possible Combinations:-
# 1.Continuous & Continuous
# 2.Continuous & Categorical
# 3.Categorical & Categorical

#1.Continuous & Continuous
#Scatter plot (Scatter plot shows the relationship between two variable but  does not indicates the strength of relationship amongst them)
#Correlation (To find the strength of the relationship)(we use Correlation(-1  negative linear correlation to +1 positive linear correlation  and 0 is no correlation))
#what is "Pearson's correlation"
#what is "Spearman's rank correlation"
#what is "kendall's rank correlation"

plot(mtcars$mpg,mtcars$disp,col = "blue")
pairs(mtcars)
cor(mtcars$mpg,mtcars$disp)
cor(mtcars,method="pearson")
cor(mtcars,method="spearman")
cor(mtcars,method="kendall")
cor.test(mtcars$mpg,mtcars$disp)
cor.test(mtcars$mpg,mtcars$disp,method="pearson")
cor.test(mtcars$mpg,mtcars$disp,method="spearman")
cor.test(mtcars$mpg,mtcars$disp,method="kendall")
cor.test(mtcars$mpg,mtcars$disp,method="kendall",exact=F)
cor.test(mtcars$mpg,mtcars$disp,method="pearson",alt="greater",conf.level=0.99)
cor(mtcars)
#Corvariance
cov(mtcars)

# 2.Continuous & Categorical
#Boxplot(Plot the categorical variable on the x axis and the continuous  variable on the y axis)
boxplot(mtcars$disp~mtcars$gear, col = "red") # col = blue

# 3.Categorical & Categorical
# Ways are:
#1. Two-way table
#2. Stacked Column Chart
#3. Chi-Square Test

#1. Two-way table:
counts= table (mtcars$vs,mtcars$gear)
counts

#2. Stacked Column Chart:
barplot(counts, main="Distribution",xlab = "no of g", col=c("darkblue","red"),legend = rownames(counts))

#3. Chi-Square Test:
data=c(40,25,19,37,39) #No. of students registered for the classes C1,C2,C3,C4,C5
#H0: p1=p2=p3=p4=p5
#Ha: p1!=p2!=p3!=p4!=p5
chisq.test(data)  #p-value = 0.02519
#Critical value from table 9.488(df=4,aplha=0.05)
#if alpha is .05, we can reject the H0
#if alpha is .025, we cannot reject the H0
data1=c(35,31,38,27,29)
chisq.test(data1)   #p-value = 0.6446, Cannot reject H0

######################Missing Value Treatment(NA)
a=c(1,2,3,4,5,6,7,8,9,NA,11,NA)
class(NA)
a
is.na(a)       #Gives True for missing Value
a[is.na(a)]    #Only NA comes
!is.na(a)   
a[!is.na(a)]   #Gives non NA values
na.omit(a)
#attributes
b=na.omit(a)
b
attributes(b)
attr(b,"na.action")
c=attr(b,"na.action")
c
attr(c,"class")

##########################Outliers
data=c(sample(x=1:20,size=40,replace=TRUE),65,80)
data 
summary(data)
boxplot(data)
#Discarding the outlier from dataset.
data1=data
length(data1)
class(data1)
bench=17 + 1.5*IQR(data1)
bench   #Will be discarding data more than 33.125
data1[data1>33.125]
data1[data<33.125]
data1=data1[data1<bench]
data1
length(data1)
summary(data1)
boxplot(data1)

##############################Variable/Data transformation
require(nlme)
Orthodont$age
Orthodont$age - 11
require(MASS)
quine[1:10,]
quine$Days
quine$Days + 1
log(quine$Days + 1)
log10(quine$Days + 1)
require(ggplot2)
diamonds[1:10,]
diamonds[1:30,c("x","y","z")]
scale(diamonds[1:30,c("x","y","z")])
