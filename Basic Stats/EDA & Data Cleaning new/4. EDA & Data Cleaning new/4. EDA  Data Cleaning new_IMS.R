
data()

data(package = .packages(all.available = TRUE))

# help , type mtcars in serach window
mtcars = mtcars
data("mtcars")
?mtcars
summary(mtcars)
names(mtcars)
str(mtcars)
hist(mtcars$mpg)
summary(mtcars$mpg)
boxplot(mtcars$mpg,col = "red")

plot(mtcars$mpg,mtcars$disp,col = "red")
?plot

boxplot(mtcars$disp~mtcars$gear, col = "red") # col = blue

counts= table (mtcars$vs,mtcars$gear)
counts

?barplot

barplot(counts, main="Distribution",
        xlab = "no of g", col=c("darkblue","red"),
        legend = rownames(counts))
?barplot

barplot(counts, main="Distribution",
        xlab = "no of g", col=c("darkblue","red"))
       

counts= table (mtcars$gear,mtcars$vs)
counts

barplot(counts, main="Distribution",
        xlab = "no of g", col=c("darkblue","red"))



############################
    ################################
        ################################
 '''
Create barplots with the barplot(height) function, where height is a vector or matrix. 
If height is a vector, the values determine the heights of the bars in the plot. If height is 
a matrix and the option beside=FALSE then each bar of the plot corresponds to a column of height,
with the values in the column giving the heights of stacked “sub-bars”. If height is a matrix and 
beside=TRUE, then the values in each column are juxtaposed rather than stacked. Include 
option names.arg=(character vector) to label the bars. The option horiz=TRUE to createa a 
horizontal barplot.

'''


# Simple Bar Plot 
counts <- table(mtcars$gear)
barplot(counts, main="Distribution", 
        xlab="Number of Gears")



# Simple Horizontal Bar Plot with Added Labels 
counts <- table(mtcars$gear)
barplot(counts, main="Distribution", horiz=TRUE,
        names.arg=c("3 Gears", "4 Gears", "5 Gears"))


# Stacked Bar Plot with Colors and Legend
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts))


# Grouped Bar Plot
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


################################################
    ####################################################
      ##########################################################
        #####################################################################

##### Data cleaning understanding 


####################################################
     ########################################################
            #############################################################
                      ##############################################################



library(e1071) # skewness calculation


##### A. Data Loading and object creation

cust_data <- read.csv ("data_cleaning.csv")

 # cust_data <- na.omit(cust_data)
str(cust_data)

 # cust_data <- na.omit(read.csv ("data_cleaning.csv"))

summary(cust_data) # For data visualisation

# cust_data <- na.omit(cust_data) # Delets missing value observations ( Omit this command if possible ) 


# Adding Total revenue variable

cust_data$total_revenue = cust_data$purchase_revenue+ cust_data$purchase_renewal +cust_data$membership_revenue + cust_data$membership_renewal


##### B. Data Visualization

hist(cust_data$duration, prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$duration), sd=sd(cust_data$duration)), add=TRUE)

hist(cust_data$membership_renewal , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$membership_renewal), sd=sd(cust_data$membership_renewal)), add=TRUE)

hist(cust_data$purchase_revenue , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$purchase_revenue), sd=sd(cust_data$purchase_revenue)), add=TRUE)

hist(cust_data$purchase_renewal , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$purchase_renewal), sd=sd(cust_data$purchase_renewal)), add=TRUE)

hist(cust_data$last_1_month_revenue , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$last_1_month_revenue), sd=sd(cust_data$last_1_month_revenue)), add=TRUE)

hist(cust_data$last_3_months_revenue , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$last_3_months_revenue), sd=sd(cust_data$last_3_months_revenue)), add=TRUE)

hist(cust_data$last_3_months_selection , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$last_3_months_selection), sd=sd(cust_data$last_3_months_selection)), add=TRUE)

hist(cust_data$total_revenue , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$total_revenue), sd=sd(cust_data$total_revenue)), add=TRUE)


skewness(cust_data$duration)
skewness(cust_data$membership_renewal)
skewness(cust_data$purchase_revenue)
skewness(cust_data$purchase_renewal)
skewness(cust_data$last_1_month_revenue)
skewness(cust_data$last_3_months_revenue)
skewness(cust_data$last_3_months_selection)
skewness(cust_data$last_3_months_selection)
skewness(cust_data$total_revenue)



##### C. Data Cleaning ( Outlier treatment, and missing value imputation)

names(cust_data) # Verifying the names of variables

 # mean and standard deviation calculation

  y=mean(cust_data$duration)
  s=sd(cust_data$duration)
  
  ucl<- y+3*s
  ucl
  lcl<- y-3*s 
  lcl


# Caping the outliers 

cust_data$duration[cust_data$duration>1691]<- 1691
cust_data$membership_revenue[cust_data$membership_revenue>0.00217]<- 0.00217
cust_data$membership_renewal[cust_data$membership_renewal>45]<- 45
cust_data$purchase_revenue[cust_data$purchase_revenue>13.73]<- 13.73
cust_data$purchase_renewal[cust_data$purchase_renewal>39.83]<- 39.83
cust_data$last_1_month_revenue[cust_data$last_1_month_revenue>17.68]<- 17.68
cust_data$last_1_month_selection[cust_data$last_1_month_selection>1.67]<- 1.67
cust_data$last_3_months_revenue[cust_data$last_3_months_revenue>41.95]<- 41.95
cust_data$last_3_months_selection[cust_data$last_3_months_selection>2.68]<- 2.68
cust_data$total_revenue[cust_data$total_revenue>77.74]<- 77.74

# Imputing missing value with mean

cust_data[which(is.na(cust_data$duration))] <- 368.25
cust_data[which(is.na(cust_data$membership_revenue))] <- 0
cust_data[which(is.na(cust_data$membership_renewal))] <- 13.86
cust_data[which(is.na(cust_data$purchase_revenue))] <- 1.09
cust_data[which(is.na(cust_data$purchase_renewal))] <- 5.07
cust_data[which(is.na(cust_data$last_1_month_revenue))] <- 4.26
cust_data[which(is.na(cust_data$last_3_months_revenue))] <- 10.98
cust_data[which(is.na(cust_data$last_3_months_selection))] <- 0.50
cust_data[which(is.na(cust_data$last_1_month_selection))] <- 0.29
cust_data[which(is.na(cust_data$total_revenue))] <- 20.03


hist(cust_data$duration, prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$duration), sd=sd(cust_data$duration)), add=TRUE)

hist(cust_data$membership_renewal , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$membership_renewal), sd=sd(cust_data$membership_renewal)), add=TRUE)

hist(cust_data$purchase_revenue , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$purchase_revenue), sd=sd(cust_data$purchase_revenue)), add=TRUE)

hist(cust_data$purchase_renewal , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$purchase_renewal), sd=sd(cust_data$purchase_renewal)), add=TRUE)

hist(cust_data$last_1_month_revenue , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$last_1_month_revenue), sd=sd(cust_data$last_1_month_revenue)), add=TRUE)

hist(cust_data$last_3_months_revenue , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$last_3_months_revenue), sd=sd(cust_data$last_3_months_revenue)), add=TRUE)

hist(cust_data$last_3_months_selection , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$last_3_months_selection), sd=sd(cust_data$last_3_months_selection)), add=TRUE)

hist(cust_data$total_revenue , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$total_revenue), sd=sd(cust_data$total_revenue)), add=TRUE)


skewness(cust_data$duration)
skewness(cust_data$membership_renewal)
skewness(cust_data$purchase_revenue)
skewness(cust_data$purchase_renewal)
skewness(cust_data$last_1_month_revenue)
skewness(cust_data$last_3_months_revenue)
skewness(cust_data$last_3_months_selection)
skewness(cust_data$last_3_months_selection)
skewness(cust_data$total_revenue)



cust_data$duration<- log(cust_data$duration+1)
cust_data$membership_renewal<- log(cust_data$membership_renewal +1)
cust_data$purchase_revenue <- log(cust_data$purchase_revenue +1)
cust_data$purchase_renewal <- log(cust_data$purchase_renewal +1)
cust_data$last_1_month_revenue <- log(cust_data$last_1_month_revenue +1)
cust_data$last_3_months_revenue <- log(cust_data$last_3_months_revenue+1)
cust_data$Last_3_months_Selection <- log(cust_data$Last_3_months_Selection +1)

hist(cust_data$duration, prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$duration), sd=sd(cust_data$duration)), add=TRUE)

hist(cust_data$membership_renewal , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$membership_renewal), sd=sd(cust_data$membership_renewal)), add=TRUE)

hist(cust_data$purchase_revenue , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$purchase_revenue), sd=sd(cust_data$purchase_revenue)), add=TRUE)

hist(cust_data$purchase_renewal , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$purchase_renewal), sd=sd(cust_data$purchase_renewal)), add=TRUE)

hist(cust_data$last_1_month_revenue , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$last_1_month_revenue), sd=sd(cust_data$last_1_month_revenue)), add=TRUE)

hist(cust_data$last_3_months_revenue , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$last_3_months_revenue), sd=sd(cust_data$last_3_months_revenue)), add=TRUE)

hist(cust_data$last_3_months_selection , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$last_3_months_selection), sd=sd(cust_data$last_3_months_selection)), add=TRUE)

hist(cust_data$total_revenue , prob=TRUE)
curve(dnorm(x, mean=mean(cust_data$total_revenue), sd=sd(cust_data$total_revenue)), add=TRUE)


skewness(cust_data$duration)
skewness(cust_data$membership_renewal)
skewness(cust_data$purchase_revenue)
skewness(cust_data$purchase_renewal)
skewness(cust_data$last_1_month_revenue)
skewness(cust_data$last_3_months_revenue)
skewness(cust_data$last_3_months_selection)
skewness(cust_data$last_3_months_selection)
skewness(cust_data$total_revenue)

summary(cust_data)


# article on variable transformation
# http://pareonline.net/getvn.asp?v=8&n=6

