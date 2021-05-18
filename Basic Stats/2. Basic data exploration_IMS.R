faithful <- faithful
data(faithful)

faithful
?lm

?faithful # Data information
names(faithful)
str(faithful)

duration = faithful$eruptions
duration
range(duration)
breaks<-seq(1.5 , 5.5,0.5)
breaks

# Classify the eruption durations according to the half-unit-length sub-intervals with cut. As the 
  # intervals are to be closed on the left, and open on the right, we set the right argument as FALSE.

duration.cut <- cut(duration,breaks, right = FALSE)
# duration.cut <- cut(duration,breaks, right = TRUE)
# duration.cut <- cut(duration,breaks, left = FALSE)
# duration.cut <- cut(duration,breaks, left = TRUE)

table(duration.cut)
duration.freq <- cbind(table(duration.cut))
duration.freq


# Histogram in R


salary=c(3519,3803,4332,4251,4661,4811,4448,4451,4343,4067,4001,3934,3652,3768
    ,4082,4101,4628,4898,4476,4728,4458,4004,4095,4056,3641,3966,4417,4367
    ,4821,5190,4638,4904,4528,4383,4339,4327,3856,4072,4563,4561,4984,5316)


hist(salary)



x <- seq(200,1200,200)
width = 200
frequency = c(6,16,24,20,10,4)
lb = x- width/2  # lower limit
ub = x + width/2# upper limit
brks = c(lb[1],ub)


y = rep(x,frequency)

?rep # replicate

hist(y, main="First Histogram" , breaks= brks , xlab = "Monthly house rent",
     ylab="number of families")
hist(y, breaks= brks)
hist(y)


?hist

hist(y , breaks= brks , xlab = "Monthly house rent",
     ylab="number of families" , main="Histogram")

###### OR ######




# Frequency Polygon in R

x <- seq(200,1200, by= 200)
frequency = c(6,16,24,20,10,4)
x1 = c(0,x,1400)
f1 = c(0,frequency,0)

plot(x1,f1)

plot(x1,f1, "c")
plot(x1,f1, "b")
?plot

plot(x1,f1,"c",xlab = "Monthly House rent",
     ylab = "number of families", main = " frequency Polygon")

plot(x1,f1,"b",xlab = "Monthly House rent",
     ylab = "number of families", main = " frequency Polygon")


?plot

'''
"p" for points,
"l" for lines,
"b" for both,
"c" for the lines part alone of "b",
"o" for both ‘overplotted’,

'''


# Box plot in R

x = c(68,44,55,47,65,50,72,54,75,60,48,60,42,
      60,56,65,45,55,65,44)
x
boxplot(x)
boxplot(x,ylab = "Marks")
f= fivenum(x)
text(rep(1.4,5),f,labels=c("Minimum","Lower quartile","Median","Upper quartile",
                             "maximum"))



trees <- trees

names(trees)
Volume = trees$Volume

# Then we compute the sample standard deviation

n= length(Volume)
s = sd(Volume) # sample standard deviation 
SE = s/sqrt(n)          # standard error estimate
SE
E = qt(0.975, n-1) # margin of error for two tail test at 95% confidence
E

xbar = mean(Volume) # sample mean
xbar+ c(-E,E)




