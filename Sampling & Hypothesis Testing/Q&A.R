#Q&A
#http://www.r-tutor.com/elementary-statistics/qualitative-data/pie-chart

######################################################################################
##################ELEMENTARY STATISTICS WITH R########################################

##Qualitative Data:

#Frequency Distribution of Qualitative Data:
library(MASS)                 # load the MASS package 
school = painters$School      # the painter schools 
school.freq = table(school)   # apply the table function
school.freq 
cbind(school.freq) 
attach(painters)
View(painters)
summary(painters)

#Relative Frequency Distribution of Qualitative Data:
library(MASS)                 # load the MASS package 
school = painters$School      # the painter schools 
school.freq = table(school)   # apply the table function
school.relfreq = school.freq / nrow(painters)    #sample size of painters with the nrow function
school.relfreq 

old = options(digits=1)
school.relfreq 
old = options(digits=1)
cbind(school.relfreq) 

#Bar Graph:
library(MASS)                 # load the MASS package 
school = painters$School      # the painter schools 
school.freq = table(school)   # apply the table function
barplot(school.freq)         # apply the barplot function

colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan") 
barplot(school.freq,col=colors)   # set the color palette  # apply the barplot function

#Pie Chart:
library(MASS)                 # load the MASS package 
school = painters$School      # the painter schools 
school.freq = table(school)   # apply the table function
pie(school.freq)              # apply the pie function

colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan") 
pie(school.freq,col=colors)  # apply the pie function # set the color palette

#Category Statistics:
library(MASS)                 # load the MASS package 
school = painters$School      # the painter schools 
c_school = school == "C"      # the logical index vector
c_painters = painters[c_school, ]  # child data set
mean(c_painters$Composition) 

tapply(painters$Composition, painters$School, mean) 

##Quantitative Data:
#Frequency Distribution of Quantitative Data:

