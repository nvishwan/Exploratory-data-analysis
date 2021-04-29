---
title: "ChickWeight"
author: "Group2 (Akshita, Khalid, Nilesh)"
date: "31/01/2020"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, fig.height=3, fig.width=8)
library(tidyverse)
library('data.table')
library('Rmisc')
library('ggpubr')
library('Hmisc')
library(RColorBrewer)
```

# 1. Understanding the Data

This **ChickWeight** dataset describes the body weights of the chicks measured at birth and every second day thereafter until day 20. They were also measured on day 21. **The purpose of the report is to examine the effect of 4 diets on early growth of chicks.**. The dataset has **578 rows and 4 columns**, having **weight**, **Time**, **Chick** and **Diet** as four variables with no missing values (checked using *is.na(ChickWeight)* function). 


**Structure of ChickWeight Dataset:**

Firstly, we will describe the general structure of ChickWeight Dataset describing the variables (their datatypes and levels)

```{r echo=FALSE}
summary(ChickWeight)

```
Using *str(ChickWeight)* function, we get the following details:\
**Weight**\
*datatype:* numeric \
*description:* weight of the chicken\
**Time**\
*datatype:* numeric \
*description:* the day weight was recorded beginning with day 0 as the day it was born\
**Chick**\
*datatype:* an ordered factor with levels 50 \
*description:* a unique identifier for the chick. For instance, the first row in chick variable shows that the chick with unique ID# 13 has the lowest weight, followed by chick ID 9, 20 and so on.\
**Diet**\
*datatype:* factor with levels 1, 2, 3, 4 \
*description:* indicates which experimental diet the chicks receive

# 2. Analyzing the Data

**Summary of the ChickWeight dataset:**

Here, we will focus on **Weight, Time, Chick and Diet - giving a sense of their central tendency, dispersion, range, levels, etc.**
There are 2 numeric variables (weight and Time) for which the measure of central tendency, dispersion and range are calculated.

**Measures of central tendency**
```{r echo=FALSE}
#THE MEASURE OF CENTRAL TENDENCY CODE:
#install.packages("data.table")

central_tendency_mean<-function(x){
 return(round(mean(x),digits=3))
  }
central_tendency_median<-function(x){
  return(median(x))
}
central_tendency_mode<-function(x){
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
  paste(ux[tab == max(tab)],collapse=" ")
}
ct<-function(x,y){
  cDT = data.table(
  Measure = c("Mean","Median","Mode"),
  "|" = c("|","|","|"),
  Weight = c(central_tendency_mean(x), central_tendency_median(x), central_tendency_mode(x)),
  "|" = c("|","|","|"),
  Time = c(central_tendency_mean(y), central_tendency_median(y),central_tendency_mode(y))
  )
  return(cDT)
}

ct(ChickWeight$weight, ChickWeight$Time)
```
**Measures of Dispersion**
```{r echo=FALSE}
#THE MEASURE OF DISPERSION CODE:
#install.packages("data.table")
#library('data.table')
dispersion_variance<-function(x){
  return(round(var(x),digits=3))
}
dispersion_sd<-function(x){
  return(round(sd(x),digits=3))
}

disper<-function(x,y){
  dDT = data.table(
  Measure = c("Variance","Standard Deviation","Inter-Quartile Range (IQR)", "Range"),
  "|" = c("|","|","|","|"),
  Weight = c(dispersion_variance(x), dispersion_sd(x), IQR(x), paste(range(x),collapse=" ")),
  "|" = c("|","|","|","|"),
  Time = c(dispersion_variance(y), dispersion_sd(y), IQR(y), paste(range(y),collapse=" "))
)
  return(dDT)
}

disper(ChickWeight$weight, ChickWeight$Time)


```

**Summarizing the above measures grouped by diet** 

```{r echo=FALSE}
#ggplot(ChickWeight, aes(x=Diet,y=weight,fill=Diet)) + geom_boxplot(outlier.size=2)

```
Average weights for each diet is shown in the following graph:
```{r echo=FALSE}
individual_mean<-aggregate( weight ~ Diet, ChickWeight, mean )
#individual_mean

```
```{r echo=FALSE}


ggplot(data=individual_mean, aes(x=Diet,y=weight))+geom_bar(stat="identity",fill= brewer.pal(length(unique(individual_mean$Diet)), "Set3"), width=0.5)+
  
geom_text(aes(label=round(weight,2)), vjust=1.6, color="black", size=3.5)+ 
  theme_light()


```
From the above graph, the average weight of *Diet 3 > Diet 4 > Diet 2 > Diet 1*. In order to understand the distribution, skewness, and general idea of mean and median, the following graphs show the *overall weight vs Diet*, and *individual weight vs Diet*.

```{r echo=FALSE}
#SD of the distribution 
#ChickWeight %>% 
  #group_by(Diet) %>% 
  #summarise(sd = sd(weight))

m<-mean(ChickWeight$weight);std<-sqrt(var(ChickWeight$weight))
hist(ChickWeight$weight,prob=T,main="Weight",xlab="Weight")
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE)
abline(v = mean(ChickWeight$weight),
 col = "royalblue",
 lwd = 2)
abline(v = median(ChickWeight$weight),
 col = "red",
 lwd = 2)

legend(x = "topright", # location of legend within plot area
 c("Density plot", "Mean", "Median"),
 col = c("darkblue", "royalblue", "red"),
 lwd = c(2, 2, 2))
```
It is observed that the distribution is rightly skewed, as Mean > Median.

```{r echo=FALSE}
par(mfcol=c(1,4))
#SD of the distribution 
sd1<-filter(ChickWeight,Diet==1) 

m<-mean(sd1$weight);std<-sqrt(var(sd1$weight))
hist(sd1$weight,prob=T,main="Weight for Diet 1",xlab="Weight")
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE)
abline(v = mean(sd1$weight),
 col = "royalblue",
 lwd = 2)
abline(v = median(sd1$weight),
 col = "red",
 lwd = 2)

#SD of the distribution 
sd2<-filter(ChickWeight,Diet==2) 

m2<-mean(sd2$weight);std2<-sqrt(var(sd2$weight))
hist(sd2$weight,prob=T,main="Weight for Diet 2",xlab="Weight")
curve(dnorm(x, mean=m2, sd=std2), col="darkblue", lwd=2, add=TRUE)
abline(v = mean(sd2$weight),
 col = "royalblue",
 lwd = 2)
abline(v = median(sd2$weight),
 col = "red",
 lwd = 2)

#SD of the distribution 
sd3<-filter(ChickWeight,Diet==3) 

m3<-mean(sd3$weight);std3<-sqrt(var(sd3$weight))
hist(sd3$weight,prob=T,main="Weight for Diet 3",xlab="Weight")
curve(dnorm(x, mean=m3, sd=std3), col="darkblue", lwd=2, add=TRUE)
abline(v = mean(sd3$weight),
 col = "royalblue",
 lwd = 2)
abline(v = median(sd3$weight),
 col = "red",
 lwd = 2)


#SD of the distribution 
sd4<-filter(ChickWeight,Diet==3) 

m4<-mean(sd4$weight);std4<-sqrt(var(sd4$weight))
hist(sd4$weight,prob=T,main="Weight for Diet 4",xlab="Weight")
curve(dnorm(x, mean=m4, sd=std4), col="darkblue", lwd=2, add=TRUE)
abline(v = mean(sd4$weight),
 col = "royalblue",
 lwd = 2)
abline(v = median(sd4$weight),
 col = "red",
 lwd = 2)


#legend(x = "topright", # location of legend within plot area

# c("Density plot", "Mean", "Median"),
#col = c("chocolate3", "royalblue", "red"),
#lwd = c(2, 2, 2))

```

Overall Co-relation for Weight~Time for the given dataset

```{r echo = FALSE}
cor(ChickWeight$Time,ChickWeight$weight)
```

**Confidence intervals for mean weight at day 20 for each diet** \
*This is the analysis for part b of Assignment 1*\
Here we are anlaysing the mean weight observed with each of the 4 diets with 95% confidence intervals to further confirm the weight~Diet relationship that was observed above. 

```{r echo=FALSE}
cw20<-filter(ChickWeight, Time == 20)
grp1<- cw20 %>% filter(Diet==1)
grp2<- cw20 %>% filter(Diet==2)
grp3<- cw20 %>% filter(Diet==3)
grp4<- cw20 %>% filter(Diet==4)

confidence_interval <- function(x1, x2, x3, x4){
m1 <- mean(x1)
s1 <- sd(x1)
n1 <- length(x1)
error <- qt(0.975,df=n1-1)*s1/sqrt(n1)
L1 <- m1-error
R1 <- m1+error

m2 <- mean(x2)
s2 <- sd(x2)
n2 <- length(x2)
error <- qt(0.975,df=n2-1)*s2/sqrt(n2)
L2 <- m2-error
R2 <- m2+error

m3 <- mean(x3)
s3 <- sd(x3)
n3 <- length(x3)
error <- qt(0.975,df=n3-1)*s3/sqrt(n3)
L3 <- m3-error
R3 <- m3+error

m4 <- mean(x4)
s4 <- sd(x4)
n4 <- length(x4)
error <- qt(0.975,df=n4-1)*s4/sqrt(n4)
L4 <- m4-error
R4 <- m4+error
    
dDT = data.table(
  Diets = c("Diet 1","Diet 2","Diet 3", "Diet 4"),
  "|" = c("|","|","|","|"),
  Sample_size = c(n1,n2, n3,n4),
  "|" = c("|","|","|","|"),
  Avg_Weight = c(m1,m2, m3,m4),
  "|" = c("|","|","|","|"),
  Lower_Value  = c(L1,L2, L3,L4),
  "|" = c("|","|","|","|"),
  Upper_Value = c(R1,R2, R3,R4),
  "|" = c("|","|","|","|"),
  Range_of_CI = c(R1-L1,R2-L2, R3-L3,R4-L4)
)
  return(dDT)

}
confidence_interval(grp1$weight, grp2$weight, grp3$weight, grp4$weight)

```


```{r echo=FALSE}

#install.packages("Hmisc")

stat_sum_cw <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data = fun, colour = "red", geom = geom, width = 0.2, ...)
}

d<-filter(ChickWeight, Time==20)
d1 <- ggplot(d, aes(Diet,weight)) + geom_point()
d1 + stat_sum_cw("mean_cl_boot", mapping = aes(group = Diet)) +stat_summary(fun.y = "mean", colour = "blue", size = 3, geom = "point")



```
The above graph depicts confidence intervals for mean weight at day 20 for each diet. It is observed that the Range of CI for Diet 2 > Diet 3 > Diet 4 ~ Diet 1. However, the average weight of Diet 3 > Diet 4 > Diet 2 > Diet 1.




**General trend of Weight observed with each diet over a period of 21 days (using GGplot and Face grid functions)**

```{r echo=FALSE}
cw2<-ggplot(ChickWeight,aes(Time,weight))
cw2+geom_point(color='red')+geom_smooth(method="lm")+facet_grid(.~Diet)

```
We observe that there is an increase in weight for all four Diets over 21 days, with *highest increase for Diet 3* (as we can see that it has the *maximum positive slope*).


# 3. Conclusion

**Conclusion** \

1. The average weights for all four Diets are positively correlated with Time i.e. there is strong linear relationship and both the variables move in the same direction, but there is maximum correlation for Diet 3 and minimum for Diet 1 w.r.t time for 20 days period. \

2. The average weight of *Diet 3 > Diet 4 > Diet 2 > Diet 1* \

3. There is an increase in weight for all four Diets over 21 days, with *highest increase for Diet 3* \

**Future Scope** \

1. Overall co-relation for Weight~Time for the given dataset indicates a strong linear relationship i.e  both the variables are positively corelated with one another, so if we have data for more days and if we go beyond 20 days, the weight of the chicken will further increase in a linearly. \


