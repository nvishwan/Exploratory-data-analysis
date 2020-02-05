---
title: "Exploratory Data Analysis"
author: "Group2 (Akshita, Khalid, Nilesh)"
date: "02/02/2020"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, fig.height=3, fig.width=8)
library(tidyverse)
library('data.table')
library('Rmisc')
library('Hmisc')
library(readr)
library(GGally) 
library(cowplot)
```

# 1. Understanding the Data

This **diamonds** dataset describes the prices and other variables such as the clarity of diamonds, their depth, size, color etc. **The purpose of the report is to explore the dataset, analyse the trends and record suggestions to improve the data**. 
The dataset has **53940 rows and 10 columns**, with no missing values (checked using *sum(is.na(diamonds))* function).\


**Structure of Diamond Dataset:**

Firstly, we will describe the general structure of diamonds Dataset describing the variables (their datatypes and levels)

**Structure**
```{r echo=FALSE}
str(diamonds)

```
Using *str(diamonds)* function, we get the datatype and structural details. \
**Description**
The *description* of variables are as shown below:\
**carat:** weight of the diamond (0.2–5.01)\
**cut:** quality of the cut with 5 factors: Fair, Good, Very Good, Premium, Ideal\
**color:** diamond colour (D,E,F,G,H,I,J), from D (best) to J (worst)\
**clarity:** measure of clarity of diamond (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))\
**depth:** total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43–79)\
**table:** width of top of diamond relative to widest point (43–95)\
**price:** price in US dollars (\$326–\$18,823)\
**x:** length in mm (0–10.74)\
**y:** width in mm (0–58.9)\
**z:** depth in mm (0–31.8)\

# 2. Analyzing the data

**Summary of the diamonds dataset:**

Here, we will focus on numeric variables **carat, depth, table, price, x, y and z - giving a sense of their central tendency, dispersion, range, levels, etc.**
There are 7 numeric variables (carat, depth, table, price, x, y and z) for which the measure of central tendency, dispersion and range are calculated. Following is the summary of the Factor variables, followed by the numeric variables:

```{r echo=FALSE}
diamonds$cut<-as.factor(diamonds$cut)
diamonds$color<-as.factor(diamonds$color)
diamonds$clarity<-as.factor(diamonds$clarity)
diamonds_df <- data.frame(diamonds$carat, diamonds$cut, diamonds$color)
names(diamonds_df) <- c("Carat", "Cut", "Color")
summary(diamonds_df)

```

**Measures of central tendency**
```{r echo=FALSE}
#THE MEASURE OF CENTRAL TENDENCY CODE:

central_tendency_mean<-function(a){
 return(round(mean(a),digits=3))
  }
central_tendency_median<-function(a){
  return(median(a))
}
central_tendency_mode<-function(a){
  ux <- unique(a)
  tab <- tabulate(match(a, ux))
  ux[tab == max(tab)]
  paste(ux[tab == max(tab)],collapse=" ")
}
ct<-function(d,e,f,g,a,b,c){
  cDT = data.table(
  Measure = c("Min", "1st Qu.", "-" ,"Mean","Median","Mode", "-" ,"3rd Qu.",  "Max"),
  "|" = c("|","|", " " ,"|","|","|", " " ,"|","|"),
  carat= c( min(d), quantile(d, 0.25) , " " ,central_tendency_mean(d), central_tendency_median(d), central_tendency_mode(d),  " " ,quantile(d,0.75) , max(d)),
  "|" = c("|","|", " " ,"|","|","|", " " ,"|","|"),
  depth= c( min(e), quantile(e, 0.25) , " " ,central_tendency_mean(e), central_tendency_median(e), central_tendency_mode(e),  " " ,quantile(e,0.75) , max(e)),
  "|" = c("|","|", " " ,"|","|","|", " " ,"|","|"),
  table= c( min(f), quantile(f, 0.25) , " " ,central_tendency_mean(f), central_tendency_median(f), central_tendency_mode(f),  " " ,quantile(f,0.75) , max(f)),
  "|" = c("|","|", " " ,"|","|","|", " " ,"|","|"),
  price= c( min(g), quantile(g, 0.25) , " " ,central_tendency_mean(g), central_tendency_median(g), central_tendency_mode(g),  " " ,quantile(g,0.75) , max(g)),
  "|" = c("|","|", " " ,"|","|","|", " " ,"|","|"),
  x= c( min(a), quantile(a, 0.25) , " " ,central_tendency_mean(a), central_tendency_median(a), central_tendency_mode(a),  " " ,quantile(a,0.75) , max(a)),
  "|" = c("|","|", " " ,"|","|","|", " " ,"|","|"),
  y = c( min(b), quantile(b, 0.25) , " " ,central_tendency_mean(b), central_tendency_median(b),central_tendency_mode(b),  " " ,quantile(b,0.75) , max(b)),
  "|" = c("|","|", " " ,"|","|","|", " " ,"|","|"),
  z = c( min(c), quantile(c, 0.25) , " " ,central_tendency_mean(c), central_tendency_median(c),central_tendency_mode(c),  " " ,quantile(c,0.75) , max(c))
  )
  return(cDT)
}

ct(diamonds$carat, diamonds$depth, diamonds$table, diamonds$price,diamonds$x, diamonds$y,diamonds$z)

```
**Measures of Dispersion**
```{r echo=FALSE}
#THE MEASURE OF DISPERSION CODE:
#install.packages("data.table")
#library('data.table')
dispersion_variance<-function(a){
  return(round(var(a),digits=2))
}
dispersion_sd<-function(a){
  return(round(sd(a),digits=2))
}

disper<-function(d,e,f,g,a,b,c){
  dDT = data.table(
  Measure = c("Var","S.Dev","IQR", "Range"),
  "|" = c("|","|","|","|"),
  carat = c(dispersion_variance(d), dispersion_sd(d), IQR(d), paste(range(d),collapse=" ")),
  "|" = c("|","|","|","|"),
  depth = c(dispersion_variance(e), dispersion_sd(e), IQR(e), paste(range(e),collapse=" ")),
  "|" = c("|","|","|","|"),
  table= c(dispersion_variance(f), dispersion_sd(f), IQR(f), paste(range(f),collapse=" ")),
  "|" = c("|","|","|","|"),
  price= c(dispersion_variance(g), dispersion_sd(g), IQR(g), paste(range(g),collapse=" ")),
  "|" = c("|","|","|","|"),
  x= c(dispersion_variance(a), dispersion_sd(a), IQR(a), paste(range(a),collapse=" ")),
  "|" = c("|","|","|","|"),
  y = c(dispersion_variance(b), dispersion_sd(b),IQR(b), paste(range(b),collapse=" ")),
  "|" = c("|","|","|","|"),
  z = c(dispersion_variance(c), dispersion_sd(c),IQR(c), paste(range(c),collapse=" "))
)
  return(dDT)
}

disper(diamonds$carat, diamonds$depth, diamonds$table, diamonds$price,diamonds$x, diamonds$y,diamonds$z)


```

**Summarizing the above measures grouped by class**

# 3. Exploring the Data

**Let's begin by finding the correlation for all the variables**

```{r echo=FALSE}
ggcorr(diamonds[,1:10], nbreaks = 4, label = TRUE, palette="RdPu", label_size = 3.5, label_color = "white")
```

We see that *price* & *carat* are highly correlated.
Also, x,y,z (i.e the length, width and depth) are correlated with price, which makes sense in real life as bigger the diamond, higher it will sell for! 
Since we know price & carat are highly correlated, we'll analyse the effect of other variables on price~carat \
1) 	**Effect of color on price per carat** \
```{r echo=FALSE}


q1<-ggplot(diamonds, aes(factor(color), price/carat, fill=color)) + geom_boxplot() + ggtitle("Diamond Price according to color") + xlab("color") + ylab("Diamond Price per carat US$") + coord_cartesian(ylim=c(0,21000))+theme(plot.title = element_text(hjust = 0.5))

q2<-ggplot(diamonds, aes(factor(clarity), price/carat, fill=clarity)) + geom_boxplot() + ggtitle("Diamond Price according to clarity") + xlab("clarity") + ylab("Diamond Price per carat US$") + coord_cartesian(ylim=c(0,21000))+theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(color = "grey20", size = 7, angle = 45, hjust = .5, vjust = .5, face = "plain"))

plot_grid(q1, q2)

```
From the above graph, it is observed that the color variation doesn't have any effect on Price per carat of the Diamond. So, analysis is done on cut and clarity factors.

2) 	**Effect of cut on price per carot** vs **effect of clarity on price per carot** \
```{r echo=FALSE}
#require(gridExtra)

p1<- ggplot (diamonds, aes (x = carat, y = price, color = cut))+
  geom_point (size = 0.3)+
  scale_color_brewer(palette = "Spectral")+
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle('Diamond Price per carat based on cut')+ theme(plot.title = element_text(hjust = 0.5))+ xlab("carat") + ylab("Diamond Price per carat US$") 

p2<- ggplot (diamonds, aes (x = carat, y = price, color = clarity))+
  geom_point (size = 0.3)+
  scale_color_brewer(palette = "Spectral")+
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle('Diamond Price per carat based on clarity') + theme(plot.title = element_text(hjust = 0.5))+ xlab("carat") + ylab("Diamond Price per carat US$") 

#grid.arrange(p1, p2, ncol=2)
plot_grid(p1, p2)

```
\
It is observed that the Blue Line (*Ideal Cut* & *IF Clarity*, respectively) is more sensitive w.r.t carat and Price per carat. This is can be further clarified and explained using the following graph:

3)	**Effect of cut and clarity together on price per carot** \
```{r echo=FALSE}
ggplot(diamonds, aes(factor(cut), (price/carat), color=cut)) + geom_point() + ggtitle("Diamond Price per Carat according to Cut & Clarity") + ylab("Diamond Price per Carat US$")+ theme(plot.title = element_text(hjust = 0.5))+facet_grid(.~clarity,switch = "both") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())


```
From the above graph, it is observed that as the cut moves towards *Ideal Cut* and clarity towards *IF (Highest Clarity)*, the price shoots more than *15000* US$, but this is not the case in every situation, probably due to other factors such as customer demand, market economy, environmental impacts, etc. 

4) **Trend of different cuts**

```{r echo=FALSE}
diamonds %>%
  ggplot(aes(x=(price))) +
  geom_histogram(stat="bin",binwidth= 500,fill="purple") +
 facet_wrap(~cut, ncol = 5, scales = "free")+ theme(axis.text.x = element_text(color = "grey20", size = 6, angle = 45, hjust = .5, vjust = .5, face = "plain"))
```
It is evident that as the price increases, the number of diamonds i.e., count, on y-axis decreases exponentially. Which is understood in real-world scenario i.e., few people can only afford to buy expensive diamonds.

# 4. Conclusion

**Conclude** \
1. Contrary to general notion, color has minimal effect on the price of diamonds.\
2. With clarity and cut, it is observed that the price increases steeply with high clarity and Ideal cut.\
3. There are more outliers in this dataset, so the dataset should be collected in a way that it should convey an accurate information for future prediction. \
\
**Future Scope** \
1. For ease of understanding to to get better insight about the diamonds dataset, the variable name should be properly defined (x, y, z). \
2. Time-Series data could be introduced with other variables to make more precise predictions & inferences.
