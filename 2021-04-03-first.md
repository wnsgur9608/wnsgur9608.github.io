---
title: "HFC_Final code for Quality Factor"
author: "Chiseung Ahn"
date: '05/04/2021'
output: html_document
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Quantifying the Quality factors with disclosed information!

<br />

* #### **Preparation**

rm(list=ls())<br />
setwd("c:\\data\\r") 

    install.packages("gamlr")                     
    install.packages("tidyr")
    install.packages("dplyr") 
    install.packages("ggplot2")
    install.packages("plyr")
    install.packages("reshape2")
    install.packages("writexl") 
    install.packages("boot") 
    install.packages("PerformanceAnalytics")
    install.packages("xts")
    install.packages("magrittr")
    install.packages("lubridate") 
    install.packages("timeSeries")
    install.packages("broom") 
    install.packages("stargazer")
    install.packages("quantmod")
    install.packages("timetk")
    install.packages('readxl')
    install.packages('stringr')
    install.packages('progress')
    install.packages('tidyverse')
    install.packages('readr')
    install.packages('xlsx')
    install.packages("writexl")
    
    library(gamlr)
    library(tidyr)
    library(dplyr)
    library(ggplot2)
    library(plyr)
    library(reshape2)
    library(writexl)
    library(boot)
    library(PerformanceAnalytics)
    library(xts)
    library(magrittr)
    library(lubridate)
    library(timeSeries)
    library(broom)
    library(stargazer)
    library(quantmod)
    library(timetk)
    library(readxl) 
    library(stringr)
    library(progress)
    library(tidyverse)
    library(readr)
    library(xlsx)
    library(writexl)

  <br />

### First, calculate the score from 0 to 4 Range

<br />

### **(1) <u>1 point</u> - Novy-Marx score**
### If the firm's Novy-Marx score exceeds the median Novy-marx score, assign 1

  <br />

* #### **Using disclosed information in Dart (Korea)**

d1 <- read_excel('KOSPI+KOSDAQ Quarterly(1998~2020)_210130.xlsx')<br />
d2 <- read_excel('KOSPI+KOSDAQ TotalAsset Annual (1998-2019)_210130.xlsx') 

  sales <- filter(d1, d1[ ,3] == 6000904001)<br />
  cogs <- filter(d1, d1[,3] == 6000905001)<br />
  corp <- unique(d1[,1])<br />
    
  gp <- data.frame(corp) 

  <br />

- #### **Gross Profit = Sales - Cost Of Good Sold**

for(i in 5:119) {<br />
 gp[,i-3] <- sales[,i] - cogs[,i]<br />
} 

  <br />

- #### **bve** : book value of equity & **asset** : book value of total asset

  bve <- filter(d1, d1[,3] == 6000903001)<br />
  asset <- d2[ ,-2]

  <br />
  
- #### **Gross Profit Annual (1998~2019)**

gp_A <- data.frame(gp[,1])<br />

for(i in 1:22) {<br />
  k1 <- 5*i+1<br />
  gp_A[,i+1] <- gp[,k1]}<br />
    
colnames(gp_A) <- c("Symbol",seq(1998,2019,by=1)) 

  <br />

- #### **Gross Profit Half (1998.5 ~ 2019.5)**

gp_H <- data.frame(gp[,1])<br />

for(i in 1:22) {<br />
  k1 <- 5*i<br />
  gp_H[,i+1] <- gp[,k1-1]+gp[,k1]+gp[,k1+2]+gp[,k1+3]}<br />
    
colnames(gp_H) <- c("Symbol",seq(1998.5,2019.5,by=1)) <br />
    
-- 1998.5 = 3Q98 + 4Q98 + 1Q99 + 2Q99<br />

  <br />

- #### **Book Value of Equity (1998~2019)**

bve_A <- data.frame(bve[,1])<br />

for(i in 1:22) {<br />
  k1 <- 5*i+4<br />
  bve_A[,i+1] <- bve[,k1]}<br />
    
colnames(bve_A) <- c("Symbol",seq(1998,2019,by=1))

  <br />

- #### **<u>Gross Profit Growth Rate (1999~2019)</u> : gpgr(t) = gp(t) / gp(t-1)**

gpgr_A <- data.frame(gp_A[,1]) <br />

for(i in 2:22) {<br />
  a <- ifelse(gp_A[,i] < 0, NA, gp_A[,i])<br />
  b <- ifelse(gp_A[,i+1] < 0, NA, gp_A[,i+1])<br />
gpgr_A[,i] <- b/a } <br />

colnames(gpgr_A) <- c("Symbol",seq(1999,2019,by=1))<br />

  <br />

- #### **<u>Gross Profitability (1999~2019)</u> : gpy(t) = gp(t-1) / total asset(t-1)**

gpy_A <- data.frame(gp_A[,1])<br />

for(i in 2:22) {<br />
  gpy_A[,i] <- gp_A[,i]/asset[,i]<br />
} <br />

colnames(gpy_A) <- c("Symbol",seq(1999,2019,by=1))

  <br />

#### **Get Novy-Marx score by dividing the sum of Gross Profitability and GPGR by 2**

  marxcore <- data.frame(gp_A[,1])<br />

for(i in 2:22) {<br />
  marxcore[,i] <- (gpy_A[,i]+gpgr_A[,i])/2<br />
} <br />

colnames(marxcore) <- c("Symbol",seq(1999,2019,by=1))

<br />

#### **Set the criteria of 0 and 1 to median here **
#### **(The background such as the characteristics of individual companies did not seem so important, so we used the median for each year for each companies)**

<br />

marxmed <- data.frame(marxcore[,1])<br />

for(i in 2:22) {<br />
  marxmed[,i] <- median(marxcore[,i], na.rm = TRUE)<br />
}<br />

colnames(marxmed) <- c("Symbol",seq(1999,2019,by=1))

<br />

marx01 <- data.frame(marxcore[,1])<br />

for(i in 2:22) {<br />
  marx01[,i] <- ifelse(marxcore[,i]>=marxmed[,i], 1,0)<br />
} <br />

colnames(marx01) <- c("Symbol",seq(1999,2019,by=1))

<br />

#### **marx01[is.na(marx01)] <- 0**

<br />
<br />

### **(2) <u>3 points</u> - CFO, NI, Capital Increase**

### 1) CFO : If the Operating Cash Flow in the previous year is more than zero, assign 1.

<br />

e1 <- read_excel("KOSPI+KOSDAQ OperatingCF Annual (1998-2019)_210130.xlsx")<br />
e1 <- e1[-c(1,2),]<br />
e1 <- e1[ ,-2]

<br />

CFOscore <- data.frame(e1[,1])

<br />

for(i in 2:22) {<br />
  CFOscore[,i] <- ifelse(e1[,i]>0, 1,0)<br />
} 
<br />
colnames(CFOscore) <- c("Symbol",seq(1999,2019,by=1))

<br />

#### **CFOscore[is.na(CFOscore)] <- 0**

<br />

### 2) NI : If the Net Income in the previous year is more than zero, assign 1.

<br />
f1 <- read_excel('KOSPI+KOSDAQ NI Annual (1998-2019)_210130.xlsx')<br />
f1 <- f1[-c(1,2),]<br />
f1 <- f1[ ,-2]
<br />
NIscore <- data.frame(f1[,1])<br />
<br />
for(i in 2:22) {<br />
  NIscore[,i] <- ifelse(f1[,i]>0, 1,0)<br />
} 
<br />
colnames(NIscore) <- c("Symbol",seq(1999,2019,by=1))

<br />

#### **NIscore[is.na(NIscore)] <- 0**

<br />

### 3) Rights Issue : If there was not the capital increase in the previous year, assign 1.

<br />
g1 <- read_excel('KOSPI+KOSDAQ Capital_increase Annual (1998-2019)_210130.xlsx')<br />
g1 <- g1[ ,-2]
<br />
RIscore <- data.frame(g1[,1])
<br />
for(i in 2:22) {<br />
  RIscore[,i] <- ifelse(g1[,i]== 0, 1, 0)<br />
} <br />
colnames(RIscore) <- c("Symbol",seq(1999,2019,by=1))

<br />

#### **RIscore[is.na(RIscore)] <- 0**

<br />
<br />

### Score (0~4) - Marx01, CFO, NI, RI
<br />
Totalscore <- data.frame(g1[,1])

<br />

#### **for(i in 2:22) {**<br />
####  **Totalscore[,i] <- marx01[,i] + CFOscore[,i] + NIscore[,i] + RIscore[,i]**<br />
#### **}**
<br />

colnames(Totalscore) <- c("Symbol",seq(1999,2019,by=1))
<br />
write.xlsx(Totalscore, 'Score0to4_210221.xlsx')
<br />

#### **After this, organize those Excel file based on the ticker order for combining with the ESG data.**
#### **And add Residual ESG Tone Score on Score0to4**
<br />
a1 <- read_excel('Score0to4_210221 (Organized).xlsx')<br />
h1 <- read_excel("residualesgscore_simpleadd_210131 (Organized).xlsx")

<br />
<br />
------------------------------------------------------------------------------
<br />

### **Last, calculate the final score from 0 to 5 Range by adding residual ESG tone score**

<br />

### **(3) <u>1 point</u> - residual ESG tone score excluding advertising expense and size**
### **If the residual ESG tone score is less than zero, assign 1 (positive tone)**

<br />

RESGscore <- data.frame(h1[,1])
<br />
for(i in 2:22) {<br />
  RESGscore[,i] <- ifelse(h1[,i]<0, 1,0)<br />
} <br />
colnames(RESGscore) <- c("Symbol",seq(1999,2019,by=1))

<br />

#### **RESGscore[is.na(RESGscore)] <- 0**

a1[is.na(a1)] <- 0

<br />

### Score (0~5) - Marx01, CFO, NI, RI, RESG

Totalscore0_5 <- data.frame(h1[,1])

<br />


#### **for(i in 2:22) {**<br />
####   **Totalscore0_5[,i] <- a1[,i] + RESGscore[,i]**<br />
#### **}** 
<br />

colnames(Totalscore0_5) <- c("Symbol",seq(1999,2019,by=1))

<br />

#### **write.xlsx(Totalscore0_5, 'Score0_5(210221).xlsx')**