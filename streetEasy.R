---
title: "StreetEasy Real-Estate Data"
author: "Celia Marraro"
date:  \`r format(Sys.Date(), "%d %B, %Y")`\
output: html_document
---

The research and findings in the following project have been completed on behalf of StreetEasy.

Required Packages
```{r,warning=FALSE, message=FALSE}

library(xts)
library(lattice)
library(ggplot2)
library(tidyr)
library(reshape)
library(car)
library(dplyr)
library(lubridate)
library(quantmod)
library(lmtest)
library(plm)
library(stargazer, quietly = T)
library(caret)
library(nnet)
library(hts)
library(knitr)

```

###Preprocessing Data
```{r, warning=F, message=F}

url1 <- 'https://s3.amazonaws.com/streeteasy-market-
data-api/data_repository/E1_rentalInventory_All.zip'

url2 <- 'https://s3.amazonaws.com/streeteasy-market-
data-api/data_repository/A9_daysOnMarket_All.zip'

url3 <- 'https://s3.amazonaws.com/streeteasy-market-
data-api/data_repository/A6_medianSalePrice_All.zip'

url4 <- 'https://streeteasy-market-data-api.s3.amazonaws.com/
data_repository/E2_medianAskingRent_All.zip'

temp = tempfile()

download.file(url4, temp)
med_AskRent <- read.csv(unz(temp, "E2_medianAskingRent_All.csv"))

download.file(url3, temp)
medSalePrice  <- read.csv(unz(temp, "A6_medianSalePrice_All.csv"))

download.file(url2, temp)
daysOnMarket <- read.csv(unz(temp, "A9_daysOnMarket_All.csv"))

download.file(url1, temp)
totalRentInv <- read.csv(unz(temp, "E1_rentalInventory_All.csv"))

unlink(temp)
```


Remove NAs
```{r}
totalRentInv <- na.exclude(totalRentInv)
med_AskRent <-na.exclude(med_AskRent)
daysOnMarket <-na.exclude(daysOnMarket)
medSalePrice <- na.exclude(medSalePrice)
```
**Median Sale Price**
```{r, warning=FALSE, message=F}

medSalePriceSub <- subset(medSalePrice, 
                          as.character(medSalePrice$Area) %in% 
                          as.character(daysOnMarket$Area) & 
                          medSalePrice$AreaType=="neighborhood")
medSalePriceLong <- gather(medSalePriceSub, Date, 
                           "Median Sale Price", 4:ncol(medSalePrice), 
                           factor_key=T)
medSalePriceLong <- medSalePriceLong[order(medSalePriceLong$Area, 
                                           medSalePriceLong$Date),]
medSalePriceLong$Date <- sub("X", "", medSalePriceLong[,4])
medSalePriceLong$Date <- sub(".", "-", medSalePriceLong[,4], fixed=T)
medSalePriceLong$Date <- as.Date(paste(medSalePriceLong$Date,1, 
                                       sep = "-"), "%Y-%m-%d")
medSalePrice_zoo <- as.zooreg(medSalePriceLong[,c(4,1:3,5)])
medSalePriceCast <- cast(medSalePriceLong, Date~Area)
medSalePrice_ts <- xts(medSalePriceCast, order.by=medSalePriceCast$Date)
```
**Median Asking Rent** 
```{r, warning=FALSE, message=FALSE}
medAskRent <-subset(med_AskRent, as.character(med_AskRent$Area) %in% 
                    as.character(daysOnMarket$Area) & 
                    med_AskRent$AreaType=="neighborhood")
medAskRentLong <- gather(medAskRent, Date, 
                         "Median Asking Rent", 
                         4:ncol(med_AskRent), factor_key=T)

medAskRentLong <- medAskRentLong[order(medAskRentLong$Area, 
                                       medAskRentLong$Date),]

row.names(medAskRentLong) <- 1:NROW(medAskRentLong)
medAskRentLong$Date <- sub("X", "", medAskRentLong[,4])
medAskRentLong$Date <- sub(".", "-", medAskRentLong[,4], fixed = T)
medAskRentLong$Date <- as.Date(paste(medAskRentLong$Date,1,sep="-"),"%Y-%m-%d")
med_AskRent_zoo <- as.zooreg(medAskRentLong[,c(4,1:3,5)])
medAskRentCast <- cast(medAskRentLong, Date~Area)
medAskRent_ts = xts(medAskRentCast, order.by=medAskRentCast$Date)
```
**Days on Market i.e. Housing supply**
```{r, message=FALSE, warning=FALSE}
daysOnMktSub <- subset(daysOnMarket, as.character(daysOnMarket$Area) %in% 
                       as.character(medAskRent$Area))
daysOnMarketLong <- gather(daysOnMktSub, Date, 
                           "Days on Market", 4:ncol(daysOnMarket), factor_key=T)
daysOnMarketLong <- daysOnMarketLong[order(daysOnMarketLong$Area, 
                                           daysOnMarketLong$Date),]

row.names(daysOnMarket) <- 1:NROW(daysOnMarket)
daysOnMarketLong$Date <- sub("X", "", daysOnMarketLong[,4])
daysOnMarketLong$Date <- sub(".", "-", daysOnMarketLong[,4], fixed=T)
daysOnMarketLong$Date <- as.Date(paste(daysOnMarketLong$Date, 1, 
                                       sep="-"),"%Y-%m-%d")
daysOnMarket_zoo <- as.zooreg(daysOnMarketLong[,c(4,1,2,3,5)])
daysOnMarketCast <- cast(daysOnMarketLong, Date~Area)
daysOnMarketCast$Date <- as.yearmon(as.character(daysOnMarketCast$Date))
daysOnMarket_ts <- as.xts(daysOnMarketCast, 
                          order.by=as.yearmon(daysOnMarketCast$Date))
```

**Total Rental Inventory**  
```{r, warning=FALSE, message=FALSE}
ttlRentInv <- subset(totalRentInv, 
                     as.character(totalRentInv$Area) %in% 
                     as.character(daysOnMarket$Area) & 
                     totalRentInv$AreaType=="neighborhood")
ttlRentIvLong <- gather(ttlRentInv, Date, "Total Rent Inventory", 
                        4:ncol(totalRentInv), factor_key=T)
ttlRentIvLong <- ttlRentIvLong[order(ttlRentIvLong$Area,ttlRentIvLong$Date),]
ttlRentIvLong$Date <- sub("X", "", ttlRentIvLong[,4])
ttlRentIvLong$Date <- sub(".", "-", ttlRentIvLong[,4], fixed=T)
ttlRentIvLong$Date <- as.Date(paste(ttlRentIvLong$Date,1, sep="-"), "%Y-%m-%d")
ttlRentInv_zoo <-as.zooreg(ttlRentIvLong[,c(4,1:3,5)])
ttlRentIvCast <- cast(ttlRentIvLong, Date ~ Area)
ttlRentInv_ts <- as.xts(ttlRentIvCast, order.by=ttlRentIvCast$Date)
```
*Price-Rent Ratio calculation*
```{r}
m1 <- apply(as.matrix.noquote(medAskRent_ts), 2, as.numeric)
m2 <- apply(as.matrix.noquote(medSalePrice_ts), 2, as.numeric)
m <- t(m2)/t(m1*12)
priceRentRatio <- t(m)
priceRentRatio <- xts(priceRentRatio, order.by=medAskRentCast$Date)
priceRentIndx <- gather(as.data.frame(priceRentRatio), 
                        Date, "Price Rent Index", 1:9, 
                        factor_key=T )
```
*Monthly growth rate  for housing supply*
```{r}
diffDaysOnMkt_1 <- diff(daysOnMarket_ts)/lag.xts(daysOnMarket_ts,1)
diffDaysOnMkt <- gather(as.data.frame(diffDaysOnMkt_1), 
                        Date, "Month Over Month Homes Supply", 
                        1:9,factor_key=T)
diffDaysOnMkt[is.na(diffDaysOnMkt)] <- 0
```
Panel data 
```{r, results='markup'}
panelData = daysOnMarketLong[,c(4,1:3,5)]
panelData<- cbind.data.frame(panelData, 
                             medAskRentLong$`Median Asking Rent`, 
                             medSalePriceLong$`Median Sale Price`, 
                             ttlRentIvLong$` Total Rent Inventory`)
colnames(panelData) <- c("Date", "Area", "Boro", 
                         "Area Type", "Days on Market", "Median Asking Rent", 
                         "Median Sale Price", "Total Rent Inventory")
panelData$Date <- as.yearmon(as.character(daysOnMarketLong$Date))
panelData$'Price Rent Index'<- priceRentIndx[,2]
panelData$'Home Supply Growth' <- diffDaysOnMkt$`Month Over Month Homes Supply`
panelData$Area <- factor(panelData$Area)
rownames(panelData) <- 1:NROW(panelData)
panelData$`Median Asking Rent` <-panelData$`Median Asking Rent`
panelData$indexTRI <- cut(panelData$`Total Rent Inventory`, 
                          breaks=100, labels=F)
panelData$indexPI <- cut(panelData$`Price Rent Index`, 
                         breaks=30, labels=F) 
head(panelData)
```
```{r, fig.height=8, fig.width=8}
scatterplot(panelData$`Median Asking Rent` 
            ~panelData$`Days on Market`+ 
            panelData$Area-1, reg.line=F, 
            xlab = "Days on Market", 
            ylab = "Median Asking Rent", 
            legend.title = "Neighborhoods", grid=F)
```
Covariance tests: Fixed Effects vs. Random Effects vs. Partial Pooling
```{r warning=FALSE, results= 'markup'}
olS <-lm(panelData$`Price Rent Index`
         ~panelData$`Days on Market`)
kable(summary(olS)$coef, digits=3)
fixed<- plm(panelData$`Price Rent Index` 
            ~panelData$`Days on Market`,
            index = c('Area','Date'), data=
            panelData, model= 'within')
kable(summary(fixed)$coef, digits=3)
pFtest(fixed, olS, lower.tail=T) #p-value < .05, choose FE
random <- plm(panelData$`Price Rent Index`
              ~panelData$`Days on Market`, index= 
              c('Area', 'Date'), data=panelData, model='random')
kable(summary(random)$coef, digits=3)
plot(random$residuals, within=F, pooling=F, random=T, 
    xlab="Days on Market", ylab="Residuals", sub="Panel effect")

phtest(fixed,random) #p-value > .05, choose RE
pool<- plm(panelData$`Price Rent Index`
           ~panelData$`Days on Market`, 
           index=c('Area','Date'), data=panelData, model='pooling')
kable(summary(pool)$coef, digits=3)
plmtest(pool, type=c("bp")) #p-value < .05, H0 of no panel effect rejected, choose RE
pcdtest(fixed, test="lm") #p-value <.05, cross-sectional dependence 
coeftest(random, vcovHC(random, type='HC0', method='white1'))
```
Construct IV using random effects model
```{r}
iv1 <- predict(random)
panelData$iv1 <- iv1 
```
Plots for exploration 
```{r}
tsRainbow <- rainbow(ncol(medAskRent_ts))
xyplot.ts(medAskRent_ts, col=tsRainbow, ylab="Median Asking Rent")
diffDaysOnMkt_1 %>% as.data.frame() %>% na.omit() %>% GGally::ggpairs(
                    axisLabels="none", 
                    title="Days on Market Monthly Growth Rate")
```
Sampling for softmax regression
```{r}
set.seed(3214)
trainingRows <- sample(1:nrow(panelData), .7*nrow(panelData))
trainS <- panelData[sort(trainingRows),]
testS <- panelData[sort(-trainingRows),]
table(trainS$Area)

```
Standardize variables for classification algorithm
```{r}
stdZ01 <- function(x){(x-min(x))/(max(x)-min(x))
                     }
train <- apply(trainS[,c(5:10,13)], 2, stdZ01) %>%
               cbind.data.frame(trainS[,c(1:2,11:12)]) 
test <- apply(testS[,c(5:10,13)], 2, stdZ01) %>%
              cbind.data.frame(testS[,c(1:2,11:12)]) 
```
Multinomial log-linear models via neural networks
```{r, results='hide', warning=FALSE, message=FALSE}
options(contrasts=c("indexTRI", "indexPI"))

mod1<- multinom(Area~`Days on Market`+`Days on Market`*indexTRI+indexTRI, 
                data=train, maxit=500) 
summary(mod1)
varImp(mod1)


mod2<- multinom(Area~indexTRI, data=train) %>%
                update(.~.+iv1*indexTRI+iv1, maxit=500, Hess=T)
summary(mod2)
varImp(mod2)
```
```{r, results='asis', warning=FALSE, message=FALSE}
stargazer(mod1, type="html", out="mod1.htm",
          intercept.bottom=F, model.numbers=F,
          align=T)
stargazer(mod2, type="html", out="mod2.htm",
          intercept.bottom=F, model.numbers=F,
          align=T) 
```
```{r, results='markup', warning=FALSE, message=FALSE}
oddsRatioTable1 <- exp(coef(mod1))
kable(oddsRatioTable1, digits=5, align="c") #log-odds


oddsRatioTable2 <- exp(coef(mod2))
kable(oddsRatioTable2, digits=3, align="c") #log-odds
```

*For this m x m confusion matrix, negative recall (specificity) is high, a good sign in terms of our prediction goals. Additionally, balanced accuracy is the preferred evaluation metric here to decipher algorithm performance *
```{r}
t1 <- predict(mod1,test, type="class")
cfMat <- confusionMatrix(t1, test$Area)
print(cfMat)
kable(cfMat$table, align="c")

t2 <- predict(mod2,test, type="class")
cfMat2 <- confusionMatrix(t2, test$Area)
print(cfMat2)
kable(cfMat2$table, align="c")
```
Price-to-Rent Ratio plot
```{r, message=FALSE, fig.height=8, fig.width=8}
priceRentTs <- ts(priceRentRatio, start=2010, frequency=12)
priceRent_hts <- hts(priceRentTs)
plot1 <- aggts(priceRent_hts, levels=1)
plot0 <- aggts(priceRent_hts, levels=0)
p1 <- autoplot(plot1[,c(1:5)])+xlab("Year")+ 
               ylab("Price to Rent Index")+
               scale_color_discrete(guide=guide_legend("Neighborhood"))
p2 <- autoplot(plot1[,c(6:9)])+xlab("Year")+ 
               ylab("Price to Rent Index")+
               scale_color_discrete(guide=guide_legend("Neighborhood"))
p0 <- autoplot(plot0)+xlab("Year")+ 
              ylab("Price to Rent Index")+
              ggtitle("Total")
lay=rbind(c(1,1), c(2,2), c(3,3))
gridExtra::grid.arrange(p0, p1, p2, layout_matrix=lay)



```





