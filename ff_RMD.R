---
title: "Portfolio Analysis with Fama French Factors"
author: "Celia Marraro"
date:  \`r format(Sys.Date(), "%d %B, %Y")`\
output: html_document
---

Required Packages
```{r, warning=FALSE, message=FALSE}
library(TTR)
library(tseries)
library(quantmod)
library(data.table)
```

Import Fama/French factors 
Description 
[Here](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/f-f_factors.html)

```{r}
url = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"

download.file(url, "F-F_Research_Data_Factors_daily_CSV.zip")
unzip("F-F_Research_Data_Factors_daily_CSV.zip")
file.remove("F-F_Research_Data_Factors_daily_CSV.zip")

f <- read.csv("F-F_Research_Data_Factors_daily.csv", header=F, skip=3)
```
**Get firm stock symbols from AMEX, NYSE, and NASDAQ exchanges**
```{r, warning=FALSE}
stocks = stockSymbols()
stocks = stocks[complete.cases(stocks),]
stocks = stocks[stocks$LastSale>=50,]

tickers=stocks$Symbol
tickers=sample(tickers, 50)
```
Compute daily returns 
```{r, warning=FALSE, message=FALSE, results='hide'}
r=list()
for(i in 1:length(tickers)){
  r[[i]]=get.hist.quote(tickers[i], quote="AdjClose")
  r[[i]]=xts(r[[i]])
  r[[i]]=dailyReturn(r[[i]], type="log")
}

names(r)=tickers
r=do.call(merge, r)
colnames(r)=tickers
r=r["2016-01-31/"]
```
Apply uniformly distributed weights to portfolio stocks
```{r}
r=replace(r, is.na(r), 0)
w= runif(50); w=w/sum(w)
r2=r%*%w
r2= xts(r2, order.by=index(r))
head(r2)
```

```{r}
par(mfrow=c(2,1))
plot(r2, main="Portfolio Daily Returns Returns")
plot(cumsum(r2), main="Portfolio Cumulative Returns")
```

```{r}
colnames(f) <- c("Date", "Mkt-Rf", "SMB", "HML", "Rf")
f <- f[-1,]
f[,1] <- as.Date(f[["Date"]], "%Y%m%d")
f <- na.exclude(f)
f1 <- xts(f,order.by=f[,"Date"])
f1 <- f1[,-1]
f1=f1["2016-01-31/"]
r2=r2[index(f1)]
```
```{r}
plot.zoo(f1, main=NA)
mtext(text="Fama-French Factors from K. French",
      adj=0, outer=T, line=-2, cex=2)
```
```{r}
fundXcess <- r2 - as.numeric(f1[,'Rf'])

ffReg <- lm(fundXcess~as.numeric(f1$SMB)
            +as.numeric(f1$HML)
            +as.numeric(f1$`Mkt-Rf`))
summary(ffReg)
```

```{r}
myplot <- plot(x=cumsum(r2), col="red", lwd=2, main="", ylim=c(-2.5, 2.5))
l1 <- lines(x=f1[,'Mkt-Rf'], col=3, lwd=1)
l2 <- lines(x=f1[,'SMB'], col=4, lwd=1)
addLegend("topright", legend.names = c("Cumul.Rtns", "Mkt-Rf", "SMB"),
          lty=1, col=c("red", 3, 4), cex=.8)
plot(as.ts(cumsum(f1[,'Mkt-Rf'])), main=NA, ylim=c(-1,50))
lines(as.ts(cumsum(f1[,"HML"])), col=3)
lines(as.ts(cumsum(f1[,'Rf'])), col=4)
lines(as.ts(cumsum(f1[,'SMB'])), col=6)
legend(0, 48, legend=c("Mkt-Rf", "HML", "Rf", "SMB"), 
       col=c("black", 3,4,6), lty=1)
mtext(text="Fama/French Factors from K. French", 
      adj=0, outer=T, line=-2, cex=1.5)
```




