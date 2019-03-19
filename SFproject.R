# Load Libraries
library(tidyverse)
library(tseries)
library(lubridate)

# Get data for time
t <- get.hist.quote("TM")
tt <- time (t)

#get stock data
ixic <- get.hist.quote("^IXIC", end = "2019-03-16") %>% cbind(year = year(tt), month = month(tt), day = day(tt))  %>% as.data.frame() %>% select("High" , "year", "month", "day") %>% filter(year == 2014 | year == 2019 )

ba_open <- get.hist.quote("BA", end = "2019-03-16") %>% cbind(year = year(tt), month = month(tt), day = day(tt))  %>% as.data.frame() %>% select("Open" , "year", "month", "day")%>% filter(year==  2014 | year == 2019)

bac_open <- get.hist.quote("BAC", end = "2019-03-16") %>% cbind(year = year(tt), month = month(tt), day = day(tt))  %>% as.data.frame() %>% select("Open" , "year", "month", "day")%>% filter(year == 2014 | year ==  2019)

brk_open <- get.hist.quote("BRK-A", end = "2019-03-16") %>% cbind(year = year(tt), month = month(tt), day = day(tt))  %>% as.data.frame() %>% select("Open" , "year", "month", "day")%>% filter(year ==  2014 | year == 2019)

dis_open <- get.hist.quote("DIS", end = "2019-03-16") %>% cbind(year = year(tt), month = month(tt), day = day(tt))  %>% as.data.frame() %>% select("Open" , "year", "month", "day")%>% filter(year == 2014 | year == 2019)

fdx_open <- get.hist.quote("FDX", end = "2019-03-16") %>% cbind(year = year(tt), month = month(tt), day = day(tt))  %>% as.data.frame() %>% select("Open" , "year", "month", "day")%>% filter(year == 2014 | year == 2019)

ir_open <- get.hist.quote("IR", end = "2019-03-16") %>% cbind(year = year(tt), month = month(tt), day = day(tt))  %>% as.data.frame() %>% select("Open" , "year", "month", "day")%>% filter(year ==  2014 | year ==2019)

msft_open <- get.hist.quote("MSFT", end = "2019-03-16") %>% cbind(year = year(tt), month = month(tt), day = day(tt))  %>% as.data.frame() %>% select("Open" , "year", "month", "day")%>% filter(year ==   2014 |  year == 2019)

# Run lm

stocks_lm <- lm(ixic[,1] ~ ba_open[,1] + bac_open[,1] + brk_open[,1] + dis_open[,1] + fdx_open[,1] + ir_open[,1] + msft_open[,1])
stocks_lm
summary(stocks_lm)

#plot lm Residuals
plot(stocks_lm $residuals)

#plot normal probability plot
stocks_stdres <- rstandard(stocks_lm)
qqnorm(stocks_stdres, ylab = "Residuals", xlab = "Fitted Value", main = "Stocks")
qqline(stocks_stdres)
