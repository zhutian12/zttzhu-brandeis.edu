#load package
library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)

#chartseries
#Use R to observe a stock's performance---------------
#chart components: bollinger bands, % bollinger change, volume, moving average convergence divergence
getSymbols("AMZN",from="2008-08-01",to="2020-3-20")
AMZN%>%Ad()%>%chartSeries()
AMZN%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')

getSymbols("FB", from = "2008-08-01",to = "2020-3-20")
FB%>%Ad()%>%chartSeries()
FB%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')

getSymbols("TSLA", from = "2008-08-01",to = "2020-03-20")
TSLA%>%Ad()%>%chartSeries()
TSLA%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')

getSymbols("AAPL",from = "2008-08-01",to = "2020-03-20")
AAPL%>%Ad()%>%chartSeries()
AAPL%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')

getSymbols("ZM", from = "2008-08-01",to = "2020-3-20")
ZM%>%Ad()%>%chartSeries()
ZM%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')

#---------------------------
#Stock returns in log

AMZN_log_returns<-AMZN%>%Ad()%>%dailyReturn(type='log')
FB_log_returns<-FB%>%Ad()%>%dailyReturn(type='log')
TSLA_log_returns<-TSLA%>%Ad()%>%dailyReturn(type='log')
AAPL_log_returns<-AAPL%>%Ad()%>%dailyReturn(type='log')
ZM_log_returns<-ZM%>%Ad()%>%dailyReturn(type='log')

#Mean of log stock returns 

AMZN_mean_log<-mean(AMZN_log_returns,na.rm = T)
FB_mean_log<-mean(FB_log_returns,na.rm = T)
TSLA_mean_log<-mean(TSLA_log_returns,na.rm = T)
AAPL_mean_log<-mean(AAPL_log_returns,na.rm = T)
ZM_mean_log<-mean(ZM_log_returns,na.rm = T)

#round it to 4 decimal places

mean_log<-c(AMZN_mean_log,FB_mean_log,TSLA_mean_log,AAPL_mean_log,ZM_mean_log)
mean_log<-round(mean_log,4)

#standard deviation of log stock returns

AMZN_sd_log<-sd(AMZN_log_returns)
FB_sd_log<-sd(FB_log_returns)
TSLA_sd_log<-sd(TSLA_log_returns)
AAPL_sd_log<-sd(AAPL_log_returns)
ZM_sd_Log<-sd(ZM_log_returns)

#round it to 4 decimal places 

sd_log<-c(AMZN_sd_log,FB_sd_log,TSLA_sd_log,AAPL_sd_log,ZM_sd_Log)
sd_log<-round(sd_log,4)

#create data frame
graphic1<-data.frame(rbind(c("AMZN",AMZN_mean_log,AMZN_sd_log),c("FB",FB_mean_log,FB_sd_log),c("TSLA",TSLA_mean_log,TSLA_sd_log),c("AAPL",AAPL_mean_log,AAPL_sd_log),c("ZM",ZM_mean_log,ZM_sd_Log)),stringsAsFactors = FALSE)
graphic1<-data.frame(mean_log,sd_log)
rownames(graphic1)<-c("AMZN","FB","TSLA","AAPL","ZM")
colnames(graphic1)<-c("Mean_Log_Return", "Sd_Log_Return")

#Correlation--------------------
data<-cbind(diff(log(Cl(AMZN))),diff(log(Cl(ZM))),diff(log(Cl(AAPL))),diff(log(Cl(FB))))
chart.Correlation(data)

#plotly graphic-----------------------
#Used plotly to create a visualization of each stock's risk v reward. 
#Risk: standard deviation of log returns
#Reward: mean of log returns

xlab<-list(title="Reward")
ylab<-list(title="Risk")

plot_ly(x=graphic1[,1],y=graphic1[,2],text=rownames(graphic1),type='scatter',mode="markers",marker=list(color=c("black","blue","red","grey","green")))%>%
  layout(title="Risk v Reward",xaxis=xlab,yaxis=ylab)

#Random Walk: past cannot predict future. 
#change in stock price has same distribution and are independent of each other
mu<-ZM_mean_log
sig<-ZM_sd_Log
testsim<-rep(NA,1000)

#generate random daily exponent increase rate using ZM's mean and sd log returns

#one year 252 trading days, simulate for 4 years 
# 4*252 trading days

price<-rep(NA,252*4)

#most recent price
price[1]<-as.numeric(ZM$ZM.Adjusted[length(ZM$ZM.Adjusted),])

#start simulating prices

for(i in 2:length(testsim)){
  price[i]<-price[i-1]*exp(rnorm(1,mu,sig))
}

random_data<-cbind(price,1:(252*4))
colnames(random_data)<-c("Price","Day")
random_data<-as.data.frame(random_data)

random_data%>%
  ggplot(aes(Day,Price))+geom_line()+labs(title="Zoom (ZM) price simulation for 4 years")+theme_bw()

#Monte Carlo Simulation
N<-500
mc_matrix<-matrix(nrow=252*4,ncol=N)
mc_matrix[1,1]<-as.numeric(ZM$ZM.Adjusted[length(ZM$ZM.Adjusted),])

for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(ZM$ZM.Adjusted[length(ZM$ZM.Adjusted),])
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}

name<-str_c("Sim ",seq(1,500))
name<-c("Day",name)

final_mat<-cbind(1:(252*4),mc_matrix)
final_mat<-as.tibble(final_mat)
colnames(final_mat)<-name

dim(final_mat) #1008 501

final_mat%>%
  gather("Simulation","Price",-Day)%>%ggplot(aes(x=Day,y=Price,Group=Simulation))+geom_line(alpha=0.2)+labs(title="Zoom Stock (ZM): 500 Monte Carlo Simulations for 4 Years")+theme_bw()

#is it likely? Check the confidence interval
probs = c(0.1,0.25,0.5,0.75,1)
final_mat[500,-1]%>%as.numeric()%>%quantile(probs=probs)

final_mat[500,] %>% gather(var,val,-Day) %>%  arrange(desc(val))
x