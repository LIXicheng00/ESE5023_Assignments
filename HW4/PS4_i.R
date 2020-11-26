#1. Plotting with ggplot2
library(ggplot2)
library(dplyr)
library(tidyr)
library(forecast)

hydro<-read.csv(file = 'hydrodata.csv',header = T)
hydro_tbl<-as_tibble(hydro) %>% 
  mutate(id = factor(id, ordered = TRUE),t=as.Date(t)) 
  
glimpse(hydro_tbl)

ggplot(hydro_tbl, aes(x =id, y =q, fill = id)) +
  geom_boxplot() +
  theme_classic()+
  theme_bw() +
  theme(plot.title=element_text(size=15, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) + 
  scale_color_discrete(name="Station") +
  labs(title="Daily flux of Yellow River in 2017-2020", 
       x="Station", y="Quantity(m^3 s)",fill='Station name')




ggplot(hydro_tbl,aes(x=t,y=q,color=id))+
  geom_line()+
  theme_bw() +
  theme(plot.title=element_text(size=15, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) + 
  scale_color_discrete(name="Station") +
  labs(title="Monthly sum flux of Yellow River in 2019-2020 in Tongguan", 
       x="Year", y="Quantity(m^3 s)")+
  facet_wrap( ~ id)

hydro_tbl %>% 
  mutate(year=substr(t,1,4)) %>% 
  filter(id=='tongguan'&year=='2019') %>% 
  ggplot(aes(q)) +
  geom_histogram(bins = 50) +
  theme_bw() +
  theme(plot.title=element_text(size=15, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) + 
  labs(title="Histogram of flux of Yellow River in 2019 in Tongguan", 
       x="Quantity(m^3 s)",y='Number of days')


hydro_tbl %>% 
  mutate(year=substr(t,1,4)) %>% 
  filter(year=='2019') %>% 
  ggplot(aes(x=t,y=q,color=id)) +
  geom_point(size=0.5) +
  theme_bw() +
  theme(plot.title=element_text(size=15, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) + 
  scale_color_discrete(name="Station")+
  labs(title="Scatter plot of flux of Yellow River in 2019", 
       x="time",y='Quantity(m^3 s)') 

library(fields)
library(maps)
library(RNetCDF)
ex.nc     <- open.nc("IUPB_s5p_201806_global_totalBrOVC.NC")
print.nc(ex.nc)
Lat       <- var.get.nc(ex.nc, "latitude")
Lon       <- var.get.nc(ex.nc, "longitude")
total_BrO_VC     <- var.get.nc(ex.nc, "total_BrO_VC") 
close.nc(ex.nc)

par(mar=c(4.5,3,2,1))
image.plot(Lon, Lat, total_BrO_VC,
           horizontal=T, useRaster=T,
           legend.shrink=0.75, axis.args=list(cex.axis = 1.25),
           legend.width=1, legend.mar=2,
           legend.args=list(text="Toal BrO Vertical Column [molec cm^{-2}]",
                            cex=1.25),
           xlab='',ylab='',midpoint=T, axes=F, ann=F
)
title(xlab="",cex.lab=1.25,font.lab=2)
axis(1,at=pretty(Lon),tck=-0.015,lwd=1,cex.axis=1.25,font=1)
title(ylab="",cex.lab=1.25,font.lab=2)
axis(2,at=pretty(Lat),tck=-0.015,lwd=1,cex.axis=1.25,font=1,las=1)
title(main=paste("Toal BrO Vertical Column in Jun. 2018"),
      cex.main=1,font.main=2)
# Add map
map('world',add=T,lwd=0.75,col="black")

# Add a box
box(lwd=2)


#2. Analysis of the time series of monthly temperature
Baoan<-read.csv(file='2281305.csv',header = T)

Baoan_tbl<-as_tibble(Baoan) 

Baoan_temp<-Baoan_tbl %>% 
  select(DATE,TMP) %>% 
  mutate(ym=substr(DATE,1,7),temp=as.numeric(substr(TMP,1,5)),
         quality=substr(TMP,7,7)) %>% 
  filter(ym>='2010-01'&ym<='2020-06'&quality=='1') %>% 
  mutate(temp=ifelse(temp=="-9999",NA,temp)) %>% 
  group_by(ym) %>% 
  summarise(monthly_mean=mean(temp)/10) %>% 
  mutate(month=as.Date(paste(ym,'01',sep='-')))


monthly_temp<- ts(Baoan_temp$monthly_mean, start=2010, frequency=12)
plot(monthly_temp,
     type='l',
     xlab='year',
     ylab='temperature(degrees Celsius)',
     main="Monthly average temperature of Bao'an in 2010.1-2020.6 in time series ",
     col = "darkgrey")
box(lwd=2,col="darkgrey")

##2.2 Decomposition
monthly_temp_components <- decompose(monthly_temp)
plot(monthly_temp_components)


###Do Box-Ljung test to the result
random<-as.numeric(monthly_temp_components$random)
Box.test(random,type='Ljung',
         lag=log(length(random)))
###Do acf to the result
omit_na_random<-na.omit(random)
rand_acf <- acf(omit_na_random, lag=40,main="white noise")
rand_acf

### Plot hist
hist(monthly_temp_components$random, prob=TRUE,
     main = "Histogram of monthly temperature")
### Add pdf
curve(dnorm(x, mean=mean(monthly_temp_components$random,na.rm=T),
            sd=sd(monthly_temp_components$random,na.rm=T)),
      add=TRUE, col="red")


##2.3 Fit an ARIMA(p,d,q) model
# hist(monthly_temp,
#      main = "Histogram of monthly mean temperature",
#      xlab = "Temperature(Degrees Celsius)")

monthly_temp_log<-log(monthly_temp)

# hist(monthly_temp_log,
#      main = "Histogram of log monthly mean temperature",
#      xlab = "Temperature(Degrees Celsius)")

monthly_temp_log_d1 <- diff(monthly_temp_log)
# hist(monthly_temp_log_d1,
#      main = "Histogram of difference of log monthly mean temperature",
#      xlab = "Temperature(Degrees Celsius)")


# Automated forecasting using an ARIMA model
model1 <- auto.arima(monthly_temp,trace=T)
model2 <- auto.arima(monthly_temp_log,trace=T)
model3 <- auto.arima(monthly_temp_log_d1,trace=T)

# Check acf and pacf
acf(monthly_temp_log)
pacf(monthly_temp_log)

# Ä£ÐÍÆÀ¼Û###############################
##source:https://blog.csdn.net/mr_muli/article/details/82779250
qqnorm(model2$residuals)
qqline(model2$residuals)
Box.test(model2$residuals,type="Ljung-Box")

## 2.5 Make predictions
month_forecast  <- 5
month_in_plot   <- 10
forecast <- forecast(model2, month_forecast)

# Plot predictions along with real values
plot(forecast, include = month_in_plot, xlab="Time", 
     ylab="log(Monthly mean)",type="o",lwd=2) 


# Get predicted values

# 2020-07
exp(forecast$mean[1])
exp(forecast$lower[1,1])
exp(forecast$upper[1,1])

# 2020-08
exp(forecast$mean[2])
exp(forecast$lower[2,1])
exp(forecast$upper[2,1])

# Verify the predictions
Baoan_temp2<-Baoan_tbl %>% 
  select(DATE,TMP) %>% 
  mutate(ym=substr(DATE,1,7),temp=as.numeric(substr(TMP,1,5)),
         quality=substr(TMP,7,7)) %>% 
  filter(ym>='2010-01'&ym<='2020-08'&quality=='1') %>% 
  mutate(temp=ifelse(temp=="-9999",NA,temp)) %>% 
  group_by(ym) %>% 
  summarise(monthly_mean=mean(temp)/10) %>% 
  mutate(month=as.Date(paste(ym,'01',sep='-')))

tail(Baoan_temp2)
