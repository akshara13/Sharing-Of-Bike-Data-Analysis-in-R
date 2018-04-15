rm(list = ls())
day <- read.csv("~/Desktop/Bike-Sharing-Dataset/day.csv")
bk_sh_dy <- day
head(bk_sh_dy)

dim(bk_sh_dy)

names(bk_sh_dy)
is.null(bk_sh_dy)
is.integer(bk_sh_dy)

bk_sh_dy<- data.frame(bk_sh_dy)
str(bk_sh_dy)

library(dplyr)
library(corrplot)
library(ggplot2)
library(stats)

bk_sh_dy$season <- factor(format(bk_sh_dy$season, format="%A"),
                          levels = c("1", "2","3","4") , labels = c("Spring","Summer","Fall","Winter"))
table(bk_sh_dy$season)

bk_sh_dy$holiday <- factor(format(bk_sh_dy$holiday, format="%A"),
                           levels = c("0", "1") , labels = c("Working Day","Holiday"))
table(bk_sh_dy$holiday)

bk_sh_dy$weathersit <- factor(format(bk_sh_dy$weathersit, format="%A"),
                              levels = c("1", "2","3","4") , 
                              labels = c("Good:Clear/Sunny","Moderate:Cloudy/Mist","Bad: Rain/Snow/Fog","Worse: Heavy Rain/Snow/Fog"))
table(bk_sh_dy$weathersit)

bk_sh_dy$yr <- factor(format(bk_sh_dy$yr, format="%A"),
                      levels = c("0", "1") , labels = c("2011","2012"))
table(bk_sh_dy$yr)

bk_sh_dy$actual_temp <- bk_sh_dy$temp*41
bk_sh_dy$actual_feel_temp <- bk_sh_dy$atemp*50
bk_sh_dy$actual_windspeed <- bk_sh_dy$windspeed*67
bk_sh_dy$actual_humidity <- bk_sh_dy$hum*100
bk_sh_dy$mean_acttemp_feeltemp <- (bk_sh_dy$actual_temp+bk_sh_dy$actual_feel_temp)/2
str(bk_sh_dy)
summary(bk_sh_dy)

h <- hist(bk_sh_dy$cnt, breaks = 25, ylab = 'Frequency of Rental', xlab = 'Total Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = 'blue')

xfit <- seq(min(bk_sh_dy$cnt),max(bk_sh_dy$cnt), length = 50)
yfit <- dnorm(xfit, mean =mean(bk_sh_dy$cnt),sd=sd(bk_sh_dy$cnt))
yfit <- yfit*diff(h$mids[1:2])*length(bk_sh_dy$cnt)
lines(xfit,yfit, col='red', lwd= 3)

par(mfcol=c(2,2))

boxplot(bk_sh_dy$cnt ~ bk_sh_dy$season,
        data = bk_sh_dy,
        main = "Total Bike Rentals Vs Season",
        xlab = "Season",
        ylab = "Total Bike Rentals",
        col = c("coral", "coral1", "coral2", "coral3")) 


boxplot(bk_sh_dy$cnt ~ bk_sh_dy$holiday,
        data = bk_sh_dy,
        main = "Total Bike Rentals Vs Holiday/Working Day",
        xlab = "Holiday/Working Day",
        ylab = "Total Bike Rentals",
        col = c("pink", "pink1", "pink2", "pink3")) 

boxplot(bk_sh_dy$cnt ~ bk_sh_dy$weathersit,
        data = bk_sh_dy,
        main = "Total Bike Rentals Vs Weather Situation",
        xlab = "Weather Situation",
        ylab = "Total Bike Rentals",
        col = c("purple", "purple1", "purple2", "purple3")) 


plot(bk_sh_dy$dteday, bk_sh_dy$cnt,type = "p",
     main = "Total Bike Rentals Vs DateDay",
     xlab = "Year",
     ylab = "Total Bike Rentals",
     col  = "orange",
     pch  = 19)


par(mfrow=c(2,2))

plot(bk_sh_dy$actual_temp, bk_sh_dy$cnt ,type = 'h', col= 'yellow', xlab = 'Actual Temperature', ylab = 'Total Bike Rentals')

plot(bk_sh_dy$actual_feel_temp, bk_sh_dy$cnt ,type = 'h', col= 'yellow', xlab = 'Actual Feel Temperature', ylab = 'Total Bike Rentals')

plot(bk_sh_dy$actual_windspeed, bk_sh_dy$cnt ,type = 'h', col= 'yellow', xlab = 'Actual Windspeed', ylab = 'Total Bike Rentals')

plot(bk_sh_dy$actual_humidity, bk_sh_dy$cnt ,type = 'h', col= 'yellow', xlab = 'Actual Humidity', ylab = 'Total Bike Rentals')

Cor_actual_temp<-cor(x = bk_sh_dy$actual_temp, y = bk_sh_dy$cnt)
Cor_actual_feel_temp <- cor(x = bk_sh_dy$actual_feel_temp, y =bk_sh_dy$cnt)

bk_sh_dy_cor<- bk_sh_dy %>% select (cnt,actual_temp,actual_feel_temp,mean_acttemp_feeltemp,actual_humidity,actual_windspeed)
bk_sh_dy_cor<- data.frame(bk_sh_dy_cor)

colnames(bk_sh_dy_cor)[1] <- "Total Number of Bike Rentals"
colnames(bk_sh_dy_cor)[2] <- "Temperature"
colnames(bk_sh_dy_cor)[3] <- "Feel Temperature"
colnames(bk_sh_dy_cor)[4] <- "Mean Actual Temp Feel Temp"
colnames(bk_sh_dy_cor)[5] <- "Humidity"
colnames(bk_sh_dy_cor)[6] <- "Windspeed"

cor(bk_sh_dy_cor)
corplot_bk_sh <- cor(bk_sh_dy_cor)
corrplot(corplot_bk_sh, method="number")

library(ggplot2)

ggplot_Temp_Rent<- ggplot(bk_sh_dy, aes(x=bk_sh_dy$actual_temp,y=bk_sh_dy$cnt))+geom_point(shape=1)+geom_smooth(method=lm)+ xlab("Actual Temp. in Celcius")+ylab("Bike Rentals")
ggplot_Temp_Rent+scale_y_continuous(breaks=c(0,1100,2345,3500,5000,6000,7000,8000))+labs(title="Total Bike Rentals Vs Actual Temperature | Intercept = 2345")

lm_test<- lm(bk_sh_dy$cnt~bk_sh_dy$actual_temp)
summary(lm_test)

plot(lm_test, col = "green")

lm_test1<- lm(sqrt(bk_sh_dy$cnt)~bk_sh_dy$actual_temp+bk_sh_dy$actual_humidity+bk_sh_dy$actual_windspeed)

lm_test1

summary(lm_test1)
lm_test2<- lm(((bk_sh_dy$cnt)^2)~bk_sh_dy$actual_temp+bk_sh_dy$actual_humidity+bk_sh_dy$actual_windspeed)

lm_test2
summary(lm_test2)

lm_test3<- lm((log(bk_sh_dy$cnt))~bk_sh_dy$actual_temp+bk_sh_dy$actual_humidity+bk_sh_dy$actual_windspeed)

lm_test3

summary(lm_test3)
lm_final<- lm(bk_sh_dy$cnt~bk_sh_dy$actual_temp+bk_sh_dy$actual_humidity+bk_sh_dy$actual_windspeed)

lm_final

summary(lm_final)
plot(lm_final,col = "gold", main = "Linear Regression: Bike Rentals, Temp, Windspeed and Humidity")









