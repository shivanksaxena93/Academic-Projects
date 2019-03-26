##Installing Packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages(chron)
install.packages("dplyr")
install.packages("ggplot2")
install.packages("utils")
install.packages("plyr")
install.packages("doBy")  
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("NLP")
##Running Packages
library(ggplot2)
library("dplyr")
library(chron)
library("dplyr")
library("ggplot2")
library(plyr)
library(utils)
library(doBy) 
library("tm")
library("SnowballC")
library("wordcloud")
library("NLP")

##Reading the CSV file
Crimesinchicago<-read.csv("C:/Users/shiva/Desktop/chicago-crime-data/Crimesinchicago.csv")

##Cleaning Data by making subsets
Crimesinchicago <- subset(crimesinchicago,duplicated(Crimesinchicago$'Case_Number'))
summary(Crimesinchicago) 

##Diving dataset into test and train
set.seed(36)
training_index<- sort(sample(nrow(Crimesinchicago),nrow(Crimesinchicago)*.7))
train<-Crimesinchicago[training_index,]
test<-Crimesinchicago[-training_index,]
train

##Defining R date as a function

Crimesinchicago$Date <- as.POSIXlt(Crimesinchicago$Date,format= "%m/%d/%Y %H:%M")
head(Crimesinchicago$Date)

##showing that R recognises data as anbject untill defined
Crimesinchicago$time <- times(format(Crimesinchicago$Date, "%H:%M:%S" ))
head(Crimesinchicago$time)

##Bifurcating time into 4 zones
time.tag<-chron(times=c("00:00:00","06:00:00","12:00:00","18:00:00","23:59:00"))
time.tag


Crimesinchicago$time.tag <- cut(Crimesinchicago$time, breaks= time.tag,labels=c("00-06","06-12","12-18","18-00"), include.lowest=TRUE)  
table(Crimesinchicago$time.tag)


##Counting the types of crime
length(unique(Crimesinchicago$Primary_Type))

##Different types of crime based on primary crime type
table(Crimesinchicago$Primary_Type)


## Generalizing Crimes
Crimesinchicago$crime <- as.character(Crimesinchicago$Primary_Type)  

Crimesinchicago$crime<-ifelse(Crimesinchicago$crime %in% c("CRIM SEXUAL ASSAULT","PROSTITUTION","SEX OFFENSE"),'SEX',Crimesinchicago$crime) 

Crimesinchicago$crime <- ifelse(Crimesinchicago$crime %in% c("MOTOR VEHICLE THEFT"),"MVT",Crimesinchicago$crime)

Crimesinchicago$crime<-ifelse(Crimesinchicago$crime %in% c("GAMBLING","INTERFERE WITH PUBLIC OFFICER","INTERFERENCE WITH PUBLIC OFFICER","INTIMIDATION","LIQUOR LAW VIOLATION","OBSCENITY","NON-CRIMINAL","PUBLIC PEACE VIOLATION","PUBLIC INDECENCY","STALKING","NON-CRIMINAL"),"NONVIO",Crimesinchicago$crime)

Crimesinchicago$crime <- ifelse(Crimesinchicago$crime =="CRIMINAL DAMAGE","DAMAGE",Crimesinchicago$crime)  

Crimesinchicago$crime <- ifelse(Crimesinchicago$crime=="CRIMINAL TRESPASS","TRESPASS",Crimesinchicago$crime) 

Crimesinchicago$crime <- ifelse(Crimesinchicago$crime %in% c("NARCOTICS","OTHER NARCOTIC VIOLATION"),"DRUG",Crimesinchicago$crime)

Crimesinchicago$crime<-ifelse(Crimesinchicago$crime=="DECEPTIVE PRACTICE","FRAUD",Crimesinchicago$crime)

Crimesinchicago$crime<-ifelse(Crimesinchicago$crime %in% c("OTHER OFFENSE","OTHER OFFENSE"),"OTHER",Crimesinchicago$crime)

Crimesinchicago$crime<-ifelse(Crimesinchicago$crime %in% c("KIDNAPPING","WEAPONS VIOLATION","OFFENSE INVOLVING CHILDREN"),"VIO",Crimesinchicago$crime)

table(Crimesinchicago$crime)  


##Plotting Crimes on primary type

qplot(Crimesinchicago$crime,xlab="Chicago's")+ scale_y_continuous("Crime Rate")

##Vulnerable hours of a day
qplot(Crimesinchicago$time.tag,xlab="Time's slot",main="vulnerable hours of the day")+scale_y_continuous("Crime's Rate")

##Crimes per days of a week
Crimesinchicago$day<-factor(Crimesinchicago$day,levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
qplot(Crimesinchicago$day,xlab="Day of week",main="Estimated number of crimes per day of a week")+scale_y_continuous("Crime's Rate")

##Crime rates per month of an year
Crimesinchicago$month<-factor(Crimesinchicago$month,levels=c("Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sep","Oct","Nov","Dec"))  

qplot(Crimesinchicago$month,xlab="Month",main="Crimes by Month")+scale_y_continuous("Number of crimes")

##Plot depicting types o crimes

text <- readLines(file.choose())

docs <- Corpus(VectorSource(text))
inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 6,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##Installing Packages
install.packages("highcharter")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("viridis")
install.packages("plotly")
install.packages("lubridate")
install.packages("xts")
install.packages("maps")
install.packages("ggmap")
install.packages("gridExtra")

##Running Packages
library(ggplot2)
library(tidyr)
library(dplyr)
library(highcharter)
library(viridis)
library(plotly)
library(lubridate)
library(xts)
library(maps)
library(ggmap)
library(gridExtra)

##Plotting Timeseries
Crimesinchicago$Day <- factor(day(as.POSIXlt(Crimesinchicago$Date, format="%m/%d/%Y %I:%M:%S %p")))
Crimesinchicago$Month <- factor(month(as.POSIXlt(Crimesinchicago$Date, format="%m/%d/%Y %I:%M:%S %p"), label = TRUE))
Crimesinchicago$Year <- factor(year(as.POSIXlt(Crimesinchicago$Date, format="%m/%d/%Y %I:%M:%S %p")))
Crimesinchicago$Weekday <- factor(wday(as.POSIXlt(Crimesinchicago$Date, format="%m/%d/%Y %I:%M:%S %p"), label = TRUE))



Crimesinchicago$Date <- as.Date(Crimesinchicago$Date, "%m/%d/%Y %I:%M:%S %p")
detach("package:plyr", unload=TRUE)

remove.packages("plyr")


by_Date <- na.omit(Crimesinchicago) %>% group_by(Date) %>% summarise(Total = n())
tseries <- xts(by_Date$Total, order.by=as.POSIXct(by_Date$Date))
plot(tseries)


##Tiemseries based on arrest made
Arrests_by_Date <- na.omit(Crimesinchicago[Crimesinchicago$Arrest == 'TRUE',]) %>% group_by(Date) %>% summarise(Total = n())
arrests_tseries <- xts(Arrests_by_Date$Total, order.by=as.POSIXct(by_Date$Date))
plot(arrests_tseries)

by_location <- Crimesinchicago %>% group_by(Location_Description) %>% summarise(Total = n()) %>% arrange(desc(Total))
plot(by_location)

by_type <- Crimesinchicago %>% group_by(Primary_Type) %>% summarise(Total = n()) %>% arrange(desc(Total))
plot(by_type)

by_district <- Crimesinchicago %>% group_by(District) %>% summarise(Total = n()) %>% arrange(desc(Total))
plot(by_district)

by_ward <- Crimesinchicago %>% group_by(Ward) %>% summarise(Total = n()) %>% arrange(desc(Total))
plot(by_ward)

by_fbi <- Crimesinchicago %>% group_by(FBI_Code) %>% summarise(Total = n()) %>% arrange(desc(Total))
plot(by_fbi)

by_arrest <- Crimesinchicago %>% group_by(Arrest) %>% summarise(Total = n()) %>% arrange(desc(Total))
plot(by_arrest)

by_domestic <- Crimesinchicago %>% group_by(Domestic) %>% summarise(Total = n()) %>% arrange(desc(Total))
plot(by_domestic)

by_year <- Crimesinchicago %>% group_by(Year) %>% summarise(Total = n()) %>% arrange(desc(Total))
plot(by_year)


##Plotting hcharts

countingcrimes <- Crimesinchicago %>% group_by(Year, Month) %>% summarise(Total = n())
chicagocrimes <- ggplot(countingcrimes, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='lightblue') +
  ggtitle("Number of Crimes in a month according to years(2001-2016)")
plot(chicagocrimes)



hchart(streets_tseries, name = "Streets") %>% 
  hc_add_series(residence_tseries, name = "Residence") %>% 
  hc_add_series(apartment_tseries, name = "Apartment") %>%
  hc_add_series(sidewalk_tseries, name = "Sidewalk") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_title(text = "Crimes in Streets/Residence/Apartment/Sidewalk") %>%
  hc_legend(enabled = TRUE)

temp<-aggregate(Crimesinchicago$crime,by=list(Crimesinchicago$crime,Crimesinchicago$time.tag),FUN=length)  


names(temp)<-c("crime","time.tag","count")

ggplot(temp,aes(x=crime,y=factor(time.tag)))+geom_tile(aes(fill=count))+scale_x_discrete("Crime",expand=c(0,0))+scale_y_discrete("Time of day",expand=c(0,-2))+scale_fill_gradient("Number of crimes",low="white",high="steelblue")+theme_bw()+ggtitle("Crimes by time of day")+theme(panel.grid.major=element_line(colour=NA),panel.grid.minor=element_line(colour=NA)) 


homicide <- Crimesinchicago[Crimesinchicago$Primary_Type=="HOMICIDE",] 

homicide_year <-  homicide %>% group_by(Year) %>% summarise(Total = n())

hchart(homicide_year, "column", hcaes(Year, Total, color = Year)) %>%
  hc_add_theme(hc_theme_economist()) %>%
  hc_title(text = "Homicide 2001-2016")  %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style=box(fontSize = "14px"))



counting_homecides <- homicide %>% group_by(Year, Month) %>% summarise(Total = n())

ggplot(counting_homecides, aes(Year, Month, fill = Total)) +
  geom_tile(size = 1, color = "white") +
  scale_fill_viridis()  +
  geom_text(aes(label=Total), color='white') +
  ggtitle("Chicago's Homicide Rate (2001-2016)")  



##Plotting and Counting crime
plot(crime_count)
summary(crime_count)


##Time series
Crimesinchicago20012016 <- Crimesinchicago[Crimesinchicago$Year %in% c('2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016'),c('Date','ID')]


##Creating Timeseries
Crimesinchicago20012016$Date <- as.Date(Crimesinchicago20012016$Date, "%m/%d/%Y %I:%M:%S %p")
by_Date <- na.omit(Crimesinchicago20012016) %>% group_by(Date) %>% summarise(Total = n())
tseries <- xts(by_Date$Total, order.by=as.POSIXct(by_Date$Date))

diff <- Crimesinchicago20012016 %>% group_by(Date) %>% summarise(y = n()) %>% mutate(y = log(y))



names(diff) <- c("ds", "y")
diff$ds <- factor(diff$ds)


tempdata <- diff$y
crimeanalysis=ts(df$y, start=c (2001,1),end= c(2016,15), frequency = 1)
summary(crimeanalysis)
plot(crimeanalysis)

##Performing Differenciation
diff_1_crimeanalysis<-diff(crimeanalysis)

plot (diff_1_crimeanalysis)

##AR model
yearlyar <-arima(x=diff_1_crimeanalysis,order = c(2,0,0))
yearlyar
qqnorm(yearlyar$residuals)
qqline(yearlyar$residuals,col=2)

Box.test(yearlyar$residuals,lag = 6,type = 'Ljung')

##MA model
yearlyMA <-arima(x=diff_1_crimeanalysis_train,order = c(0,0,2))
yearlyMA
qqnorm(yearlyMA$residuals)
qqline(yearlyMA$residuals,col=2)

Box.test(yearlyMA$residuals,lag = 6,type = 'Ljung')


##ARIMA model
yearlyarima <-arima(coredata(diff_1_crimeanalysis_train),order = c(0,1,2))
yearlyarima

tsdiag(yearlyarima)

qqnorm(yearlyarima$residuals)
qqline(yearlyarima$residuals,col=2)


Box.test(yearlyarima$residuals,lag = 6,type = 'Ljung')

##ARMA model
yearlyarma <-arima(x=diff_1_crimeanalysis_train,order = c(2,0,2),,include.mean = T, method = 'ML')
yearlyarma

qqnorm(yearlyarma$residuals)
qqline(yearlyarma$residuals,col=2)

Box.test(yearlyarma$residuals,lag=6,type = 'Ljung')


hchart(tseries, name = "Crimes") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_credits(enabled = TRUE, text = "Sources: City of Chicago Administration and the Chicago Police Department", style = list(fontSize = "12px")) %>%
  hc_title(text = "Times Series plot of Chicago Crimes") %>%
  hc_legend(enabled = TRUE)


##Model Predictions
ar_predict=predict(yearlyar, n.ahead = 30,se.fit=T)
ar_predict

ma_predict=predict(yearlyMA, n.ahead=30,se.fit=T)
ma_predict

arima_predict=predict(yearlyarima, n.ahead=30,se.fit=T)
arima_predict

arma_predict=predict(yearlyarma, n.ahead=30,se.fit=T)
arma_predict

##Forecasting

library(prophet)

pp <- prophet(df)

future <- make_future_dataframe(pp, periods = 365 * 4)

head(future)

tail(future)
forecast <- predict(pp, future)

tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(pp, forecast)

prophet_plot_components(pp, forecast)

##Forecast for best model
accuracy(forecast(yearlyar ),test)
accuracy(forecast(yearlyMA ),test)
accuracy(forecast(yearlyarima ),test)
accuracy(forecast(yearlyarma ),test)



