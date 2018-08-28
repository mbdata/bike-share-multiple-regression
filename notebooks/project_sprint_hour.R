library(ggplot2)
library(reshape2)
library(plyr)
library(car)
library(Hmisc)
library(psych)
library(scales)
library(xts)
library(randomForest)
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ModelMetrics)

#Import dataset
hour <- read.csv("../data/hour.csv")
View(hour)

#############################################################################
#                        DATA CLEANING AND EXPLORATION
#############################################################################

#remove 'instant' column since this is just a copy of the index
hour = hour[-c(1)]

#what are the dimensions of the dataset?
dim(hour)

#[1] 17379    16

#create new columns that convert normalized temperatures to raw temperatures in Celsius.
#Multiply 'temp' by 41 and 'atemp' by 50 (refer to meta-data).
hour$raw.temp = hour$temp * 41
hour$raw.atemp = hour$atemp * 50

#do the same conversion to get raw windspeed and raw humidity
hour$raw.windspeed = hour$windspeed * 67
hour$raw.hum = hour$hum * 100

hour = hour[-c(10:13)] #remove normalized columns which we just converted

#what are the mean actual and mean feeling temperatures?
mean(hour$raw.temp) #[1] 20.37647
mean(hour$raw.atemp) #[1] 23.78876

#what are the medians?
median(hour$raw.temp) #[1] 20.5
median(hour$raw.atemp) #[1] 24.24

#MONTH variable

ggplot(data=hour, aes(x=mnth, y=cnt))+geom_bar(stat='identity') #more people rent during warmer months

hour$mnth = as.factor(hour$mnth) #convert month to factor
ggplot(data=hour, aes(x=mnth, y=cnt, color=mnth))+geom_boxplot() #graph boxplots to see ranges of rental
ggplot(data=hour, aes(x=mnth, y=cnt, color=mnth))+geom_violin()
#more activity occurs in the summer months June, July, August, and September
#there appear to be many outliers for every month -- why is this?


#HOUR variable

hour$hr = as.factor(hour$hr) #convert hour to factor for all observations

#let's create a subset for just all months and see what these outliers are
jan = subset(hour, mnth==1) #January
feb = subset(hour, mnth==2) #February
mar = subset(hour, mnth==3) #March
apr = subset(hour, mnth==4) #April
may = subset(hour, mnth==5) #May
jun = subset(hour, mnth==6) #June
jul = subset(hour, mnth==7) #July
aug = subset(hour, mnth==8) #August
sep = subset(hour, mnth==9) #September
oct = subset(hour, mnth==10) #October
nov = subset(hour, mnth==11) #November
dec = subset(hour, mnth==12) #December

#all months
ggplot(data=hour)+geom_bar(aes(x=hr, y=cnt), fill="deepskyblue", stat="identity")+
  labs(title = "Total Bike Rentals by Hour", x = "Hour", y = "Count")+
  theme(plot.title = element_text(hjust=0.5), axis.title.y = element_text(angle = 0, vjust = 0.5))+
  scale_y_continuous(labels = comma)
  #plotting the hour and count shows us rentals spike around 8am, and also around 5pm
  #this can likely be attributed to people starting to bike to work in the morning, and then biking back home
  #in the evening

#let's see a boxplot
ggplot(data=hour, aes(x=hr, y=cnt, color=hr))+geom_boxplot()
#we can see there are a number of observations that lie past the top whisker for some months

#january
ggplot(data=jan)+geom_bar(aes(x=hr, y=cnt), fill="deepskyblue", stat="identity")+
  labs(title = "Total Bike Rentals by Hour", x = "Hour", y = "Count")+
  theme(plot.title = element_text(hjust=0.5), axis.title.y = element_text(angle = 0, vjust = 0.5))

ggplot(data=jan, aes(x=hr, y=cnt, color=hr))+geom_boxplot()

#april
ggplot(data=apr)+geom_bar(aes(x=hr, y=cnt), fill="deepskyblue", stat="identity")+
  labs(title = "Total Bike Rentals by Hour", x = "Hour", y = "Count")+
  theme(plot.title = element_text(hjust=0.5), axis.title.y = element_text(angle = 0, vjust = 0.5))

ggplot(data=apr, aes(x=hr, y=cnt, color=hr))+geom_boxplot()

#july
ggplot(data=jul)+geom_bar(aes(x=hr, y=cnt), fill="deepskyblue", stat="identity")+
  labs(title = "Total Bike Rentals by Hour", x = "Hour", y = "Count")+
  theme(plot.title = element_text(hjust=0.5), axis.title.y = element_text(angle = 0, vjust = 0.5))

ggplot(data=jul, aes(x=hr, y=cnt, color=hr))+geom_boxplot()

#october
ggplot(data=oct)+geom_bar(aes(x=hr, y=cnt), fill="deepskyblue", stat="identity")+
  labs(title = "Total Bike Rentals by Hour", x = "Hour", y = "Count")+
  theme(plot.title = element_text(hjust=0.5), axis.title.y = element_text(angle = 0, vjust = 0.5))

ggplot(data=oct, aes(x=hr, y=cnt, color=hr))+geom_boxplot()


#SUMMARY: We can see that rentals are higher for hotter months on average, but plotting the
#months individually shows that the peak rental hours are consistent across each month (8am, 5pm)
#Also, we see that that the outliers exist only for non-peak hours; there are no
#outliers for peak hours. Since the number of outliers for each
#hour of a given month are not abundant, we may be able to just delete these?

#NOTE: we may have to rearrange the months so that the relationship looks more linear

#Workingday variable
hour$workingday = as.factor(hour$workingday) #change to factor
plot(hour$workingday, hour$cnt) #median rentals are about the same whether it's a workday or not
#delete this variable?

#weathersit variable
hour$weathersit = as.factor(hour$weathersit) #convert to factor
plot(hour$weathersit, hour$cnt)
#we can see that rentals are higher at (1) and (2) -- good weather -- and lower at (3) & (4)
#-- create heatmap
#aggregate by month to get clearer relationship (see less outliers)
#split the variable between 1:3, 4

#registered and casual variable -- we are trying to predict the total rental count, not by
#membership status. The ratio of average registered count to average casual count is about 4:1.
#remove these two variables

#HOW DOES TEMPERATURE DIFFER ACROSS SEASONS?

spring = subset(hour, season == 1) #create subset by season
describe(spring$raw.temp) #using psych package to get descriptive stats

#vars    n  mean   sd median trimmed  mad  min   max range skew kurtosis   se
#X1    4242 12.27 4.87  11.48   11.97 4.86 0.82 29.52  28.7 0.59     0.34 0.07

summer = subset(hour, season == 2)
describe(summer$raw.temp)

#vars    n  mean   sd median trimmed  mad  min   max range  skew kurtosis   se
#X1    4409 22.33 5.71  22.96   22.43 6.08 6.56 38.54 31.98 -0.15    -0.38 0.09

fall = subset(hour, season == 3)
describe(fall$raw.temp)

#vars    n  mean   sd median trimmed  mad   min max range  skew kurtosis   se
#X1    4496 28.96 3.85   28.7   28.95 3.65 15.58  41 25.42 -0.01      0.1 0.06

winter = subset(hour, season == 4)
describe(winter$raw.temp)

#vars    n  mean   sd median trimmed  mad  min   max range skew kurtosis   se
#X1    4232 17.35 5.01  17.22    17.2 6.08 5.74 31.16 25.42 0.25     -0.7 0.08

#Plot Histograms for each season

#Spring
ggplot(data=spring, aes(raw.temp))+geom_histogram(bins=12, col="black", fill="white")+
  geom_vline(xintercept = mean(spring$raw.temp), col="red", linetype="dotted", size=1.5)+
  geom_vline(xintercept = median(spring$raw.temp), col="blue", size=1.5)+
  labs(title = "Temperature during Spring Days", x="Temperature", y="Number of Days")+
  theme(plot.title = element_text(hjust = 0.5))

#Summer
ggplot(data=summer, aes(raw.temp))+geom_histogram(bins=12, col="black", fill="white")+
  geom_vline(xintercept = mean(summer$raw.temp), col="red", linetype="dotted", size=1.5)+
  geom_vline(xintercept = median(summer$raw.temp), col="blue", size=1.5)+
  labs(title = "Temperature during Summer Days", x="Temperature", y="Number of Days")+
  theme(plot.title = element_text(hjust = 0.5))

#Fall
ggplot(data=fall, aes(raw.temp))+geom_histogram(bins=12, col="black", fill="white")+
  geom_vline(xintercept = mean(fall$raw.temp), col="red", linetype="dotted", size=1.5)+
  geom_vline(xintercept = median(fall$raw.temp), col="blue", size=1.5)+
  labs(title = "Temperature during Fall Days", x="Temperature", y="Number of Days")+
  theme(plot.title = element_text(hjust = 0.5))

#Winter
ggplot(data=winter, aes(raw.temp))+geom_histogram(bins=12, col="black", fill="white")+
  geom_vline(xintercept = mean(winter$raw.temp), col="red", linetype="dotted", size=1.5)+
  geom_vline(xintercept = median(winter$raw.temp), col="blue", size=1.5)+
  labs(title = "Temperature during Winter Days", x="Temperature", y="Number of Days")+
  theme(plot.title = element_text(hjust = 0.5))

#IS THERE A CORRELATION BETWEEN REAL TEMPERATURE AND COUNT, AND FEELING TEMP AND COUNT?

#plot the relationship between temperature and count
plot(hour$raw.temp, hour$cnt) #too cluttered -- create a new dataframe aggregated by days and includes
#avg daily temps and sum of cnt
days = aggregate(. ~dteday, data=hour, mean, na.rm=TRUE)
days = data.frame(days$dteday, days$raw.temp, days$raw.atemp)
days_cnt = aggregate(. ~dteday, data=hour, sum, na.rm=TRUE)
days = data.frame(days, days_cnt$cnt) #new dataframe grouped by date, containing avg temps, and summed cnt
colnames(days)=c("dteday","raw.temp","raw.atemp","cnt") #rename columns to be consistent

plot(days$raw.temp, days$cnt) #scatterplot shows us a positive relationship b/w temp and cnt
plot(days$raw.atemp, days$cnt) #positive relationship b/w atemp and cnt

cor(days$raw.temp, days$cnt) #actual temp vs cnt
#[1] 0.627494 -- moderate-strong correlation

cor(days$raw.atemp, days$cnt) #feeling temp vs cnt
#[1] 0.6310656 -- moderate-strong correlation

#temp vs cnt
#remove holiday/weekday/year
#remove 2012-10-29 and 30

#plot real temp and feeling temp together on the x-axis on a scatterplot
ggplot(data=day, aes(y=cnt))+geom_point(aes(x=raw.temp, color="red"))+
  geom_point(aes(x=raw.atemp, color="blue"))+
  scale_colour_manual(name="Line Color", values=c(red="red", blue="blue"))
#get legend titles, get axis titles

#############################################################################
#                           FEATURE ENGINEERING
#############################################################################

#1) create new column that splits "hour" into peak and non-peak times

hour$rush = 0
hour$rush[hour$hr == 6 | hour$hr == 7 | hour$hr == 8] = 1
hour$rush[hour$hr == 16 | hour$hr == 17 | hour$hr == 18] = 1

#############################################################################
#                               MODELING
#############################################################################

forest = randomForest(cnt ~ raw.temp+raw.atemp+season+hr+mnth+weathersit+workingday+rush+weekday, data=hour, importance=TRUE, ntree=200)
#call forest
varImpPlot(forest) #we see that workingday, hr, weathersit, and raw.atemp are the most important

row_count <- nrow(hour)
shuffled_rows <- sample(row_count)
train <- hour[head(shuffled_rows,floor(row_count*0.80)),]
test <- hour[tail(shuffled_rows,floor(row_count*0.20)),]

train.rows = createDataPartition(y= hour$cnt, p=0.8, list = FALSE)
train.data<- hour[train.rows,] # 80% data goes in here
test.data<- hour[-train.rows,] # 20% data goes in here

forest = randomForest(cnt ~ raw.temp+raw.atemp+season+hr+mnth+weathersit+workingday+rush+weekday, data=train.data, importance=TRUE, ntree=200)
forest = randomForest(cnt ~., data=train.data, importance=TRUE, ntree=200)

varImpPlot(forest) #we see that workingday, hr, weathersit, and raw.atemp are the most important


test.lm = lm(formula = cnt ~ hr+raw.atemp+weathersit+weekday+raw.temp+casual, data = train.data)
test.lm = lm(formula = cnt ~., data = train.data)
summary(test.lm)
prediction = predict(test.lm, test.data)
cor(test.data$cnt, prediction)

test$outcome = 0
predict.forest <- predict(forest, test.data)
prediction2 = predict(test.lm, test)
cor(test$cnt, prediction)
plot(test$cnt, prediction)
cor(test$cnt, prediction2)
plot(test$cnt, prediction2)
View(prediction)
summary(prediction2)

train_control = trainControl(method="cv", number=10)
model = train(cnt ~., data=train.data, trControl = train_control, method='rf')
print(model)
prediction3 = predict(model, test.data)
cor(test.data$cnt, prediction3)
plot(test.data$cnt, prediction3)
data.frame(test.data$cnt, prediction3)


plot(test.lm)
plot(day$raw.windspeed, day$cnt)
