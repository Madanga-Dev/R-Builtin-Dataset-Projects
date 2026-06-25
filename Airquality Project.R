# Airquality Data Analysis in R
install.packages("naniar")
install.packages("corrplot")
install.packages("ggplot2")
#1. Load Dataset
data(airquality)
head(airquality)


#2. Dataset Structure
str(airquality)

dim(airquality)

names(airquality)

summary(airquality)

# 3 Check missing values
colSums(is.na(airquality))


#Visualize Missing values using library(naniar), If error install.packages("naniar")
library(naniar)
gg_miss_var(airquality)

which(is.na(airquality$Ozone))


## 4 Handle Missing Values

## Option 1 : Remove Missing Values
airquality_clean<-na.omit(airquality)

dim(airquality_clean)

#5. Descriptive Statistics

mean(airquality_clean$Ozone)

median(airquality_clean$Ozone)

max(airquality_clean$Ozone)

min(airquality_clean$Ozone)


sd(airquality_clean$Ozone)

## Part 3

## Data Visualization 

# 6 . Histogram of Ozone 
hist(airquality_clean$Ozone, main="Distribution of Ozone Concetration", xlab = "Ozone", col = "skyblue")

# 7. BoxPlot of the Ozone
boxplot(airquality_clean$Ozone, main= "Ozone Levels", col = "orange")


#8. ScatterPlot between Temperature and Ozone
plot(airquality_clean$Temp, airquality_clean$Temp, main="Temperature vs Ozone Concentration", xlab="Temperature", ylab = "Ozone Concentration", pch=19,col="blue") 
abline(lm(Ozone~Temp,data=airquality_clean), col="red", lwd=2)

#Part 4 
# 9 Correlation Analysis
cor(airquality_clean$Temp, airquality_clean$Ozone)

# 10. Full Correlation Matrix
cor(airquality_clean[,1:4])

library(corrplot)     
corrplot(cor(airquality_clean[,1:4]),method="circle")

#Part 5 
# 11. Monthly Air Quality Trends (Average Ozone by Month)

monthly_Ozone<-aggregate(Ozone~Month, data=airquality_clean, mean)

monthly_Ozone
barplot(monthly_Ozone$Ozone, names.arg = monthly_Ozone$Month, col = "darkgreen",main = "Avarage Ozone by Month")


library(ggplot2)
ggplot(airquality_clean,aes(Wind,Ozone))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_minimal()+
  labs(title="Wind Speed vs Ozone")
