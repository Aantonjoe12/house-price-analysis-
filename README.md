# house-price-analysis-
R-code performing EDA &amp; regression models on real estate valuation data. 

install.packages("readx1")
install.packages("moments")
install.packages("ggplot2")
library(readxl)

data<-read_excel("Zoom/Real estate valuation data set EDA task .xlsx")
#used the readx1 package and read_excel command to bring the excel data into r

colnames(data)<- c("No","Transaction_date","House_age","Distance_MRT",
                   "Convenience_Stores","Latitude","Longitude","Price")
#changed the names of the columns to make it more concise and easy to work with

colSums(is.na(data))
#double check that there are no missing data by asking to check for values that are not available

data$Price<- data$Price/3.3
data$Price
#changed the units from 10000NTD/ping into 10000NTD/m^2 using conversion Price here is price per meter square 

data<- data[,-c(6, 7)]
str(data)
#check we have removed the unwanted columns
#removed the latitude and longitude as I want to focus on mainly the House_age, Distance_MRT and Convenience_store and 
#compare these 3 variables to the house price.

head(data)
#used to quickly view the first few data values to check that
#the data is loaded in properly
str(data)
#used to check the type of data we are working with.
summary(data)
#returns a summary of key statistics 

#Price
library(ggplot2)
ggplot(data, aes(x=Price))+ geom_histogram(binwidth = 1, 
fill="steelblue")+labs(title="Price_distrubution",x="Price (10000NTD/M^2)")
SKV_Price<-skewness(data$Price)
print(SKV_Price)


#House age

ggplot(data, aes(x=House_age))+ geom_histogram(binwidth = 1, 
fill="steelblue")+labs(title="House_age_distrubution")
#used ggplot2 package to create a histogram showing the distribution of the 
#house ages
std_dev_house_age<-sd(data$House_age)
print(std_dev_house_age)
sapply(data,sd)# used sapply function to calculate the standard deviation 
#for all the rows 

library( "moments")
SKV_House_age<-skewness(data$House_age)#SKV means Skewness Value
print(SKV_House_age) 
#used moments package to double check the skewness numerically.
#this could have been done manually however using the package allows us to
#be more efficient when looking at skewness with the other columns.

ggplot(data, aes(x= House_age, y=Price))+
  geom_point(colour="steelblue")+
  labs(title="Price against House_age",
       x="House age",
       y="Price(10000NTD/M^2)")+
  geom_smooth(method= "lm", col="red")
#used ggplot here to create a scatter plot of House_age against Price 
#then added a line of best fit to see the weak negative trend that appears.


#Distance MRT
ggplot(data, aes(x=Distance_MRT))+ geom_histogram(binwidth = 200,
fill="steelblue")+labs(title = "Distance_MRT_distrubution")
SKV_Distance_MRT<-skewness(data$Distance_MRT)
print(SKV_Distance_MRT)

ggplot(data, aes(x= Distance_MRT, y=Price))+
  geom_point(colour="steelblue")+
  labs(title="Price against distance to nearest MRT station",
       x="Distance to nearest MRT station(M)",
       y="Price(10000NTD/M^2)")+
  geom_smooth(method= "lm", col="red")



#Convenience_Stores
ggplot(data, aes(x=Convenience_Stores))+ 
  geom_bar(fill="steelblue",colour="black")+
  theme_minimal()+
labs(title= "Bar plot of Convenience_Stores", x="No. of convenience stores",
     y="count")
#here i used a bar plot as store data can be treated as categorical
100-((67/414)*100)# shows the percentage of houses that have at least 1 store

ggplot(data, aes(x= factor(Convenience_Stores), y=Price))+
  geom_boxplot(colour="steelblue")+
  labs(title="Price against no.of convenience stores",
       x="No. of convenience stores",
       y="Price(10000NTD/M^2)")+
  theme_minimal()
#used a box plot to show the relationship between a
#continous variable and categorical

model<-lm(Price ~ House_age + Distance_MRT + Convenience_Stores ,data=data)
summary(model)
#created a model using the 3 predictors
model2<-lm(Price~Distance_MRT + Convenience_Stores , data=data )
summary(model2)
#created a second model using only 2 predictors 
             
