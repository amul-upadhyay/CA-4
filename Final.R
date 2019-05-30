# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

## Initialization and Data Reads
install.packages("lubridate")
install.packages("GGally")
install.packages("ggplot2")
install.packages("hydroGOF")
install.packages("mvtnorm")

library(lubridate)
library(GGally)
library(ggplot2)
library(hydroGOF)
library(mvtnorm)

# Reading Unemployemnt dataset and renaming its two columns 

unemployment_df <- read.csv("Unemployment Rate.csv", header = , skip = 4)
names(unemployment_df)[1] <- "year"
names(unemployment_df)[2] <- "unemployment_rate"

# Rading property price dataset  and removing NA's while importing

ppr_df <- read.csv("ppr_data_encoded.csv", header = TRUE,na.strings=c(""," ","NA"))


# Merging both datasets by year so that in our ppr_df we have an extra column named unemployment_rate
ppr_df <-  merge(ppr_df, unemployment_df, by = "year")


# Printing top 6 rows
head(ppr_df)

# Checking Structure of our final dataset
str(ppr_df)


ppr_df$electoral_district_id <- as.numeric(ppr_df$electoral_district_id)

sort(table(ppr_df$ppr_county))


# As the property prices dat I have is downloaded from gov.ie and is the only dataset available about property prices in Ireland, 
# The data does not have any numerical field a=except price This surely makes this dataset suboptimal for machine leaning and analysis.
# I had to use python to get the lat long of the address being mentioned in the data and sunsequent lat loong column
# is added in the dataset.
# Nevertheless we can take out some important information with the available data.


# Listing all the counties by the property sold
# We can see that dublin is the county where highest number of properties are sold
table(ppr_df$ppr_county)

# Extracting rows where county name is equal to Dublin
# This is done so that our analysis should be focused 

ppr_df <- ppr_df[which(ppr_df$ppr_county == "Dublin"),]

# After extracting county we will narrow down our dataset to "New Dwelling"
# this is done because second hand house prices dont follow any pattern and price varies with negotiation.

ppr_df <- ppr_df[which(ppr_df$description_of_property == "New Dwelling house /Apartment"),]

# We dnt have much numerical data availabe to us in this particular dataset
# Henc ewe will change a column named 'property_size_description' to numerical
# In property_size_description column there are numerical values in the line'
# Extracting them and renaming the column we can have a numeric column

ppr_df$property_size_description <- as.character(ppr_df$property_size_description)

colnames(ppr_df)[colnames(ppr_df)=="property_size_description"] <- "area_greater_than"

ppr_df$area_greater_than[ppr_df$area_greater_than == "greater than or equal to 38 sq metres and less than 125 sq metres"] <- "38"

ppr_df$area_greater_than[ppr_df$area_greater_than == "greater than or equal to 125 sq metres"] <- "125"

ppr_df$area_greater_than[ppr_df$area_greater_than == "greater than 125 sq metres"] <- "125"

ppr_df$area_greater_than <- as.numeric(ppr_df$area_greater_than)

str(ppr_df) 


# Initially we had no numerical columns other than price and so far using python to extract latitude and longitude
# Adding Unemployment dataset
# adiing electoral Ir
# and formating 'proeprty_size_discription' gives rise to few numerical columns 
# so that we can perform correlation and further analysis on it

table(ppr_df$area_greater_than)

# Splitting the Data Set 
ratio = sample(1:nrow(ppr_df), size = 0.25*nrow(ppr_df))

Test = ppr_df[ratio,] #Test dataset 25% of total
dim(Test)# 6483 21
str(Test)
Training = ppr_df[-ratio,] #Train dataset 75% of total
dim(Training)#15130  21
str(Training)
table(is.na(Training)) # To check whether the data in train dataset is missing or not



## Checking Relationship between price, area_greater_than
str(Training)
plot1<-ggpairs(data=Training, columns=c(8,12),
               mapping = aes(color = "dark green"),
               axisLabels="show")
plot1

## Checking Relationship between price,latitude
plot2<-ggpairs(data=Training, columns=c(8,16),
               mapping = aes(color = "dark green"),
               axisLabels="show")
plot2

## Checking Relationship between price,longitude
plot3=ggpairs(data=Training, columns=c(8,17),
              mapping = aes(color = "dark green"),
              axisLabels="show")
plot3

## Checking Relationship between price,Electoral ID
plot4=ggpairs(data=Training, columns=c(8,22),
              mapping = aes(color = "dark green"),
              axisLabels="show")
plot4

## Checking Relationship between price and Unemployment
plot5=ggpairs(data=Training, columns=c(8,25),
              mapping = aes(color = "dark green"),
              axisLabels="show")
plot5
str(Training)
############################################################################################################

## From Plot 1, it seems that price vs sqft_living is a nice fit and there are not that many variations compared to others. Diagonal plot shows a nice right squewed normal distribution, which I cannot see in other variables. I can say similar stuff for bathrooms as well but it is more distorded. I would say I would pick 2 variables : sqft_living and bathrooms from plot 1.

## From Plot 2,I can say view and grades are the best ones that explains price. Grade is a perfect normal ditsribution fit and view is more like a right skewed normal distribution (speaking of diagonal plots). other elements have more than 1 peaks therefore not good for modelling. So from Plot 2 I would pick grade and view as my variables

## From Plot 3, I would probaibly pick either lat or long because both seems to be nicely representing the data. One is right skewed normal dist, other one is left skewed normal dist. I would pick variable lat from this plot. 

## So my final 5 variables are: sqft_living, bathrooms, grade, view and lat. Lets verify this using box plots:

## Price vs. Sqft_living ->> Nice correlation, as sqft increases, price increases as well.
boxplot1=boxplot(price~area_greater_than, data=Training, 
                 col=(c("gold","darkgreen")),
                 main="price~area_greater_than", xlab="Area", ylab="Price")

## Price vs. Bathrooms ->> Nice correlation, as # of bahtrooms increases [median of bar plot], price increases as well, with one expection in when bathroom=7
boxplot2=boxplot(price~latitude, data=Training, 
                 col=(c("gold","darkgreen")),
                 main="price~latitude", xlab="latitude", ylab="Price")

## Price vs. Grade ->> Nice correlation, grade increases [median of bar plot], price increases as well
boxplot3=boxplot(price~longitude, data=Training, 
                 col=(c("gold","darkgreen")),
                 main="price~longitude", xlab="longitude", ylab="Price")

## Price vs. View ->> Nice correlation, view increases [median of bar plot], price increases as well
boxplot4=boxplot(price~unemployment_rate, data=Training, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. View", xlab="View", ylab="Price")

## Price vs. Lat ->> This is more like a normal dist relationship, price peaks around when lat= 47.64 and declines afterwards, but this can be modeled easily. I would say Lat explains the price as well.
boxplot5=boxplot(price~electoral_district_id, data=Training, 
                 col=(c("gold","darkgreen")),
                 main="Price vs. Lat", xlab="Lat", ylab="Price")

## Each of those box plots shows that those variables might be directly related in predicting house prices.

## To strengthen my hypothesis I also computed correlation between prices and variables, and my top 5 picks are supported with correlation coefficients as well [see below]

## Plots 1,2 and 3 shows the correlation between each variables and they are:
# corr between price vs area_greater_than: 0.098
# corr between price vs latitude: -0.00578
# corr between price vs latitude: 0.00192
# Corr between price and electoral_district_ID = 0.028
# Corr between price and unemplyment_rate = -0.0721


############################################################################################################
## I want to use the predictor area greater than for predicting house prices.

## Question's model claims that error is Normally distributed. But after scatterplot it says model is incorrect. Below see the first scatter plot: 

plot(Training$area_greater_than,Training$price, main="area greater than vs. Price of House", xlab="area greater than", ylab="Price of House", pch=19)

## Since this scatterplot is too crowded - I will plot aggregated vectors to see the relationship between 2 variables. 

vec_price_sqftliving <- aggregate(price~area_greater_than, data=Training, FUN = median)
plot(vec_price_sqftliving, col = "green")
scatterplot1<-recordPlot()




Model1 <- lm(data=Training,price~area_greater_than)
Model2 <- lm(data=Training,log(price)~log(area_greater_than))
Model3 <- lm(data=Training,price~unemployment_rate)
Model4 <- lm(data=Training,log(price)~log(unemployment_rate))


Beta0_Model1<-coef(Model1)[1]
Beta1_Model1<-coef(Model1)[2]
Beta0_Model2<-coef(Model2)[1]
Beta1_Model2<-coef(Model2)[2]
R_Squared_Model1<-summary(Model1)$r.squared
R_Squared_Model2<-summary(Model2)$r.squared

cat("Model1 coefficients and R-Squared:\nBeta0:",Beta0_Model1,"\nBeta1:",Beta1_Model1,"\nR-squared:",R_Squared_Model1)
cat("Model2 coefficients and R-Squared:\nBeta0:",Beta0_Model2,"\nBeta1:",Beta1_Model2,"\nR-squared:",R_Squared_Model2)

## Sub Part III

## I would compute MSE (Mean Squared Errors) to compare two different models.First I need to compute price_hats for my test data using the Model 1 and Model 2 coefficients. Then using mse function of package hydroGOF, I compute MSEs for my models.
price_hat_Model1<-predict(Model1,newdata=Test) ##Prediction using Model1
price_hat_Model2<-exp(predict(Model2,newdata=Test)) ##Prediction using Model2- notice that I had to take exponent of predict function because MODEL 2 returns log of predicted value.

MSE_Model1=mse(price_hat_Model1,Test$price) ## computing MSE for Model 1
MSE_Model2=mse(price_hat_Model2,Test$price) ## computing MSE for Model 2

cat("MSE for Model1:",MSE_Model1,"\nMSE for Model2:",MSE_Model2)

cat("MSE for Model 2 is ",round(100*(MSE_Model2/MSE_Model1-1),2),"% more than Model 1. Therefore I can safely suggest that Model 1 is better than Model 2.")

############################################################################################################

## Best Subset Selection Method: We have suggested 12 variables. For each of P choose 12 subsets, I will calculate SSE and see which one gives me a smaller SSE and I pick that variable. To ease our computation, it is suggested to use subset size 1.

## Suggested variables:  bedrooms, bathrooms, log(sqft living), log(sqft lot), floors, waterfront, view, condition, grade, yr built, lat, long

## Creating Models using 1 variables each so total 12 Models. 
Model_PartE_1<-lm(log(price)~area_greater_than,data=Training)
Model_PartE_2<-lm(log(price)~electoral_district_id,data=Training)
Model_PartE_3<-lm(log(price)~log(area_greater_than),data=Training)
Model_PartE_4<-lm(log(price)~longitude,data=Training)
Model_PartE_5<-lm(log(price)~unemployment_rate,data=Training)
Model_PartE_6<-lm(log(price)~latitude,data=Training)


## Predicting prices using each Model. Please note that I have to take exponent of predict function since it returns log of price.
price_hat_PartE_1<-exp(predict(Model_PartE_1,newdata=Test))
price_hat_PartE_2<-exp(predict(Model_PartE_2,newdata=Test))
price_hat_PartE_3<-exp(predict(Model_PartE_3,newdata=Test))
price_hat_PartE_4<-exp(predict(Model_PartE_4,newdata=Test))
price_hat_PartE_5<-exp(predict(Model_PartE_5,newdata=Test))
price_hat_PartE_6<-exp(predict(Model_PartE_6,newdata=Test))


## Computing SSE for each variable Models.
SSE_PartE_1<-sum((Test$price-price_hat_PartE_1)^2)
SSE_PartE_2<-sum((Test$price-price_hat_PartE_2)^2)
SSE_PartE_3<-sum((Test$price-price_hat_PartE_3)^2)
SSE_PartE_4<-sum((Test$price-price_hat_PartE_4)^2)
SSE_PartE_5<-sum((Test$price-price_hat_PartE_5)^2)
SSE_PartE_6<-sum((Test$price-price_hat_PartE_6)^2)


## Finding variable with min SSE
SSE<-c(SSE_PartE_1,SSE_PartE_3,SSE_PartE_4)
which(SSE==min(SSE)) ## SSE number 9 is the minimim SSE which is variable grade so it is the best predictor.
SSE_PartE_1


############################################################################################################


# Using Scatter Plots for unemplyment rate vs Price

plot(Training$unemployment_rate,log(Training$price), main="unemployment vs. Log Price of House", xlab="unemployment", ylab="Log Price of House", pch=19)
plot(log(Training$unemployment_rate),log(Training$price), main="log unemployment rate vs. Log Price of House", xlab="Log Unemployment", ylab="Log Price of House", pch=19)

## I think we could take log of uneployment and get better performance.

# Using Scatter Plots for area vs Price:

plot(Training$area_greater_than,log(Training$price), main="area greater than vs. Log Price of House", xlab="area greater thanooms", ylab="Log Price of House", pch=19)
plot(log(Training$area_greater_than),log(Training$price), main="area greater than vs. Log Price of House", xlab="Log area greater than", ylab="Log Price of House", pch=19)

## I think we could take log of bahtroom and get better performance.


# Using Scatter Plots for lat vs Price:

plot(Training$latitude,log(Training$price), main="Grade vs. Log Price of House", xlab="Grade", ylab="Log Price of House", pch=19)
plot(log(Training$latitude),log(Training$price), main="Log Grade vs. Log Price of House", xlab="Log Grade", ylab="Log Price of House", pch=19)

## I think grade is good as it is, we should keep it in the current form.

## Sub Part III
Model3<-lm(log(price)~log(area_greater_than)+unemployment_rate,data=Training)
summary(Model3)

## Assigning coefficients and R-squared
Beta0_Model3<-coef(Model3)[1]
Beta1_Model3<-coef(Model3)[2]
Beta2_Model3<-coef(Model3)[3]
Beta3_Model3<-coef(Model3)[4]
Beta4_Model3<-coef(Model3)[5]
Beta5_Model3<-coef(Model3)[6]
R_Squared_Model3<-summary(Model3)$r.squared

cat("Model3 coefficients and R-Squared:\nBeta0:",Beta0_Model3,"\nBeta1:",Beta1_Model3,"\nBeta2:",Beta2_Model3,"\nBeta3:",Beta3_Model3,"\nBeta4:",Beta4_Model3,"\nBeta5:",Beta5_Model3,"\nR-squared:",R_Squared_Model3)

cat("R-Squared for Model 3 is ",100*(R_Squared_Model3/R_Squared_Model2-1),"% better than Model 2.\nR-squared for Model 3 and Model 2 are:",R_Squared_Model3,"and",R_Squared_Model2,"respectively.")

## Sub Part IV

## Model 2 has higher bias and Model 3 has higher variance. As we increase the number of independent variables in our models, we increase the accuracy therefore we decrease the bias. However, at the same time, we increase the variance. To get the optimal model, we need to take a look at bias-variance trade off and select the model with minimum residual errors. Model 2 is too simple and it is likely to produce more errors, R-squared also supports this claim, Model 2 explains less % of data than Model 3 [comparing R-squareds]. We need to make sure not to overfit the data also.


## Sub Part V

## As we used earlier in part D section iii, MSE is a good method to compare different models. I already compute MSE for Model 2 which is:
MSE_Model2

## Now I need to compute MSE for Model 3

price_hat_Model3<-exp(predict(Model3,newdata=Test)) ##Prediction using Model3- notice that I had to take exponent of predict function because MODEL 3 returns log of predicted value

MSE_Model3=mse(price_hat_Model3,Test$price) ## computing MSE for Model 3

cat("MSE for Model2:",MSE_Model2,"\nMSE for Model3:",MSE_Model3)

cat("MSE for Model 2 is ",round(100*(MSE_Model2/MSE_Model3-1),2),"% more than Model 3. Therefore I can safely suggest that Model 3 is better than Model 2.")


## Sub Part VI

## We can use built in confint() function for CI 90%

confint(Model3,level=0.9)
CI90_Beta0_Model3<-c(confint(Model3,level=0.9)[1],confint(Model3,level=0.9)[,2][1])
CI90_Beta1_Model3<-c(confint(Model3,level=0.9)[2],confint(Model3,level=0.9)[,2][2])
CI90_Beta2_Model3<-c(confint(Model3,level=0.9)[3],confint(Model3,level=0.9)[,2][3])
CI90_Beta3_Model3<-c(confint(Model3,level=0.9)[4],confint(Model3,level=0.9)[,2][4])
CI90_Beta4_Model3<-c(confint(Model3,level=0.9)[5],confint(Model3,level=0.9)[,2][5])
CI90_Beta5_Model3<-c(confint(Model3,level=0.9)[6],confint(Model3,level=0.9)[,2][6])

cat("90% CI for coefficients:","\nBeta0:",CI90_Beta0_Model3,"\nBeta1:",CI90_Beta1_Model3,"\nBeta2:",CI90_Beta2_Model3,"\nBeta3:",CI90_Beta3_Model3,"\nBeta4:",CI90_Beta4_Model3,"\nBeta5:",CI90_Beta5_Model3)

## Sub Part VII
## Creating Beta1 and Beta 2 sd s via summary function

Beta1_Model3_sd<-coef(summary(Model3))[,2][2]
Beta2_Model3_sd<-coef(summary(Model3))[,2][3]

## qnorm gives percentage based on shaded left area so I want 
t_value_b1<-qnorm(1-0.025/2,mean=0,sd=Beta1_Model3_sd) 
t_value_b2<-qnorm(1-0.025/2,mean=0,sd=Beta2_Model3_sd) 

CI95_Beta1_Model3low<-Beta1_Model3-t_value_b1
CI95_Beta1_Model3high<-Beta1_Model3+t_value_b1
CI95_Beta2_Model3low<-Beta2_Model3-t_value_b2
CI95_Beta2_Model3high<-Beta2_Model3+t_value_b2

cat("So confidence region for 95% is [",CI95_Beta1_Model3low,",",CI95_Beta1_Model3high,"]*[",CI95_Beta2_Model3low,",",CI95_Beta2_Model3high,"]")

############################################################################################################
## If we plot residual vs. a variable that is not used in the prediction and if we see any recognizable patterns, then we can say that some of the variation in residual can be actually explained by the non-used variable therefore we should include it in our model to reduce the residual errors. Therefore we can say that this approach is a very good idea.


############################################################################################################
## To calculate residuals, we simply need to substract price_hat_Model3 from the actual price.
residual_Model3=Test$price-price_hat_Model3

plot(Test$area_greater_than,residual_Model3) ## Residual vs. sqft_lot

plot(Test$unemployment_rate,residual_Model3) ## Residual vs. floors

plot(Test$latitude,residual_Model3) ## Residual vs. view

plot(Test$longitude,residual_Model3) ## Residual vs. condition

plot(Test$electoral_district_id,residual_Model3) ## Residual vs. sqft_above

############################################################################################################

## Sub Part I


plot(Test$yr_built,residual_Model3) ## Residual vs. yr_built
plot(Test$lat,residual_Model3) ## Residual vs. lat

## I would include them by indexing them. So yr_built starts from 1900 and ends at 2015. I would use the following equation: 

## Age = 2015-yr_built +1 

#I want to start the age from 1 because in case I take the log function, I dont want to have log(0) since it is -infinity. I would do the same for lat also: 

## LatOpt= lat- min(lat) +1  

##this way I would start my lat from 1. Then I may perform a log transformation. It is hard to tell which one is more effective without actually computing this.

## Sub Part II

Model4<-lm(log(price)~log(area_greater_than)+lattude,data=Training)
summary(Model4)

## Assigning coefficients and R-squared
Beta0_Model4<-coef(Model4)[1]
Beta1_Model4<-coef(Model4)[2]
Beta2_Model4<-coef(Model4)[3]
Beta3_Model4<-coef(Model4)[4]
Beta4_Model4<-coef(Model4)[5]
Beta5_Model4<-coef(Model4)[6]
Beta6_Model4<-coef(Model4)[7]
Beta7_Model4<-coef(Model4)[8]
R_Squared_Model4<-summary(Model4)$r.squared

cat("Model4 coefficients and R-Squared:\nBeta0:",Beta0_Model4,"\nBeta1:",Beta1_Model4,"\nBeta2:",Beta2_Model4,"\nBeta3:",Beta3_Model4,"\nBeta4:",Beta4_Model4,"\nBeta5:",Beta5_Model4,"\nBeta6:",Beta6_Model4,"\nBeta7:",Beta7_Model4,"\nR-squared:",R_Squared_Model4)

cat("R-Squared for Model 4 is ",100*(R_Squared_Model4/R_Squared_Model3-1),"% better than Model 3.\nR-squared for Model 4 and Model 3 are:",R_Squared_Model4,"and",R_Squared_Model3,"respectively.")

## Sub Part III
## We already computed MSE for Model 3:
MSE_Model3

## Let's compute MSE for Model 4:
price_hat_Model4<-exp(predict(Model4,newdata=Test)) ##Prediction using Model4- notice that I had to take exponent of predict function because MODEL 4 returns log of predicted value.

MSE_Model4=mse(price_hat_Model4,Test$price) ## computing MSE for Model 4

cat("MSE for Model3:",MSE_Model3,"\nMSE for Model4:",MSE_Model4)

cat("MSE for Model 3 is ",round(100*(MSE_Model3/MSE_Model4-1),2),"% more than Model 4. Therefore I can safely suggest that Model 4 is better than Model 3. So Model 4 predicts the prices better.")

############################################################################################################
## Residual vs. Zipcode 
boxplot_PartI=boxplot(residual_Model3~Test$area_greater_than,
                      col=(c("gold","darkgreen")),
                      main="Residual vs. Zipcode", xlab="Zipcode", ylab="Residual")

logboxplot_PartI=boxplot(residual_Model3~log(Test$unemployment_rate), 
                         col=(c("gold","darkgreen")),
                         main="Residual vs. Log Zipcode", xlab="Log Zipcode", ylab="Residual")

## I tried both regular and log zip codes and they both seems to have same patterns so I will just include zip code directly without transformation.

Model5<-lm(log(price)~log(area_greater_than),data=Training)
summary(Model5)

cat("R-squared for Model5 is:",summary(Model5)$r.squared)

## Let's compute MSE for Model 5:
price_hat_Model5<-exp(predict(Model5,newdata=Test)) ##Prediction using Model5- notice that I had to take exponent of predict function because MODEL 5 returns log of predicted value.

MSE_Model5=mse(price_hat_Model5,Test$price) ## computing MSE for Model 5

cat("MSE for Model4:",MSE_Model4,"\nMSE for Model5:",MSE_Model5)

cat("MSE for Model 4 is ",round(100*(MSE_Model4/MSE_Model5-1),2),"% more than Model 5. Therefore I can safely suggest that Model 5 is better than Model 4.")

