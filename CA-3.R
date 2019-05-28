
# Locading The needed Librabries


library(MASS)
library(plyr)
library(ggplot2)
library(qplot)

#Reading Propert price.csv file and unemployment.csv file

ppr_df <- read.csv("Property Prices.csv", header = TRUE)

unemployment_df <- read.csv("Unemployment Rate.csv", header = , skip = 4)

# Printing the top 5 entries of both dataframes
head(ppr_df)
head(unemployment_df)

#Assigning Column names to Unemployemnt dataframe

names(unemployment_df)[1] <- "Year"
names(unemployment_df)[2] <- "Unemployment_Rate"

#Assigning Column names to ppr_df dataframe



# Dropping Unneccessary columns

ppr_df <- read.csv("Property Prices.csv", header = TRUE,na.strings=c(""," ","NA"))

names(ppr_df)[1] <- "Year"
names(ppr_df)[2] <- "Address"
names(ppr_df)[3] <- "Postal_Code"
names(ppr_df)[4] <- "County"
names(ppr_df)[5] <- "Price"
names(ppr_df)[6] <- "Not_Full_Market_Price"
names(ppr_df)[7] <- "Vat_Exclusive"
names(ppr_df)[8] <- "Is_Property_New"
names(ppr_df)[9] <- "Poperty_Size_Description"


ppr_df_new <- ppr_df[which(ppr_df$Poperty_Size_Description != "NA"), names(ppr_df) %in% c("Year","County","Price","Poperty_Size_Description")]


ppr_df_new_dublin <- ppr_df_new[which(ppr_df_new$County == "Dublin"), names(ppr_df_new) %in% c("Year","County","Price","Poperty_Size_Description")]


ppr_df_new_dublin$Poperty_Size_Description <- as.character(ppr_df_new_dublin$Poperty_Size_Description)


ppr_df_new_dublin$Poperty_Size_Description[ppr_df_new_dublin$Poperty_Size_Description == "greater than or equal to 38 sq metres and less than 125 sq metres"] <- "38"


ppr_df_new_dublin$Year <- format(as.Date(ppr_df_new_dublin$Year, format="%d/%m/%Y"),"%Y")


#install.packages("doBy")
#library(doBy)

ppr_df_new_dublin$Price <- gsub('[€]','',ppr_df_new_dublin$Price)


ppr_df_new_dublin$Price <- as.numeric(gsub(',','',ppr_df_new_dublin$Price))

ppr_df_new_dublin <- ppr_df_new_dublin[which(ppr_df_new_dublin$Poperty_Size_Description == "38"), names(ppr_df_new_dublin) %in% c("Year","County","Price","Poperty_Size_Description")]


summaryBy(Price ~ Year, data = ppr_df_new_dublin, FUN = list(mean,max,median,sd))

str(ppr_df_new_dublin)


count <- table(ppr_df_new_dublin$Year)

count

ppr_df_new_dublin <- ppr_df_new_dublin[-which(ppr_df_new_dublin$Year == "2019"), names(ppr_df_new_dublin) %in% c("Year","County","Price","Poperty_Size_Description")]





ppr_df_new <- ppr_df[-which(ppr_df$Poperty_Size_Description != "NA"), names(ppr_df) %in% c("Year","County","Price","Poperty_Size_Description")]



matches <- regmatches(ppr_df_new$Poperty_Size_Description, gregexpr("[[:digit:]]+", ppr_df_new$Poperty_Size_Description))

as.numeric(unlist(matches))

matches
drops <- c("Address", "Postal_Code","Not_Full_Market_Price", "Vat_Exclusive", "Poperty_Size_Description")

ppr_df <- ppr_df[ , !(names(ppr_df) %in% drops)]

# Checking Final column names
colnames(ppr_df)


# Removing month and dat from the date column and keeping only year

ppr_df$Year <- format(as.Date(ppr_df$Year, format="%d/%m/%Y"),"%Y")

# Removing Currency symbol from the Price Column

ppr_df$Price <- as.numeric(gsub('[€ ,]', '', ppr_df$Price))

#Checking the Structure of Dataframe
str(ppr_df)

# Taking Average of Property Prices and Grouping them by year

ppr_df <- aggregate(ppr_df[, 3], list(ppr_df$Year), mean)

head(ppr_df)

colnames(ppr_df)


# Assigning New Column names
names(ppr_df)[1] <- "Year"
names(ppr_df)[2] <- "Price"

# Merging Unemployenet dataframe to the ppr_df dataframe

ppr_df <-  merge(ppr_df, unemployment_df, by = "Year")

# Checking head

head(ppr_df)

# Adding a Categorical column where Unemployment rate greater than 7 will be treated as Yes 
# and less will be treated as No

ppr_df$Is_Unenployment_Greater_Than_7 <- ifelse(ppr_df$Unemployment_Rate >=7, "Yes", "No")

#Checking Structure

str(ppr_df)

# Testing differences in means
#One of the most common statistical tasks is to compare an outcome between two groups.
#The example here looks at comparing Property Prices When Unemployment rate is grater than 7 and when it is less than 7.

# Create boxplot showing how Property Prices varies between
# Umemployment status

qplot(x = as.factor(Is_Unenployment_Greater_Than_7), y = Price,
      geom = "boxplot", data = ppr_df,
      xlab = "Is Unemployment greater than 7", 
      ylab = "Price in Euros",
      fill = I("lightblue"))

install.packages("car")
library(car)


leveneTest(ppr_df$Price~ppr_df$Is_Unenployment_Greater_Than_7)
# The Plot clearly suggests that Unemployment rate plays a role in Property Prices
# How can we assess whether this difference is statistically significant?
# Let’s compute a summary table

ddply(ppr_df, ~ Is_Unenployment_Greater_Than_7, summarize,
      mean.price = mean(Price),
      sd.price = sd(Price)
)

#The standard deviation is good to have, but to assess statistical significance 
# we really want to have the standard error (which the standard deviation adjusted by the group size).

ddply(ppr_df, ~ Is_Unenployment_Greater_Than_7, summarize,
      group.size = length(Price),
      mean.price = mean(Price),
      sd.price = sd(Price),
      se.mean.price = sd.price / sqrt(group.size)
)

# This difference is looking quite significant. 
#To run a two-sample t-test, we can simple use the t.test() function.

ppr_df.t.test <- t.test(Price ~ Is_Unenployment_Greater_Than_7, data = ppr_df)

ppr_df.t.test


names(ppr_df.t.test)


ppr_df.t.test$p.value


ppr_df.t.test$estimate  # group means


ppr_df.t.test$conf.int  # confidence interval for difference


attr(ppr_df.t.test$conf.int, "conf.level")  # confidence level


#The ability to pull specific information from the output of the hypothesis test allows
# you to report your results using inline code chunks. That is, 
# you don’t have to hardcode estimates, p-values, confidence intervals, etc.

# Calculate difference in means between smoking and nonsmoking groups
ppr_df.t.test$estimate



pprdf.unemployment.diff <- round(ppr_df.t.test$estimate[1] - ppr_df.t.test$estimate[2], 1)


pprdf.unemployment.diff
# Confidence level as a %
conf.level <- attr(ppr_df.t.test$conf.int, "conf.level") * 100


conf.level

# Our study finds that when Unemployment rate is less than 7 
# Prpperty Prices are on average 61598.9 higher. 


# What is statistical significance testing doing?






