##########################################################
# Create prices set, validation set (final hold-out test set)
##########################################################


library(dplyr)
options(dplyr.summarise.inform=F)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library (readr)

#download house price data and add column names
housePrices <- read_csv(url("https://github.com/egglesworth/NYC-Prices/raw/main/nyc-rolling-sales.csv"))
colnames(housePrices) <- c("num", "Borough", "Neighbourhood", "Building_class_cat", "Tax_now", "Block", "Lot", "Easement", "Building_class_now", "Address", "Appt_num", "Zip_code", 
                           "Residential_units", "Commercial_units", "Total_units", "Land_sf", "Gross_sf", "Year", "Tax_class", "Building_class_sale", "Sale_price", "Sale_date")

#remove houses with a negligible or zero sale price or gross square feet
housePrices <- as.data.frame(housePrices) %>% 
  mutate(Sale_price = as.numeric(Sale_price), Decade=10* floor(Year/10), GSF= round(as.numeric(Gross_sf), -2), LSF =round(as.numeric(Land_sf), -3), Block =round(as.numeric(Block), -2)) %>%
  filter(Sale_price >500, Decade>0, GSF>0, Sale_price<10^7) 

# Validation set will be 10% of housePrices data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = housePrices$Sale_price, times = 1, p = 0.1, list = FALSE)
prices <- housePrices[-test_index,]
temp <- housePrices[test_index,]

# Make sure borough and GSF in validation set are also in prices set
validation <- temp %>% 
  semi_join(prices, by = "Building_class_sale") %>%
  semi_join(prices, by = "GSF") %>%
  semi_join(prices, by = "Total_units") 

# Add rows removed from validation set back into prices set
removed <- anti_join(temp, validation)
prices <- rbind(prices, removed)

rm(housePrices, test_index, temp, removed)

##########################################################
#Analyse prices datasets
##########################################################

#Number of entries in the data set
nrow(prices)

#plot number of sales by total units
prices %>%
  group_by(Total_units) %>%
  summarise(number= n()) %>%
  ggplot(aes(x=Total_units, y=number)) +
  geom_bar(stat="identity")+ 
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), limits=c(0.5,10.5))+
  labs(x="Total units", y= "Number of properties",
       subtitle="Distribution of properties by total units")

#plot number of sales by  gross square feet
prices %>%
  group_by(GSF) %>%
  summarise(number= n()) %>%
  ggplot(aes(x=GSF, y=number)) +
  geom_bar(stat="identity")+ 
  xlim(0,10000)+
  labs(x="Square feet of property", y= "Number of properties", 
       subtitle= "Distribution of properties by gross square feet")
  
#plot number of sales by square feet of land
prices %>%
  group_by(LSF) %>%
  summarise(number= n()) %>%
  ggplot(aes(x=LSF, y=number)) +
  geom_bar(stat="identity") + 
  xlim(0,10000)+
  labs(x="Square feet of land", y= "Number of properties", 
       subtitle= "Distribution of properties by square feet of land")

#plot number of sales by decade
prices %>%
  group_by(Decade) %>%
  summarise(number= n()) %>%
  ggplot(aes(x=Decade, y=number)) +
  geom_bar(stat="identity") +
  scale_x_continuous(limits=c(1880,2020)) + 
  labs(x="Decade", y= "Number of properties", 
       subtitle="Distribution of properties by decade built")

#plot a histogram of sale prices
prices %>%
  ggplot(aes(Sale_price)) +
  geom_histogram(binwidth=5000)+
  xlim(0,2*10^6)+
  labs(x="Sale price", y="Number of properties", 
       subtitle= "Distribution of properties by sale price")

prices %>%
  ggplot(aes(x=GSF, y=Sale_price)) +
  geom_point()+
  xlim(0,2*10^4)+
  ylim(0,2*10^6)
##########################################################
#Build a model to predict prices 
##########################################################
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_i <- createDataPartition(y = prices$Sale_price, times = 1, p = 0.3, list = FALSE)
train_set <- prices[-test_i,]
test_set_temp <- prices[test_i,]
test_set <- test_set_temp %>% 
  semi_join(train_set, by = "Total_units") %>%
  semi_join(train_set, by = "GSF") %>%
  semi_join(train_set, by = "Building_class_sale") 


# Add rows removed from test set back into train set
removed <- anti_join(test_set_temp, test_set)
train_set <- rbind(train_set, removed)
  
#predict purely based on mean of total data set
mu <- mean(train_set$Sale_price) 
mu

predicted_price <- test_set %>% 
  mutate(pred = mu ) %>%
  pull(pred) #extract the predictions as a list

#test how good this prediction is
rmse_mu <- RMSE(predicted_price, test_set$Sale_price) 
rmse_mu


#add a term b_u which adjusts predicted price based on total units
unit_avgs <- train_set %>% 
  group_by(Total_units) %>% #group by borough
  summarize(b_u = mean(Sale_price - mu)) 

#produce predictions based on adjusting depending on total units
predicted_price <- test_set %>% 
  left_join(unit_avgs, by='Total_units') %>% 
  mutate(pred = mu + b_u) %>%
  pull(pred) 

#test how good this prediction is
rmse_units <- RMSE(predicted_price, test_set$Sale_price) 
rmse_units

#Include a term b_g which adjusts price based on gross square feet
sqft_avgs <- train_set %>% 
  left_join(unit_avgs, by='Total_units') %>%
  group_by(GSF) %>%       #group by user
  summarize(b_g = mean(Sale_price - mu - b_u)) 

#Predict price based on borough and square footage of property
predicted_price <- test_set %>% 
  left_join(unit_avgs, by='Total_units') %>%
  left_join(sqft_avgs, by='GSF') %>%
  mutate(pred = mu + b_u + b_g) %>%
  pull(pred)

NAs_pred <- which(is.na(predicted_price))
predicted_price[NAs_pred] <- mu
#test how good this prediction is
rmse_sqft <- RMSE(predicted_price, test_set$Sale_price)
rmse_sqft

#Include a term based on building class
bclass_avgs <- train_set %>% 
  left_join(unit_avgs, by='Total_units') %>%
  left_join(sqft_avgs, by='GSF') %>%
  group_by(Building_class_sale) %>%
  summarize(b_c = mean(Sale_price - mu - b_u - b_g )) 

#Predict price based on borough, square footage, square feet of land,
#building class 
predicted_price <- test_set %>%  
  left_join(unit_avgs, by='Total_units') %>%
  left_join(sqft_avgs, by='GSF') %>%
  left_join(bclass_avgs, by='Building_class_sale') %>%
  mutate(pred=ifelse(is.na(b_c), mu + b_u + b_g, 
                     mu + b_u + b_g + b_c)) %>%
  pull(pred)

#test how good this prediction is
rmse_class <- RMSE(predicted_price, test_set$Sale_price) 
rmse_class

dataf=data.frame(predicted_price)
ggplot(dataf, aes(x=predicted_price)) +
  geom_histogram(binwidth=200) +
  labs(x="Predicted sale price", y="Number of properties")

#######################################################################
#Regularisation
######################################################################

#Define a function to predict the value of houses 
#given a certain value of lambda
predict_prices_lambda <- function(l, train, test){
  #predict purely based on mean of total data set
  mu <- mean(train$Sale_price) 
  
  #Include a term b_u which adjusts price based on total units
  unit_avgs <- train %>% 
    group_by(Total_units) %>% #group by total units
    summarize(b_u = mean(Sale_price - mu)) 
  
  #Include a term b_g which adjusts price based on gross square feet
  sqft_avgs <- train %>% 
    left_join(unit_avgs, by='Total_units') %>%
    group_by(GSF) %>%       
    summarize(b_g = sum(Sale_price - mu - b_u)/(n()+l)) 
  
  #Include a term based on building class
  bclass_avgs <- train %>% 
    left_join(unit_avgs, by='Total_units') %>%
    left_join(sqft_avgs, by='GSF') %>%
    group_by(Building_class_sale) %>%
    summarize(b_c = sum(Sale_price - mu - b_u - b_g )/(n()+l)) 
  
  #Predict prices based on the above adjustments
  predicted_price <- test %>%  
    left_join(unit_avgs, by='Total_units') %>%
    left_join(sqft_avgs, by='GSF') %>%
    left_join(bclass_avgs, by='Building_class_sale') %>%
    mutate(pred=ifelse(is.na(mu + b_u + b_g + b_c), mu, mu + b_u + b_g  + b_c)) %>%
    pull(pred)
  
  predicted_price
}

rmse_lambda <-  function(l, train, test){
  predicted_price <- predict_prices_lambda(l, train, test)
  rmse_class <- RMSE(predicted_price, test$Sale_price) 
  rmse_class
}


#split training data into two sets
test_i <- createDataPartition(y = train_set$Sale_price, times = 1, p = 0.3, list = FALSE)
train_set_1 <- train_set[-test_i,]
train_set_2 <- train_set[test_i,]
train_set_2 <- train_set_2 %>% 
  semi_join(prices, by = "Total_units") %>%
  semi_join(prices, by = "GSF")

lambdas <- seq(0, 100, 2)
lambdas
rmses <- sapply(lambdas, rmse_lambda, train= train_set_1, test=train_set_2)
lambda <- lambdas[which.min(rmses)]
lambda
reg_rmse <- rmse_lambda(lambda, train_set, test_set)

names_rmse <- c("Mean", "Total units", "Square feet", "Building Class", 
                "Regularisation")
results_rmse <- c(rmse_mu/10^3, rmse_units/10^3, rmse_sqft/10^3, rmse_class/10^3, reg_rmse/10^3)
df <- data.frame(names_rmse, results_rmse)
ggplot(data=df, aes(x= reorder(names_rmse,  results_rmse), y=results_rmse)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="Method", y="RMSE (thousands of dollars)", subtitle="RMSE against additional method used")



##########################################################
#Validation
##########################################################

#Find RMSE of model on validation data set
rmse_lambda(lambda, prices, validation)
