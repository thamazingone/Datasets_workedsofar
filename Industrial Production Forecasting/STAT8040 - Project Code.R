library(fpp3)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(seasonal)
library(forecast)
library(stats)
library(feasts)
library(fable)
library(lubridate)

setwd('D:/STAT8040')
#Passing the loaded dataset into a variable
#Read data through readxl UI option

#Understanding the data:
df <- Bitcoin_prices
#Checking top rows
head(df)

#Checking the number of rows and columns
dim(df)

#Summarizing the data
summary(df)

#Our Target variable here is Close.
colnames(df)

#Creating another variable with Category as 'Bitcoin'
#We need this for creating tibble and for that we need a key variable
df['Category']='Bitcoin'


#Creating tibble
new_prices <- df %>%
  as_tsibble(key='Adj Close',index=Date)

# Basic Visualizations

# Create a time plot of the Opening price of bitcoin
ggplot(df, aes(x = Date, y = Open)) + geom_line(col='blue') +
  labs(title = "Bitcoin Opening Price Over Time", x = "Date", y = "Open Price")

# Create a time plot of the Closing price of bitcoin
ggplot(df, aes(x = Date, y = Close)) + geom_line(col='red') +
  labs(title = "Bitcoin Closing Price Over Time", x = "Date", y = "Close Price")

# Create a time plot of the Volume of bitcoin
ggplot(df, aes(x = Date, y = Volume)) + geom_line(col='orange') +
  labs(title = "Bitcoin Volumes traded over Time", x = "Date", y = "Close Price")

# Create an ACF plot
acf(df$Close, main = "ACF Plot for Bitcoin Closing Price")

#Decomposition
#Exponential Decomposition
dcmpp <- ets(df$Close)

# Plot the decomposition
autoplot(dcmpp)

# Split the data into training and testing sets
train_data <- head(new_prices, n = 80)
test_data <- tail(new_prices, n = 20)

# Fit an ARIMA model
arima_model <- auto.arima(train_data$Close)
arima_forecast <- forecast(arima_model, h = 20)

# Fit an Exponential Smoothing model
ets_model <- ets(train_data$Close)
ets_forecast <- forecast(ets_model, h = 20)

# Compare the forecasts
accuracy(arima_forecast, test_data$Close)
accuracy(ets_forecast, test_data$Close)

# Plot the forecasts
plot(arima_forecast, main = "ARIMA Forecast")

new_prices <- new_prices %>% 
  model(
    Mean=MEAN(Close),
    Naive=NAIVE(Close))

#Forecasting
prices_forecasted <- new_prices %>% forecast(h = 365)

accuracy(prices_forecasted$Close, test_data$Mean) %>% select(.model, RMSE, MAE)

# Plot the forecasts
plot(prices_forecasted, main = "Exponential Smoothing Forecast")
