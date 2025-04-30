library(fpp2)
library(tidyr)
library(dplyr)
library(urca)
library(tseries)

data <- read.csv("inflation.csv")
data
View(data)

# Data Cleaning -----------------------------------------------------------

colnames(data) <- gsub("^X", "", colnames(data))
colnames(data)
data

year<-c(1980:2022)      #making a vector consisting of all years
year<-as.character(year)

inf<-data |>  gather(year,key = "Year",value="InflationRate")
inf1<-na.omit(inf) #omitting NA values

inf_clean <- inf1 |> 
  filter(InflationRate != "no data")
View(inf_clean)

names(inf_clean)<-c("region","year","inflation")
inf_clean$year<-as.integer(inf_clean$year)

# Filtering the INDIA data  ---------------------------

India<-filter(inf_clean,region=="India")
India$inflation<-as.numeric(India$inflation)
India$year<-as.numeric(India$year)

India_ts <- ts(India$inflation, start = min(India$year), end = max(India$year), frequency = 1)
(India_ts)
autoplot(India_ts)

# Checking with General forecasting Techniques ----------------------------
naive_model <- naive(India_ts, h = 3)
autoplot(naive_model)

drift_model <- rwf(India_ts, drift = TRUE, h = 3)
autoplot(drift_model)

accuracy(naive_model)
accuracy(snaive_model)
accuracy(drift_model) #Best method on basis of accuracy result

accuracy_drift <- accuracy(drift_model) %>% as_tibble() %>% mutate(Method = "Drift")
accuracy_arima <- accuracy(fit2) %>% as_tibble() %>% mutate(Method = "ARIMA") # Best forecasting

# Combine and compare
accuracy_comparison <- bind_rows(accuracy_drift, accuracy_arima) |> select(Method, RMSE, MAE, MAPE,MASE,ACF1)
  
print(accuracy_comparison)


# Checking Stationarity ---------------------------------------------------

India_ts|> ur.kpss() |> summary()

adf.test(India_ts)

nsdiffs(India_ts)
ndiffs(India_ts )

ggAcf(India_ts)#q=2,1
ggPacf(India_ts)#p=1

y<-diff(India_ts)
ndiffs(y)

# Fitting Arima for India data --------------------------------------------

auto.arima(India_ts, trace = TRUE, approximation = FALSE, seasonal = FALSE, stepwise = FALSE)
fit1 <- Arima(India_ts, order = c(0,1,1))
fit1

fit2 <- Arima(India_ts, order = c(2,1,2))
fit2

# Checking Residuals and Forecasting for India data -----------------------
checkresiduals(fit2)
res <- residuals(fit2)
mean(res)

ggAcf(checkresiduals(fit2))
ggPacf(checkresiduals(fit2))

forecast_fit2<-(forecast(fit2, h=3))
forecast_fit2
autoplot(forecast(fit2, h=3)) +
  ggtitle("Forecast of India from ARIMA(2,1,2)") +
  xlab("Year") + ylab("Forecasted Value") +
  theme(plot.title = element_text(hjust = 0.5))

# Checking for Heteroskedasticity using ARCH-LM Test ----------------------
library(FinTS)
ArchTest(residuals(fit2))
#checking Accuracy
accuracy(fit2)



# Filtering China Data and Performing Same steps followed above -------------------------------------------
china<-filter(inf_clean,region=="China, People's Republic of")
china$inflation<-as.numeric(china$inflation)
china$year<-as.numeric(china$year)

china_ts <- ts(china$inflation, start = min(china$year), end = max(china$year), frequency = 1)
(china_ts) 
autoplot(china_ts)

# Checking Stationarity for China Data
china_ts|> ur.kpss() |> summary()

adf.test(china_ts)
nsdiffs(china_ts)
ndiffs(china_ts)

y_china<-diff(china_ts)
ndiffs(y_china)

ggAcf(china_ts)# q=1
ggPacf(china_ts)# p=3

#Fitting into ARIMA Model for China
auto.arima(china_ts, trace = TRUE, approximation = FALSE, seasonal = FALSE, stepwise = FALSE)

fit3 <- Arima(china_ts, order = c(1,1,2))
fit3

#Checking Residuals for China 
checkresiduals(fit3)
res <- residuals(fit3)
mean(res)

forecast_fit3 <- forecast(fit3, h = 3)
forecast_fit3
autoplot(forecast(fit3, h=3)) +
  ggtitle("Forecast of China from ARIMA(1,1,2)") +
  xlab("Year") + ylab("Forecasted Value") +
  theme(plot.title = element_text(hjust = 0.5))

#Checking Heteroskedasticity using ARCH-LM Test
ArchTest(residuals(fit3))

#Checking Accuracy 
accuracy(fit3)



# Filtering data for US and Performing the Same steps followed above --------

US<-filter(inf_clean,region=="United States")
US$inflation<-as.numeric(US$inflation)
US$year<-as.numeric(US$year)

US_ts <- ts(US$inflation, start = min(US$year), end = max(US$year), frequency = 1)
(US_ts) 
autoplot(US_ts)

#Checking Stationarity for US data 
US_ts|> ur.kpss() |> summary()

adf.test(US_ts)

nsdiffs(US_ts)
ndiffs(US_ts)

y_US<-diff(US_ts)
ndiffs(y_US)

ggAcf(US_ts)# q=1
ggPacf(US_ts)#p=1

#Fitting the ARIMA model for US Data
auto.arima(US_ts, trace = TRUE, approximation = FALSE, seasonal = FALSE, stepwise = FALSE)
fit4 <- Arima(US_ts, order = c(0,1,0))
fit4
fit5 <- Arima(US_ts, order = c(1,1,1))
fit5

#checking the residuals for US data
checkresiduals(fit4)
res <- residuals(fit4)
mean(res)
ggAcf(checkresiduals(fit4))
ggPacf(checkresiduals(fit4))

forecast_fit4 <- forecast(fit4, h = 3)
forecast_fit4
autoplot(forecast(fit4, h=3)) +
  ggtitle("Forecast of US from ARIMA(0,1,0)") +
  xlab("Year") + ylab("Forecasted Value") +
  theme(plot.title = element_text(hjust = 0.5))

#Checking the Heteroskedasticity using ARCH-LM Test for US Data
ArchTest(residuals(fit4))
#Checking Accuracy
accuracy(fit4)

#Comparison Plot
autoplot(India_ts, series = "India") +
  autolayer(US_ts, series = "US") +
  autolayer(china_ts, series = "China") +
  autolayer(forecast_fit2, series = "India Forecast", PI = FALSE) +
  autolayer(forecast_fit3, series = "US Forecast", PI = FALSE) +
  autolayer(forecast_fit4, series = "China Forecast", PI = FALSE) +
  ggtitle("Forecast Comparison: India, US, China") +
  xlab("Year") + ylab("Forecasted Value") +
  guides(colour = guide_legend(title = "Model")) +
  theme_minimal()
