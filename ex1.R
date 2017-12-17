setwd("/Users/gpietrus/git/arima_examples")
source("common.R")
library(forecast)
library(xts)

#parametry globalne
forecast_levels = c(80,90,95)
ts_length = 10000

#parametry modelu ARIMA(2,0,2)
ar1 = 0.8897
ar2 = -0.4858
ma1 = -0.2279
ma2 = 0.2488
sd = sqrt(0.1)

#generowanie szeregu
ts_data = arima.sim(n = ts_length, list(ar = c(ar1, ar2), ma = c(ma1, ma2)), sd = sd)
plot(ts_data)

#pozbiory szeregu
subsets = seq(from=10, by=100, to=ts_length)

#modele dla pozbiorow
models = lapply(subsets, function(i) { 
  print(i)
  return(arima(ts_data[1:i], order=c(2,0,2)))
  })

#parametry uzyskanych modeli
ar1s = sapply(models, function(model) model$coef[1])
ar2s = sapply(models, function(model) model$coef[2])
ma1s = sapply(models, function(model) model$coef[3])
ma2s = sapply(models, function(model) model$coef[4])
sigma2s = sapply(models, function(model) model$sigma2)

#zbieznosc uzyskanych parametrow wzgledem dlugosci szeregu
par(mfrow=c(2,2))
plot_coef(ar1s, ar1, "ar1")
plot_coef(ar2s, ar2, "ar2")
plot_coef(ma1s, ma1, "ma1")
plot_coef(ma2s, ma2, "ma2")
par(mfrow=c(1,1))

#ciagle prognozowanie krokoterminowe
split_points = seq(from=9750, by=1, to=9800)

for (split_point in split_points) {
  ts_train = head(ts_data, n=split_point)
  ts_test = tail(ts_data, n=length(ts_data)-split_point)
  
  forecast_length = 3
  model = arima(ts_train, order=c(2,0,2))
  forecasted = forecast(model, forecast_length, level=forecast_levels)
  plot(forecasted, include=50)
  ts_test_forecasted = head(ts_test, forecast_length) 
  points(ts_test_forecasted)
}

#przy prognozowaniu dlugoterminowych prognoza "wyplaszcza sie"
#i sprowadza sie do modelu ARIMA(0,0,0) z pewnym sd
split_point = 9750
ts_train = head(ts_data, n=split_point)
ts_test = tail(ts_data, n=length(ts_data)-split_point)
model = arima(ts_train, order=c(2,0,2))
plot(forecast(model, h=length(ts_test), level=forecast_levels), include=2000)
lines(ts_test)

#todo: sarima
ts_data = load_ts("data/2447_1.csv",sep=",")
plot(ts_data) 

split_points = seq(from=280, by=1, to=330)

for (split_point in split_points) {
  ts_train = head(ts_data, n=split_point)
  ts_test = tail(ts_data, n=length(ts_data)-split_point)
  
  forecast_length = 3
  model = auto.arima(ts_train, seasonal=TRUE)
  forecasted = forecast(model, forecast_length, level=forecast_levels)
  plot(forecasted, include=50)
  ts_test_forecasted = head(ts_test, forecast_length) 
  points(ts_test_forecasted)
}

model = auto.arima(ts_data, seasonal=TRUE)
forecasted = forecast(model, h=12*12, level=forecast_levels)
plot(forecasted)

#todo: podobnie jak w poprzednim
#todo: pokazac co przy dlugoterminowej prognozie sie stanie

#todo: napisac tu jakis komentarz
ts_data = AirPassengers
model = auto.arima(ts_data)
model_no_diff = auto.arima(ts_data, d=0, D=0)
plot(forecast(model))
plot(forecast(model_no_diff))
#todo: pokzac roznice pomiedzy modelem z roznicowaniem a bez roznicowania

#todo: problemy z sarima

#todo: 5 minut data
#todo: fourier
ts_data = load_ts("data/seasonal_long.csv")
period = 288
harmonics = 1
forecast_length = 250

ts_data = ts(coredata(ts_data), frequency=period)

model = tslm(ts_data ~ fourier(ts_data, K = harmonics))
forecasted = forecast(model, data.frame(fourier(ts_data, K=harmonics,h=forecast_length)))
plot(forecasted, include=1000)

#todo: show the problem of only fourier - average
#todo: narysowac golego fouriera na tym sszeregu albo pod nim

x = index(ts_data)
coefs = model$coefficients
fourier_ts_data = ts(coefs[1]+
                       coefs[1]*sin(2*pi*x)/(2*pi)+
                       coefs[2]*cos(2*pi*x)/(2*pi), 
                     frequency=period)
plot(ts_data)
lines(fourier_ts_data, col="red")

#todo: fourier + arima
ts_data = load_ts("data/seasonal_long.csv")
period = 288
harmonics = 1
forecast_length = 3

ts_data = ts(coredata(ts_data), frequency=period)

#porownanie prognoz z modelu fouriera i fouriera+arima
split_points = seq(from=2250, to=2490, length.out=5)

par(mfrow=c(2,1))
for (split_point in split_points) {
  print(split_point)
  ts_data_train = ts(head(ts_data, split_point), frequency=288)
  fourier_model = Arima(ts_data_train, order=c(0,0,0), xreg=fourier(ts_data_train, K = harmonics))
  fourier_arima_model = Arima(ts_data_train, order=c(1,1,0), xreg=fourier(ts_data_train, K = harmonics))
  plot(forecast(fourier_model, xreg=(fourier(ts_data_train, K=harmonics,h=forecast_length))), include=100, main="Fourier model")
  plot(forecast(fourier_arima_model, xreg=(fourier(ts_data_train, K=harmonics,h=forecast_length))), include=100, main="Fourier + ARIMA model")
}
par(mfrow=c(1,1))


#todo: multi season

  
#todo: tbats

#1.csv - bardzo gesty time series
