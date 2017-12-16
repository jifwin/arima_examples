install.packages("forecast")
library(forecast)

ts_length = 10000

ar1 = 0.8897
ar2 = -0.4858
ma1 = -0.2279
ma2 = 0.2488
sd = sqrt(0.2)
ts_data = arima.sim(n = ts_length, list(ar = c(ar1, ar2), ma = c(ma1, ma2)), sd = sd)
plot(ts_data)

subsets = seq(from=100, by=100, to=ts_length)
models = lapply(subsets, function(i) { 
  print(i)
  return(arima(ts_data[1:i], order=c(2,0,2)))
  #return(auto.arima(ts_data[1:i]))
  
  })
ar1s = sapply(models, function(model) model$coef[1])
ar2s = sapply(models, function(model) model$coef[2])
ma1s = sapply(models, function(model) model$coef[3])
ma2s = sapply(models, function(model) model$coef[4])
sigma2s = sapply(models, function(model) model$sigma2)

plot_coef = function(values, value, title) {
  values = na.omit(unname(values))
  y_min = min(values, value, -1)
  y_max = max(values, value, 1)
  plot(subsets, values, main=title, ylim=c(y_min,y_max))
  abline(h=value)
}

plot_coef(ar1s, ar1, "ar1")
plot_coef(ar2s, ar2, "ar2")
plot_coef(ma1s, ma1, "ma1")
plot_coef(ma2s, ma2, "ma2")
plot_coef(sigma2s, sd^2, "sigma2")

#todo: porownac auto arime do wymuszonych parametrow
#todo: wykres w zaleznosci od szumu!! todo, dziwne jakby nie zalezalo?
#todo: pokazac ze im prostszy model tym prosciej dopasowac
#todo: pokazac ze im mnienjszy szum tym mniej probek potrzeba
#todo: pokazac ze potrzeba naprawde duzo probek zeby auto.arima zadziaalal
#todo: pokazac jak sie sprawdzaja prognozy krotko i dlugoterminowe - mimo ze model nie jest idealnie dopasowany
#todo: wizualizacja
#todo: moze to powinnno byc w kolejynm kroku? conajmniej 2
#todo: zrobic to w petli
#todo: pokazac ze ze wzwiekszaninem ilosci probek uzyskany model jest blizszy reaalu
#todo: pokazac ile potrzebujemy probek gdy wybierzemy model na sile!!!