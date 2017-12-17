plot_coef = function(values, value, title) {
  values = na.omit(unname(values))
  y_min = min(values, value, -1)
  y_max = max(values, value, 1)
  plot(subsets, values, main=title, ylim=c(y_min,y_max))
  abline(h=value)
}

load_ts = function(file, sep=";") {
  data = read.csv(file, sep=sep)
  return(ts(data$value))
}
