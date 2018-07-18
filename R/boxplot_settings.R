# functions
# functions to return text in a boxplot
max.n <- function(x){
  if (max(x)>0 & max(x)< 10){
    return(c(y = max(x)*1.32, label = round(max(x),1))) 
  } else if (max(x)< 0){
    return(c(y = max(x)*0.8, label = round(max(x),1)))
  } else{
    return(c(y = max(x)*1.12, label = round(max(x),1))) 
  } 
  # experiment with the multiplier to find the perfect position
}
min.n <- function(x){
  if (min(x) < 0) {
    return(c(y = min(x)*1.16, label = round(min(x),1)))
  } else {
    return(c(y = min(x)*0.32, label = round(min(x),1))) 
  }
  # experiment with the multiplier to find the perfect position
}
# function for mean labels
mean.n <- function(x){
  if (mean(x) > -20 & mean(x) < 0) {
    return(c(y = mean(x)*0.88, label = round(mean(x),1)))
  } else if (mean(x) < -20){
    return(c(y = mean(x)*1.08, label = round(mean(x),1)))
  } else{
    return(c(y = mean(x)*1.02, label = round(mean(x),1))) 
  } 
  # experiment with the multiplier to find the perfect position
}
# function for layout
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)