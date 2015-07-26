power <- function(x, a){
 out =  x^a
 print(out)
}

power(2,3)
power(3,8)

power(10,3)
power(8,17)
power(131,3)

#------------------------------------------

power3 <- function(x, a){
  result =  x^a
  return(result)
}


result = power3(2,3)
 
y = power3(1:10,2)
x = rep(1:10)
out = data.frame(x, y[1])
names(out) <- c("x","y")
plot(out$y, out$x)

#------------------------------------------------

plt_pwr <- function(x,a){

  y = power3(x,a)
  x = rep(x)
  out = data.frame(x, y[1])
  names(out) <- c("x","y")
  plot(out$y, out$x)
  
}

plt_pwr(1:10,3)



