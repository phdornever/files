
library(mvtnorm)
library(ggplot2)
library(dplyr)

set.seed(10000)
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


Sigma <- matrix(c(1,0.8,0.8,1),nrow = 2)
dat <-rmvnorm(1000,mean=rep(0,2),Sigma)
unif <- dat
unif[,1] <- pnorm(dat[,1], mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
unif[,2] <- pnorm(dat[,2], mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
cauchy <- tan(pi*(unif-0.5))
colnames(cauchy) <- c('x','y')
cauchy <- as.data.frame(cauchy)
x <- as.data.frame(cauchy$x)
y <- as.data.frame(cauchy$y)
x <- cauchy$x %>% remove_outliers() %>% na.omit() %>% as.data.frame()
y <- cauchy$y %>% remove_outliers() %>% na.omit() %>% as.data.frame() 


p <- ggplot() + geom_histogram(data = x,aes(.),binwidth = .1,alpha = 0.5,color = "blue",fill = "blue") + 
  geom_histogram(data = y,aes(.),color="black",fill="white",alpha = 0.5,linetype = "dashed")+ 
  scale_colour_manual(".", values = c("red", "blue"))
p


############################################################################################

####### Problem 5 #######
mu <- 200
phi <- 1/2
x <- rnorm(100,mu,sqrt(1/phi))


samplerGibbs <- function(start_a,start_b,n_sim,data){
  n <- length(data)
  x_bar <- mean(data)
  res <- matrix(0,nrow = n_sim,ncol = 2)
  res[1,] <- c(start_a,start_b)
  for (i in 2:n_sim) {
    res[i,1] <- rnorm(1,x_bar,1/sqrt(n*res[i-1,2]))
    res[i,2] <- rgamma(1,shape = n/2,rate = 1/(sum((x-res[i,1]))^2))
  }
  return(res)
}

res <- samplerGibbs(0,5,100,x)
plot(res[,1])
