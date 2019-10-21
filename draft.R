library(ggplot2)
library(tidyverse)
library(dplyr)

airfreight <- read.table("airfreight.txt")
airfreight <- airfreight[-1,]
airfreight <- airfreight %>% as.data.frame() %>%
  rename(transfer = V1, arrival = V2) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character, as.numeric)

fit <- lm(arrival~transfer, data = airfreight)
summary(fit) # get coefficient


ggplot(data = airfreight, aes(x = transfer, y = arrival)) + geom_point() + geom_smooth(method = "lm") + 
  labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                     "Intercept =",signif(fit$coef[[1]],5 ),
                     " Slope =",signif(fit$coef[[2]], 5),
                     " P =",signif(summary(fit)$coef[2,4], 5)))


fit$coef[[1]] + 1 * fit$coef[[2]]
fit$coef[[1]] + 2 * fit$coef[[2]]

fit$coef[[1]] + mean(airfreight$transfer) * fit$coef[[2]]

mean(airfreight$arrival)
#true


#Problem2
X <- cbind(rep(1,10),airfreight$transfer)
Y <- airfreight$arrival
h <- X %*% solve(t(X) %*% X) %*% t(X)
Sxx <- sum((X[,2] - mean(X[,2])) ^ 2)
Sxy <- sum((X[,2] - mean(X[,2])) * (Y - mean(Y)))
Syy <- sum((Y - mean(Y)) * (Y - mean(Y)))
beta1_hat <- Sxy/Sxx
sigma_hat <- t(Y) %*% (diag(1,10,10) - h) %*% Y/8
t <- beta1_hat/sqrt(sigma_hat/Sxx)
t2 <- beta0_hat/sqrt(sigma_hat * (1 / 10 + mean(X[,2]) ^ 2 / Sxx))
t
sqrt(sigma_hat/Sxx)
2*pt(-abs(t),df=8) # p-value
qt(.025, 8, lower.tail = TRUE, log.p = FALSE)
qt(.025, 8, lower.tail = FALSE, log.p = FALSE)

# (c) TEST POSITIVE 
2*pt(-abs(t),df=8) # p-value
qt(.05, 8, lower.tail = FALSE, log.p = FALSE)

#(d)
beta0_hat <- mean(Y) - beta1_hat * mean(X[,2])

t2
#e
t3 <- (beta0_hat - 9)/sqrt(sigma_hat * (1 / 10 + mean(X[,2]) ^ 2 / Sxx))
t3
pt(-abs(t3),df=8) # p-value
qt(.025, 8, lower.tail = FALSE, log.p = FALSE)

#Problem3
#(a)
predict(fit,data.frame(transfer = 2.4), interval = "confidence", level = .99)

#(b)
predict(fit,data.frame(transfer = 2), interval = "confidence", level = .99)

#(c)
predict(fit,data.frame(transfer = c(2,2,2)), interval = "confidence", level = .99)
pairwise.t.test()

#problem 4
#(a)
anova(fit)
aov(arrival~transfer, data = airfreight)
sum((predict(fit) - mean(Y))^2) #regression sum of squares RSS/SSR
sum((predict(fit) - Y)^2) # residual sum of squares SSE
summary(anova(fit))
summary(aov(arrival~transfer, data = airfreight))

anova(lm(arrival~transfer, data = airfreight))
anova(lm(I(arrival-0.*transfer)~1, data = airfreight))



library(dplyr)
library(ggplot2)
library(lattice)
library(GGally)
library(tree)
tskull <- read.csv("Tskull_19.csv")
tskull$Type <- as.factor(tskull$Type)
train <- tskull %>% filter(Holdout==0)
valid <- tskull %>% filter(Holdout==1)
pairs(tskull[,1:5], col=as.factor(tskull$Type))

ggpairs(tskull,columns = 1:5,ggplot2::aes(colour=as.factor(Type)))
ggplot(tskull,aes(colour=as.factor(type)))

#proportion difference
t <- tskull %>% filter(Type==1) %>% select(Length, Breadth, Height, Fheight, Fbreadth) %>% 
  colMeans() %>% as.data.frame()
t2 <- tskull %>% filter(Type==2) %>% select(Length, Breadth, Height, Fheight, Fbreadth) %>%
  colMeans() %>% as.data.frame()

t2-t # difference of mean

rt <- tree(Type~., data = tskull)
summary(rt)
plot(rt)
text(rt, pretty = 0)

rt_train <- tree(as.factor(Type)~., data = train, control = tree.control(nobs = 24,mincut = 1, minsize = 2, mindev = 0.01 ))
plot(rt_train)
text(rt_train,pretty = 0)
sum(predict(rt_train, train, type = "class") == train$Type)/24
sum(predict(rt_train, valid, type = "class") == valid$Type)/8
summary(rt_train)

rt_train <- tree(as.factor(Type)~., data = train, control = tree.control(nobs = 24,mincut = 2, minsize = 4, mindev = 0.01 ))
plot(rt_train)
text(rt_train,pretty = 0)
sum(predict(rt_train, train, type = "class") == train$Type)/24
sum(predict(rt_train, valid, type = "class") == valid$Type)/8

rt_train <- tree(as.factor(Type)~., data = train, control = tree.control(nobs = 24,mincut = 2, minsize = 8, mindev = 0.01 ))
plot(rt_train)
text(rt_train,pretty = 0)
sum(predict(rt_train, train, type = "class") == train$Type)/24
sum(predict(rt_train, valid, type = "class") == valid$Type)/8


rt_train <- tree(as.factor(Type)~., data = train, control = tree.control(nobs = 24,mincut = 2, minsize = 10, mindev = 0.01 ))
plot(rt_train)
text(rt_train,pretty = 0)
sum(predict(rt_train, train, type = "class") == train$Type)/24
sum(predict(rt_train, valid, type = "class") == valid$Type)/8

rt_train <- tree(as.factor(Type)~., data = train, control = tree.control(nobs = 24,mincut = 2, minsize =12, mindev = 0.01 ))
plot(rt_train)
text(rt_train,pretty = 0)
sum(predict(rt_train, train, type = "class") == train$Type)/24
sum(predict(rt_train, valid, type = "class") == valid$Type)/8

rt_train <- tree(as.factor(Type)~., data = train, control = tree.control(nobs = 24,mincut = 2, minsize =14, mindev = 0.01 ))
plot(rt_train)
text(rt_train,pretty = 0)
sum(predict(rt_train, train, type = "class") == train$Type)/24
sum(predict(rt_train, valid, type = "class") == valid$Type)/8





p11 <- 1601
p12 <- 162527
p21 <- 510
p22 <- 412368
p11/(p11 + p12) - p21/(p21 + p22)
p11/(p11 + p12)/(p21/(p21 + p22))

(p11 * p22)/(p12 * p21)




# stat5044

#problem1
x <- c(0,1,2,3,4,5,6,7,8,9)
y <- c(98,135,162,178,221,232,283,300,374,395)
x <- cbind(rep(1,10),x)

r <- y - x %*% solve(t(x) %*% x) %*% t(x) %*% y  #residuals

lmfit <- lm(y~x)
plot(fitted(lmfit),residuals(lmfit),xlab = "Fitted", ylab = "Residual")
plot(fitted(lmfit),abs(residuals(lmfit)),xlab = "Fitted", ylab = "|Residual|")
#linearity verified
residuals(lmfit)
# n1=5=n2, rL=2, rU=10, runs=3, fails to reject randomness.

# BF test for homoscedasticity
r1 <- residuals(lmfit)[4:8]
r2 <- residuals(lmfit)[-c(4,5,6,7,8)]
n1 <- 5
n2 <- 5
r1nod <- median(r1)
r2nod <- median(r2)
d1 <- abs(r1 - r1nod)
d2 <- abs(r2 - r2nod)
s <-(sum((d1 - mean(d1))^2) + sum((d2 - mean(d2))^2))/(10-2)
t_bf <- (mean(d1) - mean(d2))/sqrt((1/n1 + 1/n2) * s) # t distribution with df 5+5-2=8

pt(t_bf, 8, lower.tail = FALSE, log.p = FALSE) # P-value is .159, fail to reject constant variance

#test for normality
shapiro.test(residuals(lmfit)) # fail to reject normality

#Problem 2
library(dplyr)
library(ggplot2)
airfreight <- read.table("airfreight.txt")
airfreight <- airfreight[-1,]
airfreight <- airfreight %>% as.data.frame() %>%
  rename(transfer = V1, arrival = V2) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character, as.numeric)

#a
X <- cbind(rep(1,10),airfreight$transfer)
Y <- airfreight$arrival
h <- X %*% solve(t(X) %*% X) %*% t(X)
Sxx <- sum((X[,2] - mean(X[,2])) ^ 2)
Sxy <- sum((X[,2] - mean(X[,2])) * (Y - mean(Y)))
beta1_hat <- Sxy/Sxx
beta0_hat <- mean(Y) - beta1_hat * mean(X[,2])

ggplot(data = airfreight, aes(x = transfer, y = arrival)) + geom_point() + 
  stat_function(fun = function(x) 10.2 + 4*x) + 
  labs(title = "Intercept =4， Slope = 10.2")

#b
sigma_hat <- t(Y) %*% (diag(1,10,10) - h) %*% Y/8
t <- qt(.05/2,8,lower.tail = FALSE)
CI <- rbind(beta0_hat,beta1_hat)
CI <- CI %>% as.data.frame() %>%
  rename(parameter = V1) %>%
  mutate(uppertail = parameter + t * rbind(sqrt(sigma_hat * (1 / 10 + mean(X[,2]) ^ 2 / Sxx)), sqrt(sigma_hat/Sxx)), 
        lowertail = parameter - t * rbind(sqrt(sigma_hat * (1 / 10 + mean(X[,2]) ^ 2 / Sxx)), sqrt(sigma_hat/Sxx)))

#c
residual <- Y - X %*% rbind(beta0_hat,beta1_hat)
residual <- as.data.frame(cbind(residual,X %*% rbind(beta0_hat,beta1_hat)))
colnames(residual) <- c("residual","Y_bar")
ggplot(data = residual, aes(x=Y_bar,y=residual)) + geom_point() + geom_abline(intercept = 0)
#there is a linear relationship because residuals are spread equally along the ranges of predictors


#Problem 3
beta1 <- rep(0,1000)
for (i in 1:1000) {
dat_boostrap <- airfreight[sample(nrow(airfreight),20,replace = TRUE),]
X1 <- cbind(rep(1,20),dat_boostrap$transfer)
Y1 <- dat_boostrap$arrival
beta1[i] <- c(0,1) %*% solve(t(X1) %*% X1) %*% t(X1) %*% Y1
}
beta1_boostrap <- sum(beta1)/1000
quantile(beta1, probs = c(0.025,0.975)) 
#bootstrap is asymptotically more consistent than the standard intervals obtained using sample variance and assumptions of normality.
#bootstrap automatically makes assumption of independence.


#Problem4
for (i in 1:5) {
r1 <- sample(residuals(lmfit),5,replace = FALSE)
r2 <- setdiff(residuals(lmfit),r1)
n1 <- 5
n2 <- 5
r1nod <- median(r1)
r2nod <- median(r2)
d1 <- abs(r1 - r1nod)
d2 <- abs(r2 - r2nod)
s <-(sum((d1 - mean(d1))^2) + sum((d2 - mean(d2))^2))/(10-2)
t_bf <- (mean(d1) - mean(d2))/sqrt((1/n1 + 1/n2) * s) # t distribution with df 5+5-2=8

print(pt(t_bf, 8, lower.tail = FALSE, log.p = FALSE)) # P-value is .159, fail to reject constant variance
}

library(lmtest)
bptest(lmfit)


x <- c(0,1,2,3,4,5,6,7,8,9)
y <- c(98,135,162,178,221,232,283,300,374,395)
x <- cbind(rep(1,10),x)



t <- lm(r^2~x)
summary(t)
summary(lmfit)
sum(((x %*% solve(t(x) %*% x) %*% t(x) %*% (r^2)) - mean((r^2)))^2)

181.1^2 * 8

sum((r^2 - mean(r^2))^2) - 181.1^2 * 8 #my

sum((x %*% solve(t(x) %*% x) %*% t(x) %*% (r^2) - r^2)^2) /8


ssr_star <- sum((x %*% solve(t(x) %*% x) %*% t(x) %*% (r^2) - mean(r^2))^2)

sse <- sum((x %*% solve(t(x) %*% x) %*% t(x) %*% y - y)^2)
(ssr_star/2) / ((sse/10)^2)
pchisq((ssr_star/2) / ((sse/10)^2), 1, ncp = 0, lower.tail = F)


a <- sse/8

sqrt(a)




#stat5034 hw 4

#p1
#a
library(ggplot2)


f <- function(alpha,n){
  m <- matrix(0,ncol = n,nrow = 1000) 
  for (i in 1:1000) {
    m[i,] <- rbeta(n, shape1 = alpha, shape2 = 1, ncp = 0)
  }
  return(rowMeans(m))
}

alpha <- 0.5
n <- 5
avg <- f(alpha,n)

ggplot(data = as.data.frame(avg),aes(x=avg)) + geom_histogram(aes(y = ..density..)) + 
  stat_function(fun = dnorm, args = list(mean = alpha/(alpha+1), sd = sqrt(alpha*1/((alpha + 1)^2 * (alpha + 2))/n)))


m <- matrix(0,ncol = 30,nrow = 1000)
for (i in 1:1000) {
  m[i,] <- rbeta(n=30, shape1=.5, shape2=1, ncp = 0)
}
avg <- rowMeans(m)
hist(avg)

m <- matrix(0,ncol = 100,nrow = 1000)
for (i in 1:1000) {
  m[i,] <- rbeta(n=100, shape1=.5, shape2=1, ncp = 0)
}
avg <- rowMeans(m)
hist(avg)


hist(rbeta(n=100, shape1=5, shape2=1, ncp = 0))
density(rbeta(n=100, shape1=.5, shape2=1, ncp = 0))
