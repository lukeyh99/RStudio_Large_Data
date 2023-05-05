#Name:Luke Hartnett
#Student Number: 18375011


#exam2021

#q1
#a
#tried subtracting 20 customers everytime the time was at a 10 minute mark
 N <- 0; t <- 1
 join <- function(mu , lambda,N,t) 
 while(N <= 50) {
   N <- N + join(lambda = 2 , mu = 20)
   if(t %% 10 == 0) N <- N-20
   t <- t + 10
 }
 #b
 queue <- function(N,t,lambda,mu)
 {
   while(N <= 50) {
     N <- N + join(lambda = 2 , mu = 20)
     if(t %% 10 == 0) N <- N-20
     t <- t + 10
   }
   N
 }
 N
z <- rep(queue(N = 0,t = 1 , lambda = 2 , mu = 20),1000)

#c
N <- 0; t <- 1
join <- function(mu , lambda,N,t) 
  while( N <= 50 && t <= 300 ) {
    N <- N + join(lambda = 2 , mu = 20)
    if(t %% 1 ==  0) N <- N + 2
    t <- t + 1
  }
 

#2
#a
X <- matrix(c(1,1,1,0,1,1,2,4,2,1,2,3,5,6,7),nrow = 3 , ncol = 5)
X
#b
X_svd <- svd(X)
#c
X2 <- V %*% Dx %*% t(U)
U <- X_svd$u
V <- X_svd$v
Dx <- diag(X_svd$d)
#d
library(magrittr)

X <- V %*% tcrossprod(Dx,U)
all.equal(X,X2)

#e
all.equal(tcrossprod(X),(V %*% tcrossprod(Dx,V) ))

#2
#a
#create a new set of data for when its greater than 3
iris2 <- filter(iris, iris$Sepal.Width %>%
                  is_greater_than(3))
iris2$Petal.Length/2 %>%
  multiply_by(iris2$Petal.Width/2) %>%
  multiply_by(pi)


iris2$Petal.Length/2 %>%
  multiply_by(iris2$Petal.Width/2) %>%
  multiply_by(pi) %>%
  median
  
  #b

while(iris$Sepal.Width > 3)
{
  mean_petal_area <- c(mean(((pi*iris$Petal.Length/2)*iris$Petal.Width)/2))
 
}
mean_petal_area
#c
library(ggplot2)
##couldnt get it to output a graph unsure why but this whas what i was aiming for
ggplot(iris2, aes(aes(iris2$Species, mean_petal_area )) +
  geom_bar(aes(color = iris2$Species)) +
  geom_smooth(method ="lm") +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()

 
#4
#a
set.seed(18375011)
box_muller <- function(numpairs)
{
  
  unif <- runif(numpairs , min = 0 , max = 5000)
  theta <- runif(numpairs, 0, 2 * pi)
  E <- -log(numpairs) 
  R <- sqrt(2) * cos(theta)
  x
  
}
#c
simc <- rnorm(5000 , mean = 10 , sd = 3),
##used rnorm as i couldnt figure out the box muller funtion
#d
 plot(density(simc), main=expression(paste("N(", mu, "=0,", sigma^2, "=1)"))),
 curve(dnorm, add=TRUE, col="red"),


#e
 #i
 #was getting 10000 rows instead of 10000 columns so had to use this as i was runing out of time
 biv <- rep(simc,10000)
matrix(biv) 
#ii
sigma <- matrix(c(2,-1,-1,2), nrow = 2, ncol = 2)
sigma
#iii
SIG12 <- chol(sigma)
SIG12
#iv
MU <- c(1,8)
#v
BIV <- MU + SIG12 %*% biv
#vi
library("MASS")
kde2d(BIV[1,], BIV[2,])
contour(kde2d(BIV[1,], BIV[2,]))
#i believe all the answers woul have worked i i got biv correct
