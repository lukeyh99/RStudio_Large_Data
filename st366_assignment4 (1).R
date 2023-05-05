#Name:Luke Hartnett
#Student num;18375011
#Q6:
#a
n <- 100
mu <- 3
sigma <- 5
x <- rnorm(n, mu, sigma)
g <- rep(1:20, each=5)
y <- tapply(x,g,mean)

#b
u <- 0
j <- 0
y2 <- numeric(20)
s <- seq(1,100 , by = 5)
for(i in s)
{
  u <- u + 5
  j <- j + 1
  
  y2[j] <- mean(c(x[i:u]))
}
y2
all.equal(y,y2)

#c
index <- seq(2.5,97.5,length = 20)
plot(index,y2,ylim=range(x)*1.1, type="o", col = "red" )
points(x,cex = .8, pch = 21, bg = "lightgray")

#d
upr <- mu + 1.96*sigma 
lwr <- mu - 1.96*sigma
abline( h = upr , lty = 2)
abline( h = lwr , lty = 2)
abline( h = mean(x) , lty = 2)
#e
which(x>upr|x<lwr)
#25 29 33 36 41 45 52 59 62 66 67 95
#f
gcb <- which(x<lwr | x > upr, arr.ind = TRUE)
points(gcb,x[gcb],pch = 21 ,bg = "blue")

#g
test0 <- function(x,upr,lwr)
{
  indices <- which(x<lwr | x > upr, arr.ind = TRUE)
  return(indices)
  
}
#h
test0(x,upr,lwr)
#25 29 33 36 41 45 52 59 62 66 67 95
#i
jump <- function(x)
{
  xDiff <- diff(x) 
  xAbs <- abs(xDiff)
  return(max(xAbs))
}
jump(x) 
#21.03803

#q1
A <- matrix(c(1,1,1,1,1,2,1,3,7,6,4,1,9,49,36,8,1,27,343,216),nrow = 5 , ncol = 4)
A
#b
x <- c()
m <- 2
powermat <- function(x,m)
{
  
}