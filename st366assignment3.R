#Luke Hartnett
#18375011
#Q1
qroots <- function(a = 0,b,c) {
  d <- sqrt(b ^ 2 - 4 * a * c)
  return(c((-b - d) / (2 * a), (-b + d) / (2 * a)))
}
qroots(1,0,-1)
##a can never be zero because you cannot divide by zero

#Q2
#a
fibs <- function(n)
{
  fib <- numeric(n)
  fib[1:2] <- 1
  for (i in 3:n)
    {fib[i] <- fib[i-1] + fib[i-2]
  }
  fib
  
}  
#b
n <- 30
fib <- numeric(n)
fib[1:2] <- 1
for(i in 3:n){
  fib[i] <- fib[i - 1] + fib[i - 2]
}
fn <- fib[-1]
fn1 <- fib[-n]
fib
plot(fn/fn1)
#yes it does seem to be converging 
#c
 fib1 <- 1
 fib2 <- 1
 fibs <- c(fib1, fib2)
 n <- 2
 golden <- (1+sqrt(5))/2
 eps <- 0.001
 while(abs(fib2/fib1 - golden) >= eps) {
   fibnew <- fib1 + fib2
   fibs <- c(fibs, fibnew)
   fib1 <- fib2
   fib2 <- fibnew
   n <- n+1
   print(fib2/fib1)
 }
plot(fib2/fib1)
#yes it does seem to be converging on the one point


#5
 nmonths <- 0
 debt <- 1000
 while(debt > 0)
 {
   debt <- debt+debt*(0.11/12)
   debt <- debt - 12
   nmonths <- nmonths + 1
   
 }
 nmonths

 #159

 #b
 target <- 0
 owed <- numeric(159)
 nmonths <- 1
 i <- 0
 rate <- 0.11
 debt <- 1000
 #debt <- loan*(1+(0.11/12))
 while(debt > target)
 {
    debt <- debt+debt*(0.11/12)
    i <- i+1
    owed[i] <- debt
    debt <- debt -12
    
    
 }

owed
nmonths
 plot(1:159, owed, type = "s",xlab = "Number of months", ylab = "amount owed per month",main = "owed monthly")
 abline(h = target, lty = 2)
 
#c
sum(owed*rate)

