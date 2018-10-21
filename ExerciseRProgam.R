#Differentiation

#Number 1
num1 <- function(x){
  return(sqrt(x)*(x+1))
}
library(Ryacas)
x <- Sym("x")
Simplify(deriv(sqrt(x))*x+1)

#Number 2
num2 <- function(x){
  return(2*x^2-3/sqrt(x))
}
library(Ryacas)
x<-Sym("x")
Simplify(deriv(2*x^2-3/sqrt(x)))

#Number 3
num3 <- function(x){
  return(x-1/x+1)
}
library(Ryacas)
x<-Sym("x")
Simplify(deriv(x-1/x+1))

num4 <- function(x){
  return(x^-4)
}
library(Ryacas)
x<-Sym("x")
Simplify(deriv(x^-4))



#Limits

#Number 1
f1 <- function (x){
  fx<- (1-cos(x))/x
  return(fx)
}
library(Ryacas)
x<- Sym("x")
Limit (f(x),x,0)

#Number 2
f2 <- function (x){
  f2<- (2*(-3+x)^2 - 18)/x
  return(f2)
}
library(Ryacas)
x<- Sym("x")
Limit (f(x),x,0)

#Number 3
f3 <- function (x){
  f3<- (t-sqrt(3*x+4))/4-x
  return(f3)
}
library(Ryacas)
x<- Sym("x")
Limit (f(x),x,4)


#Integral
#Number 1
f <- function(x){
  return(2*x^3)
}
integrate(f, lower = 0, upper = 3 )
library(Ryacas)
x<- Sym("x")
Integrate(2*x^3,x)

#Number 2
f<-function(x){
  return(1-5*x^4)
}
integrate(f, lower = -1 , upper = 2)
library(Ryacas)
x<-Sym("x")
Integrate(1-5*x^4, x)

#Number 3
f<-function(x){
  return(x^4-3*x^2+5)
}
integrate(f, lower = -2 , upper = 2)
library(Ryacas)
x<-Sym("x")
Integrate(x^4-3*x^2+5, x)

#Number 4
f<-function(x){
  return(x^2 + 1/2*sqrt(x))
}
integrate(f, lower = 1, upper = 4)
library(Ryacas)
x<-Sym("x")
Integrate(x^2 + 1/2*sqrt(x), x)

#Number 5
f<-function(x){
  return((2-3*x)^2)
}
integrate(f, lower = 0 , upper = 2)
library(Ryacas)
x<-Sym("x")
Integrate((2-3*x)^2, x)