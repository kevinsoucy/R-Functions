##===========================================
## Chapter 12 Linear Discriminant Analysis
##
## Example 2:  Fisher iris Data
##===========================================

library(MASS)  ## includes lda and qda for discriminant analysis
set.seed(1)

## linear discriminant analysis
## equal prior probabilities as same number from each species  
zlin=lda(Species ~., data=iris)
zlin;
plot(zlin);


predict(zlin, newdata=data.frame(Sepal.Length=5.1,Sepal.Width=3.5,Petal.Length=1.4, Petal.Width=0.2))
predict(zlin, newdata=data.frame(Sepal.Length=5.1,Sepal.Width=3.5,Petal.Length=1.4, Petal.Width=0.2))$class

## quadratic discriminant analysis
zqua=lda(Species~., iris,prior=c(1,1,1)/3)
predict(zqua,newdata=data.frame(Sepal.Length=5.1,Sepal.Width=3.5,Petal.Length=1.4, Petal.Width=0.2))
predict(zqua,newdata=data.frame(Sepal.Length=5.1,Sepal.Width=3.5,Petal.Length=1.4, Petal.Width=0.2))$class

## cross-validation (1000 times)
n=150
nt=100
neval=n-nt
rep=1000

errlin=dim(rep)
errqua=dim(rep)
for (k in 1:rep) {
  train=sample(1:n,nt)
  ## linear discriminant analysis
  m1=lda(Species~., iris[train,], prior=c(1,1,1)/3)
  tablin=table(iris$Species[-train],predict(m1, iris[-train,])$class)
  errlin[k]=(neval-sum(diag(tablin)))/neval
  
  ## quadratic discriminant analysis
  m2=qda(Species~.,iris[train,],prior=c(1,1,1)/3)
  tablin=table(iris$Species[-train], predict(m2,iris[-train,])$class)
  errqua[k]=(neval-sum(diag(tablin)))/neval
}
merrlin=mean(errlin)
merrlin
merrqua=mean(errqua)
merrqua

