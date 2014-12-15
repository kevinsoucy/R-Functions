# Before fitting
# Plot y vs each x, must be linearly related without skew, no outliers, normal residuals, 
# homoskedastic, residuals symmetric about 0 with no discernible pattern. 
# Heteroskedasticity may be due to correlations with other variables, can't 
# determine homoskedasicity before fiting model. Key assumption is constant standard deviation

# Plot all pairwise comparisons bw x's - looking for: 
#Transform Variables - y: try y^a, ln(y), ln(1/y-1) 

#Function that plots all transformations of x: sqrt(), ^2, log, 1/x,exp() , -x^-2
plotAllTrans = function(X,Y) {
  par(mfrow=c(2,4))
  plot(X,Y, main="Y vs. X")
  plot(sqrt(X),Y, main="Y vs. sqrt X")
  plot((X^2),Y, main="Y vs. X^2")
  plot(log(X),Y,  main="Y vs. log X")
  plot(1/(X), Y, main="Y vs. 1/X")
  plot(exp(X),Y, main="Y vs. exp X")
  plot(1/sqrt((X)),Y, main="Y vs. 1/sqrt X")
  plot(-X^-.9, Y, main="Y vs. -X^-.9")
  plot(log(X),log(Y), main= "logY vs. log X")
  plot(X, log(1/Y),  main="log 1/Y  vs. X")
  plot(X,log(Y),  main="logY vs.X")
  plot(X,Y^2,  main="Y^2 vs. X")
  plot(X^2,Y^2, main="Y^2 vs. X^2")
  plot(X^2, Y^.9,main="Y^.9 vs. X^2")
  plot(X^2,Y^.01, main="Y^.01 vs. X^2")
}
par(mfrow=c(2,3))
par(mfrow=c(1,1))

#Plot all X^+powers no Y

#Plot all X^-powers no Y

#Plot all X^+ Y+-

#Plot all X^+ Y+

#Plot all -X^- Y+

 #Plot all -X^- Y-



Change 0's to na's
is.na(hospital$SALES) = !hospital$SALES


# Fit Model
fit1 = lm(y~x1+x2..., data=df)
# Plot residuals vs fitted values : roughly horizontal centered and symmetric about 0,  Look for large outliers (>2std)
plot(fit1$residuals~fit1$fitted.values, main="Residuals vs. Fitted Values", ylab="Residuals", xlab="Fitted Values")
abline(h=0)
#Plot residuals vs all x's,roughly horizontal centered and symmetric about 0
plot(fit1$residuals~x1, ylab="Residuals", xlab="Transformed X1", main= "Residuals vs. Transformed X1")
abline(h=0)

#F-value: MSmodel / MSerror k = 5 betas
df reg = (k-1) =4, df error = N-k  = 77
#r1 = residuals
SSE = 714.9375
SSR = 7392.525
MSR = 1848.131
MSE = 9.284903

#plot normal qq plot of residuals(should be roughly linear)
par(mfrow=c(1,1))
qqnorm(fit1$residuals, main="Normal Q-Q Plot of Residuals: First Iteration")
