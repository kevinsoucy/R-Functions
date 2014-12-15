#Text Mining
#Phrase Counts, frequencies assigned to words and word combinations
#need to pre-process text, 
# Porter Stemming Algo: cut words to root: taxing, taxes, taxation all => tax
# tf-idf = term frequency-inverse document frequency score = (tf*idf)
#tf = term frequency in single doc = (# times specific word appears in doc / doc word count)
#idf inverse document frequency = log(total docs / docs containg word)

#Inverse Multinomial Logistic Regression
# P(y=k) = [exp(alphak+x*Betak)] / sum[exp(alphah+xBetah)] for k 1,2...g
library(textir)
mnlm() #penalty based estimation/variable selection, result could be review rating etc..

data(we8there) ## 6166 reviews and 2640 bigrams
we8mnlm=mnlm(we8thereCounts, overall,bins=5) ## bins: for faster inference if covariates are factors

## covariate is a factor with 5 levels

we8mnlm$intercept ## estimates of alphas
we8mnlm$loadings  ## estimates of betas

fitted(we8mnlm)
as.matrix(fitted(we8mnlm))[1,] ## fitted counts for first review

## following provides fitted multinomial probabilities

pred=predict(we8mnlm,overall,type="response")
pred[1,] ## predicted multinomial probs for review 1

sum(pred[1,]) ## must add to one


predinv=predict(we8mnlm,we8thereCounts,type="reduction") ##  predicts inverse prediction (fitted reduction)
predinv[1:10]  ## prints predicted ratings for first 10 reviews

plot(predinv)
plot(predinv~overall)
corr(predinv, overall)

## ROC curve for classification of y with p

roc <- function(p,y){
  
  y <- factor(y)
  
  n <- length(p)
  
  p <- as.vector(p)
  
  Q <- p > matrix(rep(seq(0,1,length=500),n),ncol=500,byrow=TRUE)
  
  fp <- colSums((y==levels(y)[1])*Q)/sum(y==levels(y)[1])
  
  tp <- colSums((y==levels(y)[2])*Q)/sum(y==levels(y)[2])
  
  plot(fp, tp, xlab="1-Specificity", ylab="Sensitivity")
  
  abline(a=0,b=1,lty=2,col=8)
  
}

c2=overall==4

c3=overall==5

c=c2+c3

min=min(predinv)

max=max(predinv)

pp=(predinv-min)/(max-min)

## plot of ROC curve

roc(p=pp, y=c)

cut <- 0 

truepos <- c==1 & predinv>=cut

trueneg <- c==0 & predinv<cut

# hit-rate / sensitivity (predict good review if review is good)

sum(truepos)/sum(c==1)
sum(trueneg)/sum(c==0)
