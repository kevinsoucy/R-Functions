#Split Data into training and test sets
TrainTestSets = function(df, trainPerc, trainName, testName, seed=1){
  set.seed=(seed)
  split = round(trainPerc*nrow(df))
  df$rand = sample(1:nrow(df), nrow(df), replace=F)
  assign(toString(trainName), df[df$rand<=split,],envir = .GlobalEnv)
  assign(toString(testName),  df[df$rand>split,], envir = .GlobalEnv)
}

# Classification Tree
library(MASS) 
library(tree)
## read in the iris data
dat = train[,-9:-10]
DataTree <- tree(Outcome~White+Male+DustHi+DustMedium+Smoker+MoreThan20+Tento20,data=train)

plot(DataTree)
plot(DataTree,col=8)
text(DataTree,digits=2)
summary(DataTree)

Datasnip=snip.tree(DataTree,nodes=c(7,12))
Datasnip
plot(Datasnip)
text(Datasnip)



