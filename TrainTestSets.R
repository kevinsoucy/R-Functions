#Split Data into training and test sets
TrainTestSets = function(df, trainPerc, trainName, testName, seed=1){
set.seed=(seed)
split = round(trainPerc*nrow(df))
df$rand = sample(1:nrow(df), nrow(df), replace=F)
assign(toString(trainName), df[df$rand<=split,],envir = .GlobalEnv)
assign(toString(testName),  df[df$rand>split,], envir = .GlobalEnv)
}