#PCA Principal Components Analysis
#For large numbers of correlated x variables, reduces number of x's
#Creates new independent variables that are linear combinations of the old correlated vars
# Output called principal components
# For p x's, there are z possible independent PC's, choose ones that contain most variation/information
# The 
## correlation matrix
df = car_data
categorical = c(1,)
cor(df[,-categorical])

pca1 <- prcomp(df[,-categorical], scale=TRUE) 
pca1
## we strip the first column (labels) from the data set
## scale = TRUE: variables are first standardized. Default is FALSE

plot(pca1, main="")
mtext(side=1, "TITLE Principal Components",  line=1, font=2)
