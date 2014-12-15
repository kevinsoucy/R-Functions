#Factor Analysis Notes

#Divide predictors into related groups, eg demographic info and
#number of different operations performed etc. 
#Run Factor Analysis on groups separately, must say how many latent variables/factors
# to use in analysis
#Tells you % of variance explained by one factor, where each factor is a linear comb
#of variables
#Rule of thumb is to exclude eigenvalues of correlation matrices that are less than 1
#CHOOSING NUMBER OF FACTORS: Graph of eigenvalues vs number of factors(SCREE plot)
SCREE option adds scree plot
PROC FACTOR data=train METHOD=PRIN NFACT=3 ROTATE=VARIMAX SCREE out=z1;
var  BEDS RBEDS OUTV ADM SIR TH trauma rehab;  FACTOR data=train METHOD=PRIN NFACT=4 ROTATE=VARIMAX out=z1;
var  


# Test the null hypothesis that there are m factors



#SAS
Data HOSpital;
run;
PROC FACTOR data=train METHOD=PRIN NFACT=2 out=z;   #prin = PCA
var  HIP95 KNEE95 HIP96 KNEE96 FEMUR96;
RUN;
PROC FACTOR METHOD=ML   NFACT=4;    RUN; #MAximum Likelihood
PROC FACTOR ROTATE=VARIMAX ROUND;   RUN; #Rotations
PROC FACTOR ROTATE=QUARTIMAX ROUND; RUN; #Rotations

