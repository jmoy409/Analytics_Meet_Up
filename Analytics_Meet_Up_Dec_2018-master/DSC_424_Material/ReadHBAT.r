
library(foreign)  # Allows us to read spss files!
library(corrplot)
library(car)
library(QuantPsyc)
library(leaps)
library(RColorBrewer)
library(Hmisc)
library(psych)

# Set the working directory
setwd("/home/jmoy001/Documents/Presentation/DSC_424_Material")

# Read in the hbat spss "hbat" dataset from the book by Hair, et. al.
hbat = read.spss("/home/jmoy001/Documents/Presentation/DSC_424_Material/HBAT.sav", to.data.frame=T,use.value.labels=FALSE)
attr(hbat,'variable.labels')
#names(hbat) <- c("ID", "X1-Customer Type", "X2-Industry Type","X3-Firm Size", "X4-Region","X5-Distribution System",
           #"X6-Product Quality","X7-E-Commerce Activities","X8-Technical Suppport","X9-Complaint Resolution",
           #"X10-Advertising","X11-Product Line","X12-Salesforce Image","X13-Competitive Pricing",
           #"X14-Warranty and Claims","X15-New Products","X16-Order and Billing","X17-Price Flexibility", 
           #"X18-Delivery Speed","X19-Satisfaction","X20-Likely to Recommend","X21-Likely to Purchase",
           #"X22-Purchase Level","X23-Consider Strategic Alliance")
str(hbat)
head(hbat)

#Check frequencies and missing values for all variables or a specific variable
describe(hbat)

#Check for specific values missing
is.na(hbat$x6) # returns TRUE if a specific value of x6 variable is missing

# Pull out just the numeric fields and place customer satisfaction 
# at the front because it will be our "Y".  It makes it easier to 
# interpret correlation matrices!

#Change Variable Names

hbatNumeric = hbat[, c(20, 7:19)]
head(hbatNumeric)
plot(hbatNumeric)

#Check data types
str(hbatNumeric)

# Compute the correlation matrix and visualize it
cor.hbat = cor(hbatNumeric)
cor.hbat
corrplot(cor.hbat, method="ellipse")
corrplot(cor.hbat, method="number")
corrplot(cor.hbat, method="circle",col=c("yellow", "red","blue","green"))
corrplot(cor.hbat, method="color", col=brewer.pal(n=8, name="RdYlBu"))
#see http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram for more options

#Show full color palette
display.brewer.all()
         
# Try a fit of the full set of parameters.  Note that the . in 
# the regression formula gives all the rest of the parameters.
# Based upon correlations X8, X9, X14, X18 have correlations greater than 0.80.
# Based upon domain knowledge, exclude X14 and X18 and keep in X8 and X9
attr(hbat,'variable.labels')
fullFit = lm(x19 ~ ., data=hbatNumeric)
summary(fullFit)

lm.beta.fullfit <- lm.beta(fullFit)
lm.beta.fullfit

# And compute the vif scores to get an idea of the multicolinearities here!
vif(fullFit)

#Refer to Correlation Plot for Correlations of X18 and X14
corrplot(cor.hbat, method="number")

fullFit1 = lm(x19 ~ . - x18 -x14, data=hbatNumeric)

# And compute the vif scores to get an idea of the multicolinearities here!
vif(fullFit1)

# Now, fit with a single parameter (the one with the highest correlation)
fit1 = lm(x19 ~ x9, data=hbatNumeric)
summary(fit1)

# Get the standardized coefficients for x9
# List of Variable Importance
lm.beta.fit1<-lm.beta(fit1)
lm.beta.fit1

# Try adding another parameter (this one has the most correlation with the residuals of the last fit)
fit2 = lm(x19 ~ x9 + x6, data=hbatNumeric)
summary(fit2)
vif(fit2)


###################################################################
# Automated fitting
###################################################################


# The leaps package has a beautiful subset search routine that also provides a viaualization
# of its results
hbatSubsets = regsubsets(x19 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data=hbatNumeric, nbest=10)
hbatSubsets

plot(hbatSubsets, scale="adjr2")


bestR2Fit = lm(x19 ~ x6 + x7 + x9 + x11 + x12 + x16, data=hbatNumeric)
summary(bestR2Fit)

# The R function "step" can perform stepwise regression, but to get going, we need to feed 
# it the two "bounding" models so that it knows what to work with
null = lm(x19 ~ 1, data=hbatNumeric)
null

full = lm(x19 ~ ., data=hbatNumeric)
full

# First we do a forward search - Forward Stepwise
hbatForward = step(null, scope = list(lower=null, upper=full), direction="forward")
summary(hbatForward)
# Compare the results to the full search above

# Next do a backward search - Backward Stepwise
hbatBackward = step(full, direction="backward")
summary(hbatBackward)

# Finally we do a "stepwise" search combining the two - Both Forward and Backward Stepwise
hbatStep = step(null, scope = list(upper=full), direction="both")
summary(hbatStep)
summary(hbatForward)
summary(hbatBackward)

# Things are really nice if they all agree!

dim(hbatNumeric)

library(psych)
options("scipen"=100, "digits"=2)
cor(hbatNumeric[, 2:14])
MCorrTest = corr.test(hbatNumeric[, 2:14], adjust="none")
MCorrTest

M = MCorrTest$p
M

# The probability matrix uses a different test above the diagonal, we are just interested in 
# the entries below the diagonal, so we make it symmetric
for (i in 2:nrow(M))
  for (j in 1:(i-1))  # Only grab elements below the diagonal
    M[j, i] = M[i,j]  # Copy into the corresponding element above the diagonal
M

# Now, for each element, see if it is < .01 (or whatever significance) and set the entry to 
# true = significant or false
MTest = ifelse(M < .01, T, F)
MTest

# Now lets see how many significant correlations there are for each variable.  We can do
# this by summing the columns of the matrix
colSums(MTest) - 1  # We have to subtract 1 for the diagonal elements (self-correlation)



