


# ---- C1 ----
#load libraries
library(foreign)  # Allows us to read spss files!
require(stats)
require(corrplot)
library(dplyr)
library(tidyr)
require(curl)
require(rio)
require(car)
require(glmnet)
require(rpart)
require(shinyFiles)
require(gvlma)
require(data.table)
require(ggplot2)
require(factoextra)
require(FactoMineR)
library(psych)

# ---- C2 ----
hbat = read.spss("/home/jmoy001/Documents/Presentation/DSC_424_Material/HBAT.sav", to.data.frame=T,use.value.labels=FALSE)
colnames(hbat)

attr(hbat,'variable.labels')

#Change Variable Names
colnames(hbat) <- attr(hbat,'variable.labels')
colnames(hbat)

#pull numeric columns only, remove dummy variable columns
hbatNumeric = hbat[, c(20, 7:19)]

##Correlation Plot
##Without dependent variable
#copy same table
hbatNumeric_Independent <- hbatNumeric

#remove dependent variable
X19_Satisfaction <-hbatNumeric_Independent$`X19 - Satisfaction`
hbatNumeric_Independent$`X19 - Satisfaction` <- NULL
corrplot(cor(hbatNumeric_Independent), method="number")

##Remove correlations greater than .70
#focus on upper triangle
pcor <- cor(hbatNumeric_Independent, use = "complete.obs")
pcor[upper.tri(pcor)] <- 0
diag(pcor) <- 0
upper.df <-hbatNumeric_Independent[,!apply(pcor,2,function(x) any(x > abs(0.7)))]
upper.df <- cbind(X19_Satisfaction,upper.df)


#regression Upper.df
fullFit_Upper = lm(X19_Satisfaction ~ ., data=upper.df)

#Regression results
summary(fullFit_Upper)
# ---- C2a ----
par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(fullFit_Upper)

# ---- C2b ----
gvlma(fullFit_Upper)

# ---- C2c ----
vif(fullFit_Upper)


# ---- C2d ----
#Symptoms of Multicollinearity

#Includes Dependent Variable
corrplot(cor(upper.df), method="number")
#first column only: Correlation of independent variables with dependent variable
cor(upper.df)[,1]

#reference from earlier, regression summary 
summary(fullFit_Upper)


#Reference 

# ---- C2e ----
#Ridge and Lasso Regression
hbatNumeric_Independent <- hbatNumeric
X19_Satisfaction <-hbatNumeric_Independent$`X19 - Satisfaction`
hbatNumeric_Independent$`X19 - Satisfaction` <- NULL


#Lasso
#alpha=1
#must read in x (independent variables) as matrix
lasso_model <- cv.glmnet(as.matrix(hbatNumeric_Independent), X19_Satisfaction, lambda=10^seq(4,-1,-.1), alpha=1)

#dotted line shows optimal lambda values
#Either 1 standard deviation above the best or the best lambda value
plot(lasso_model)

#log function to match plot view
Optimal_Lambda <- lasso_model$lambda.1se
Lasso_coefficients <- lasso_model$glmnet.fit$beta[ , lasso_model$glmnet.fit$lambda == Optimal_Lambda]

#Ridge
#alpha=0
#must read in x (independent variables) as matrix
ridge_model <- cv.glmnet(as.matrix(hbatNumeric_Independent), X19_Satisfaction, lambda=10^seq(4,-1,-.1), alpha=0)

#dotted line shows optimal lambda values
#Either 1 standard deviation above the best or the best lambda value
plot(ridge_model)

#log function to match plot view
Optimal_Lambda <- ridge_model$lambda.1se
Ridge_coefficients <- ridge_model$glmnet.fit$beta[ , ridge_model$glmnet.fit$lambda == Optimal_Lambda]


#ElasticNet
#alpha=.5
#must read in x (independent variables) as matrix
elastic_net_model <- cv.glmnet(as.matrix(hbatNumeric_Independent), X19_Satisfaction, lambda=10^seq(4,-1,-.1), alpha=0.5)

#dotted line shows optimal lambda values
#Either 1 standard deviation above the best or the best lambda value
plot(elastic_net_model)

#log function to match plot view
Optimal_Lambda <- elastic_net_model$lambda.1se
Elastic_net_coefficients <- elastic_net_model$glmnet.fit$beta[ , elastic_net_model$glmnet.fit$lambda == Optimal_Lambda]


coef_df <- data.table(Features = names(Lasso_coefficients),
                      Lasso= Lasso_coefficients,
                   Ridge= Ridge_coefficients,
                   Elastic_Net = Elastic_net_coefficients)

#wide to long
coef_df <- coef_df %>%
  tidyr::gather(Regularized_Regression, value, Lasso:Elastic_Net)

ggplot(coef_df, aes(Features, value, fill=Regularized_Regression)) + 
  coord_flip() + geom_bar(stat='identity') + 
  #split by test
  facet_wrap(~ Regularized_Regression) + 
  #remove legend
  guides(fill=FALSE)


# ---- C3a ----


library(psych)
options("scipen"=100, "digits"=5)


#see what the p values are for each bi-variate relationship
#independent variables only
MCorrTest = corr.test(hbatNumeric[, 2:14], adjust="none")
M = MCorrTest$p
MTest = ifelse(M < .01, T, F)


colSums(MTest) - 1  # We have to subtract 1 for the diagonal elements (self-correlation)
#shows the amount of variables that are correlated with each other
#run a PCA or factor analysis on these variables to remove the multicollinearity by removing redundant columns

### NO POINT OF PCA IF THERE IS NO MULTICOLLINEARITY. Purpose of PCA is to group variables into principal components

#if we do a PCA, component will drag all variables

# We remove two columns, the first (x15 = column 11) is not correlated with anything else
# The second (x17 = column 13) is correlated with too many other variables. It might create one component (HARD TO EXPLAIN!)
# keep x14 because it has relationship with other variables and could fall into another component

#1 taken out too since it is our dependent variable, customer satisfaction
#17 is taken out because it might create 1 component, because it is related to everything else, may push the 9
#related variables to 1 component vs. being spread out to multiple components

#remove those 3 columns
hbatReduced = hbatNumeric[, -c(1, 11, 13)]



# ---- C3b ----
p = prcomp(hbatReduced, center=T, scale=T)
#scale=T, using correlation matrix. If same units, scale=F and using co-variance matrix

plot(p)
#need to add horizontal y=1 to see which eigen values are greater than 1
abline(1, 0)

#4 principal components


# ---- C3c ----
#Number of components where Eigenvalue evens out
plot(p)

# 4 principal components


# ---- C3d ----
summary(p)


# 3-4 principal components

# ---- C3e ----

p2 = psych::principal(hbatReduced, covar=FALSE, rotate="varimax", nfactors=4, scores=TRUE, oblique.scores=TRUE)

p2
#Ideally want a low RMSE and p value less than .05
#4 components have an error rate of .01%


#Loading - correlation between the factor and the variable. Interpret them as beta coefficients in PCA
#Ex) A high loading suggests a variable has a high influence within that factor

#Scores - Loadings multiplied by variable gives the formula of the score.
#Ex) The variables that makes up that component

#Purpose of rotation?
#Rotate components to redistributes the variance and makes the loadings easier to interpret.


print(p2$loadings, cutoff=.4, sort=T)
#9, 16, 18, 11 are part of component 1
#remember, 11 is called a cross loading because in 2 different areas(could be more). variable should be 1 per component
#instead of asking a cutoff of .4, try asking for a cutoff of .6
print(p2$loadings, cutoff=.6, sort=T)

#Cross loadings removed
#Now we would need to be able to create names for the new variables to be interpretable.


# ---- C4a ---


df <- read.csv("/home/jmoy001/Documents/Presentation/2016 School Explorer.csv")

#change from N/A to NA
df[df=="N/A"] <- NA


#total
sum(is.na(df))

#NA for each variable, hidden because it was 8 pages, but the variables of interest for PCA had no NAs
#sapply(df, function(x) sum(is.na(x)))

#select columns that start with Grade
test <- df %>%
  select(starts_with("Grade"))

#Remove Grades, Grade Low, Grade high
#Grades shows all grades the school has data on
#Grade low is the lowest grade taught, Grade high is the highest grade taught
test <- test[,-c(1,2,3)] 

#remove columns of sum total
final <- test %>%
  select(-contains("All"))


MCorrTest = corr.test(final, adjust="none")

M = MCorrTest$p

MTest = ifelse(M < .01, T, F)

# Amount of variables that are correlated with other variables
colSums(MTest) - 1


# ---- C4b ---

#how to check for Factorability: Steps to test if PCA can be done with this data.
#pulled KMO and Barlett's test from this website
#http://minato.sip21c.org/swtips/factor-in-R.pdf

#KMO Function
kmo <- function(x)
{
  x <- subset(x, complete.cases(x)) # Omit missing values
  r <- cor(x) # Correlation matrix
  r2 <- r^2 # Squared correlation coefficients
  i <- solve(r) # Inverse matrix of correlation matrix
  d <- diag(i) # Diagonal elements of inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2 # Squared partial correlation coefficients
  diag(r2) <- diag(p2) <- 0 # Delete diagonal elements
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}


kmo(final)

#The overall test provided a value of .81, which is greater than .70. Thus, the data meets the sampling adequacy.

# ---- C4c ---

#Barlett Function
Bartlett.sphericity.test <- function(x)
{
  method <- "Bartlett's test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x)) # Omit missing values
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "X-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
                        method=method, data.name=data.name), class="htest"))
}

Bartlett.sphericity.test(final)

#With the p-value being less than .05, there is sufficient variance in the data to run a PCA.


# ---- C4d ---

psych::alpha(final, check.keys= TRUE)

#With the reliability value at .9, it shows that the groupings of the variables makes sense and is sufficient enough for PCA


# ---- C4e ---

#PCA
PCA_2a <- prcomp(final, center=T, scale=T)
plot(PCA_2a)
abline(1,0)

#it seems that the first 10 components have an eigen value greater than 1. Let's now look at the cumulative proportion of variance that each component has.

summary(PCA_2a)

# with 9 components, 62.2% of the variance is explained. Lastly, lets look at the knee of the plot.


pCA_2A1 <- PCA(final, graph = FALSE)
fviz_eig(pCA_2A1, addlabels = TRUE, ylim = c(0, 35), ncp=30)

#It looks like after 6 components, the plot levels off.

#From the 3 tests, let's run a PCA using 6 principal components


pca_2b <- psych::principal(final, rotate="varimax", nfactors=6)
pca_2b

print(pca_2b$loadings, cutoff=.44, sort=T)

#It seems that RC1 has Hispanic or Latino grouped with Asian or Pacitic Islander. Perhaps we can increase the groupings to see if more components would separate the grade and ethnicity for each variable that would make the component groupings make sense.

#increase components to 9, since 9 components had 62.2% of the data, which is within 60-80% variance that we should have.
pca_2b <- psych::principal(final, rotate="varimax", nfactors=9)

print(pca_2b$loadings, cutoff=.46, sort=T)

#ensure that varimax was the appropriate rotation.
round(cor(pca_2b$scores),2)



# ---- C5a ----
#look at factor analysis, see if 4 components is still a good model to use
#simpliest way to drop a variable is to set it equal to null
fit = factanal(hbatReduced, 4)
#.51 takes out cross loading
print(fit$loadings, cutoff=.51, sort=T)




