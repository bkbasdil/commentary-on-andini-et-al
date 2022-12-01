################################################################################
###################### MACHINE LEARNING PROJECT: R CODE#########################
################################################################################
# TITLE: COMMENTARY ON PAPER "Targeting with machine learning: An application  #
#        to a tax rebate program in Italy"                                     #
# AUTHORS: Baris Kaan BASDIL, Nikolaos PAPOULIAS                               #
# CREATED: 27/11/2022                                                          #
# LAST MODIFIED: 01/12/2022                                                    #
# LAST MODIFIED BY: Nikolaos PAPOULIAS                                         #
################################################################################

# Preamble 1: Necessary packages
  # Please install relevant packages
  library(beepr) # This will be useful when we run some long codes below
  library(haven) # To import STATA files
  library(xtable) # To create LaTeX tables
  library(purrr) # For uniform distribution
  library(readxl) # For reading excel files
  library(ggplot2) # For ggplots

# Preamble 2: Importing the dataset

rfam10 <- read_dta("C:/Users/kaanb/Downloads/ind10_stata/rfam10.dta")
ricfam10 <- read_dta("C:/Users/kaanb/Downloads/ind10_stata/ricfam10.dta")
risfam10 <- read_dta("C:/Users/kaanb/Downloads/ind10_stata/risfam10.dta")
rper10 <- read_dta("C:/Users/kaanb/Downloads/ind10_stata/rper10.dta")
  
rfam12 <- read_dta("C:/Users/kaanb/Downloads/ind12_stata/rfam12.dta")
ricfam12 <- read_dta("C:/Users/kaanb/Downloads/ind12_stata/ricfam12.dta")
risfam12 <- read_dta("C:/Users/kaanb/Downloads/ind12_stata/risfam12.dta")
rper12 <- read_dta("C:/Users/kaanb/Downloads/ind12_stata/rper12.dta")

rfam14 <- read_dta("C:/Users/kaanb/Downloads/ind14_stata/rfam14.dta")
ricfam14 <- read_dta("C:/Users/kaanb/Downloads/ind14_stata/ricfam14.dta")
risfam14 <- read_dta("C:/Users/kaanb/Downloads/ind14_stata/risfam14.dta")
rper14 <- read_dta("C:/Users/kaanb/Downloads/ind14_stata/rper14.dta")

rvar <- read_excel("train_indicator.xlsx")
rvar <- rvar[,2]

# Cleaning the Data for 2010

# Checking whether the indices are the same for rfam, ricfam, and risfam
sum(rfam10$nquest != ricfam10$nquest)
sum(risfam10$nquest != ricfam10$nquest)
# Indeed, they are equal

# Now, what we need to do is to calculate min and max income per household
# First, we create a matrix with the first column being household codes

y <- matrix(0, nrow = nrow(rfam10), ncol = 3)
y[,1] <- rfam10$nquest # Now, the first column is the household code

# The following code will impute next to each household name the maximum 
# income

summary(rper10$yl)

y[,2] <- 0 # The column now is all minimums

for (i in 1:nrow(y)) {
  for (j in 1:nrow(rper10)) {
    if(y[i,1] == rper10$nquest[j]) {
      if(y[i,2] < rper10$yl[j]) {
        y[i,2] <- rper10$yl[j]
      }
    }
  }
}

# The following code will impute next to each household name the minimum 
# income

y[,3] <- 147500 # The column now is all maximums

for (i in 1:nrow(y)) {
  for (j in 1:nrow(rper10)) {
    if(y[i,1] == rper10$nquest[j]) {
      if(y[i,3] > rper10$yl[j]) {
        y[i,3] <- rper10$yl[j]
      }
    }
  }
}


ytemp <- y # Keeping y intact

ytemp <- cbind(y,ricfam10$af, rfam10$ycf, rfam10$yl)

colnames(ytemp) <- c("ID", "MinIncome", "MaxIncome", "Financial Assets",
                     "FinancialIncome", "Disposable Income") 

ytemp <- ytemp[ytemp[,3] != 0,] # Dropping non-zero households
summary(ytemp) # Indeed, we don't have any zero-income households
nrow(ytemp) # There are 2205 such households

# Now, since we have cleaned the data for the 2010 sample, we can start
# cleaning the data for the 2012 sample. After this, we will do PCA to
# get the parameters.

# Cleaning the Data for 2012

# Checking whether the indices are the same for rfam, ricfam, and risfam
sum(rfam12$nquest != ricfam12$nquest)
sum(risfam12$nquest != ricfam12$nquest)
# Indeed, they are equal

# Now, what we need to do is to calculate min and max income per household
# First, we create a matrix with the first column being household codes

y2 <- matrix(0, nrow = nrow(rfam12), ncol = 3)
y2[,1] <- rfam12$nquest # Now, the first column is the household code

# The following code will impute next to each household name the maximum 
# income

summary(rper12$yl) # To check the minimum by hand. This will ease computation
# for the following double-loop.

y2[,2] <- 0 # The column now is all minimums

for (i in 1:nrow(y2)) {
  for (j in 1:nrow(rper12)) {
    if(y2[i,1] == rper12$nquest[j]) {
      if(y2[i,2] < rper12$yl[j]) {
        y2[i,2] <- rper12$yl[j]
      }
    }
  }
}

# The following code will impute next to each household name the minimum 
# income

summary(rper12$yl) # To check the maximum by hand. This will ease computation
# for the following double-loop.

y2[,3] <- 220000 # The column now is all maximums

for (i in 1:nrow(y2)) {
  for (j in 1:nrow(rper12)) {
    if(y2[i,1] == rper12$nquest[j]) {
      if(y2[i,3] > rper12$yl[j]) {
        y2[i,3] <- rper12$yl[j]
      }
    }
  }
}

ytemp2 <- y2 # Keeping y intact

ytemp2 <- cbind(y2,ricfam12$af, rfam12$ycf, rfam12$yl)

colnames(ytemp2) <- c("ID", "MinIncome", "MaxIncome", "Financial Assets",
                      "FinancialIncome", "Disposable Income") 

ytemp2 <- ytemp2[ytemp2[,3] != 0,] # Dropping non-zero households
summary(ytemp2) # Indeed, we don't have any zero-income households
nrow(ytemp2) # There are 2246 such households

ypca<- rbind(ytemp, ytemp2) # This will be the dataset to use for pca
summary(ypca) # Summary statistics
nrow(ypca)

# In line with Andini et al, we will train our model with a random sample 
# constituting two-thirds of our sample, and test it on the remaining third. 

# First, let's create a dataset of probabilities
# Not to be run again, please use the variable rvar provided in the
# second column of the test_indicators file. rvar is defined in the
# first part of the code.

#set.seed(10000)
#rvar <- rdunif(nrow(ypca), 3,1)
#rvar
#length(rvar)
 
#replicability_rvar <- write.csv(rvar, "train_indicator.csv")

#for(i in 1:length(rvar)) {
#  if(rvar[i] != 3) {
#    rvar[i] <- 1
#  } else {
#    rvar[i] <- 0
#  }
#}

ypca.train <- ypca[rvar!= 1,]

oldpca <- prcomp(ypca.train[,-1], center = TRUE,scale. = TRUE) # With centering and scaling
oldpca # Factor loadings
summary(oldpca) # Variances
xtable(oldpca) # LaTeX table

# Now we can use the first component to build the index for the remaining third

ypca.test <- ypca[rvar==1,]
indextemp <- -0.5628856*ypca[,2] - 0.1474835*ypca[,3] - 0.4861395*ypca[,4] - 0.3552653*ypca[,5] - 0.5466840*ypca[,6]

summary(indextemp) # Summary statistics for the non-indexed mass                                                                              
indexnum <- max(indextemp)-indextemp # Numerator of the index
indexden <- max(indextemp)-min(indextemp) # Denominator of the index 
index <- 100*indexnum/indexden # The division yields the index (one-scale)
summary(index) # Finally we have the index (hundred-scale)

# Building upon the index, we can define being "pca-constrained"

pca.constrained <- c()
for(i in 1:length(index)) {
  if(index[i] < 3.1) { #Index lower than average := "pca-constrained"
    pca.constrained[i] <- 1
  } else {
    pca.constrained[i] <- 0
  }
}
summary(pca.constrained) # Summary statistics
length(index) # Number of households

# Let's examine the distribution of the index

ggplot(as.data.frame(index), aes(x=index)) +
  geom_histogram(binwidth=10) +
  labs(title = "Historgram of the Economic Well-Being Index",
       subtitle = "for the test sample of the 2012 and 2014 Surveys",
       x = "Index",
       y = "Frequency")

ggplot(as.data.frame(index[index<10]), aes(x=index[index<10])) +
  geom_histogram(binwidth=1) +
  labs(title = "Historgram of the Economic Well-Being Index",
       subtitle = "for index values lower than 10",
       x = "Index",
       y = "Frequency")

# Now, we will compare the number of individuals that are deemed to be 
# constrained by Andini et al, and by our index

# Recall that Andini et al. deemed the following households to be constrained:

# those with yearly financial assets over 13 255 AND with yearly
# disposable income under 52 691 AND with yearly financial income
# under 432.93 AND with minimum labor income under 13 895

# OR

# those with annual financial assets under 13 255 AND yearly 
# disposable income under 36 040

# OR

# those with financial assets under 13 255 AND yearly disposable
# income over 36 040 AND maximum labor income under 34 500.

# We will create a new indicator called AndiniConstrained to
# indicate such people.

AndiniConstrained <- 0

# Column names are:  c("ID", "MinIncome", "MaxIncome", "Financial Assets",
#                      "FinancialIncome", "Disposable Income") 

for (i in 1:nrow(ypca)) {
  if(ypca[i,4] > 13254 && ypca[i,6] < 52691 && ypca[i,5] < 432.93 && ypca[i,2] <13895) {
    AndiniConstrained[i] <- 1
  } else if(ypca[i,4] < 13255 && ypca[i,6] < 36040) {
    AndiniConstrained[i] <- 1
  } else if(ypca[i,4] < 13255 && ypca[i,6] > 36039 && ypca[i,3] <34500) {
    AndiniConstrained[i] <- 1
  } else AndiniConstrained[i] <- 0
}

summary(AndiniConstrained)

# Creating the cross-tabulation matrix

crosstab <- matrix(0, nrow = 2, ncol = 2)

crosstab

for (i in length(pca.constrained)) {
  if(pca.constrained[i] == 1 && AndiniConstrained[i] == 1) {
    crosstab[1,1] <- crosstab[1,1] + 1
  } else if(pca.constrained[i] == 1 && AndiniConstrained[i] == 0) {
    crosstab[2,1] <- crosstab[2,1] + 1
  } else if(pca.constrained[i] == 0 && AndiniConstrained[i] == 1) {
    crosstab[1,2] <- crosstab[1,2] + 1
  } else crosstab[2,2] <- crosstab[2,2] + 1
  
}

ctab <- cbind(AndiniConstrained, pca.constrained) # For the contingency table
table(as.data.frame(ctab)) # 3800 observations match whereas 651 do not.


xtable(table(as.data.frame(ctab))) # LaTeX code

summary(index)# Now, we will compare average economic variables for the two indicators

olddatawithindicators <- cbind(ypca, AndiniConstrained, pca.constrained)

data.AndiniConstrained <-olddatawithindicators[AndiniConstrained == 1,]
data.pca.constrained <- olddatawithindicators[pca.constrained == 1,]
summary(data.AndiniConstrained)
summary(data.pca.constrained)

sum(AndiniConstrained) #3113 households are predicted to be constrained
sum(pca.constrained) #3634 households are predicted to be constrained

# Repeating the same process for the 2014 sample

sum(rfam14$nquest != ricfam14$nquest)
sum(risfam14$nquest != ricfam14$nquest)

y14 <- matrix(0, nrow = nrow(rfam14), ncol = 3)
y14[,1] <- rfam14$nquest

summary(rper14$yl)

#Gathering maximum incomes for the 2014 sample

y14[,2] <- 0 # The column now is all minimums

for (i in 1:nrow(y14)) {
  for (j in 1:nrow(rper14)) {
    if(y14[i,1] == rper14$nquest[j]) {
      if(y14[i,2] < rper14$yl[j]) {
        y14[i,2] <- rper14$yl[j]
      }
    }
  }
}

#Gathering minimum incomes for the 2014 sample

y14[,3] <- 135000 # The column now is all maximums

for (i in 1:nrow(y14)) {
  for (j in 1:nrow(rper14)) {
    if(y14[i,1] == rper14$nquest[j]) {
      if(y14[i,3] > rper14$yl[j]) {
        y14[i,3] <- rper14$yl[j]
      }
    }
  }
}

y14temp <- y14 # Keeping y intact

y14temp <- cbind(y14,ricfam14$af, rfam14$ycf, rfam14$yl)

colnames(y14temp) <- c("ID", "MinIncome", "MaxIncome", "Financial Assets",
                     "FinancialIncome", "Disposable Income") 

ypca14 <- y14temp[y14temp[,3] != 0,] # Dropping non-zero households
summary(ypca14) # Indeed, we don't have any zero-income households
nrow(ypca14) # There are 2054 such households

#Estimating the index in the 2014 sample using the loadings estimated in the 
#2010 - 2012 sample

indextemp14 <- -0.5628856*ypca14[,2] - 0.1474835*ypca14[,3] - 0.4861395*ypca14[,4] - 0.3552653*ypca14[,5] - 0.5466840*ypca14[,6]

#We construct the index employing the exact same process as above.

summary(indextemp14)                                                                          
indexnum14 <- max(indextemp14)-indextemp14
indexden14 <- max(indextemp14)-min(indextemp14)
index14 <- 100*indexnum14/indexden14 
summary(index14) 

#Based on our model, we distinguish again between constrained and non-constrained
#households

pca14.constrained <- c()
for(i in 1:length(index14)) {
  if(index14[i] < 3.1) {
    pca14.constrained[i] <- 1
  } else {
    pca14.constrained[i] <- 0
  }
}
summary(pca14.constrained) # Summary statistics
length(index14) # Number of households

#Distribution of index14

#Determining Andini Constrained Households in the 2014 sample

AndiniConstrained14 <- 0

# Column names are:  c("ID", "MinIncome", "MaxIncome", "Financial Assets",
#                      "FinancialIncome", "Disposable Income") 

for (i in 1:nrow(ypca14)) {
  if(ypca14[i,4] > 13254 && ypca14[i,6] < 52691 && ypca14[i,5] < 432.93 && ypca14[i,2] <13895) {
    AndiniConstrained14[i] <- 1
  } else if(ypca14[i,4] < 13255 && ypca14[i,6] < 36040) {
    AndiniConstrained14[i] <- 1
  } else if(ypca14[i,4] < 13255 && ypca14[i,6] > 36039 && ypca14[i,3] <34500) {
    AndiniConstrained14[i] <- 1
  } else AndiniConstrained14[i] <- 0
}

summary(AndiniConstrained14)

#Matching matrix

crosstab14 <- matrix(0, nrow = 2, ncol = 2)

crosstab14

for (i in length(pca14.constrained)) {
  if(pca14.constrained[i] == 1 && AndiniConstrained14[i] == 1) {
    crosstab14[1,1] <- crosstab14[1,1] + 1
  } else if(pca14.constrained[i] == 1 && AndiniConstrained14[i] == 0) {
    crosstab14[2,1] <- crosstab14[2,1] + 1
  } else if(pca.constrained14[i] == 0 && AndiniConstrained14[i] == 1) {
    crosstab14[1,2] <- crosstab14[1,2] + 1
  } else crosstab14[2,2] <- crosstab14[2,2] + 1
  
}


ctab14 <- cbind(AndiniConstrained14, pca14.constrained)
table(as.data.frame(ctab14)) # 1817 observations match whereas 237 do not.

xtable(table(as.data.frame(ctab))) # LaTeX code

# Now, we will compare average economic variables for the two indicators

olddatawithindicators14 <- cbind(ypca14, AndiniConstrained14, pca14.constrained)

#Characteristics of households predicted to be consumption constrained by our 
#model vs. Andini et al. model

data.AndiniConstrained14 <-olddatawithindicators14[AndiniConstrained14 == 1,]
data.pca14.constrained <- olddatawithindicators14[pca14.constrained == 1,]
summary(data.AndiniConstrained14)
summary(data.pca14.constrained)

sum(AndiniConstrained14) 
sum(pca14.constrained) 

#Characteristics of households predicted not to be consumption constrained by our 
#model vs. Andini et al. model

data.AndininonConstrained14 <-olddatawithindicators14[AndiniConstrained14 == 0,]
data.pca14.nonconstrained <- olddatawithindicators14[pca14.constrained == 0,]
summary(data.AndininonConstrained14)
summary(data.pca14.nonconstrained)

