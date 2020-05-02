# #======================================================================= 
# #   ADVANCE STATISTICS  - Factor Hair 
# #=======================================================================

# Setting up working directory
setwd("/home/sb/Desktop/Workspace-test/3. Advanced Statistics/Project 2/")
getwd()

# Read the dataset and save it to mydata_hair
mydata_hair = read.csv("Factor-Hair-Revised.csv", header = TRUE)

# Check for NA values
sum(is.na(mydata_hair))
colSums(is.na(mydata_hair))
#Inference: No missing values present in the given dataset

# Explore the data
head(mydata_hair)
tail(mydata_hair)
summary (mydata_hair)
str(mydata_hair)
View(mydata_hair)

# Remove the column ID from the dataset
mydata_hair = mydata_hair[,-1]
summary (mydata_hair)

# Check for Outliers
library(ggplot2)
boxplot(mydata_hair)

boxplot(mydata_hair$ProdQual)
ggplot(mydata_hair, aes(x = mydata_hair$ProdQual)) + geom_histogram(aes(y = ..density..)) + geom_density()

boxplot(mydata_hair$Ecom, main="Ecom", col = "Red", boxwex=0.1)
outlier_values = boxplot.stats(mydata_hair$Ecom)$out #outliers_value
mtext(paste("Outliers: ", paste(outlier_values, collapse= ",  ")), cex=0.6)
ggplot(mydata_hair, aes(x = mydata_hair$Ecom)) + geom_histogram(aes(y = ..density..)) + geom_density()

boxplot(mydata_hair$TechSup)
ggplot(mydata_hair, aes(x = mydata_hair$TechSup)) + geom_histogram(aes(y = ..density..)) + geom_density()

boxplot(mydata_hair$CompRes)
ggplot(mydata_hair, aes(x = mydata_hair$CompRes)) + geom_histogram(aes(y = ..density..)) + geom_density()

boxplot(mydata_hair$Advertising)
ggplot(mydata_hair, aes(x = mydata_hair$Advertising)) + geom_histogram(aes(y = ..density..)) + geom_density()

boxplot(mydata_hair$ProdLine)
ggplot(mydata_hair, aes(x = mydata_hair$ProdLine)) + geom_histogram(aes(y = ..density..)) + geom_density()

boxplot(mydata_hair$SalesFImage, main = "SalesFImage", col = "Red", boxwex=0.1)
outlier_values = boxplot.stats(mydata_hair$SalesFImage)$out #outliers_value
mtext(paste("Outliers: ", paste(outlier_values, collapse= ",  ")), cex=0.6)
ggplot(mydata_hair, aes(x = mydata_hair$SalesFImage)) + geom_histogram(aes(y = ..density..)) + geom_density()

boxplot(mydata_hair$ComPricing)
ggplot(mydata_hair, aes(x = mydata_hair$ComPricing)) + geom_histogram(aes(y = ..density..)) + geom_density()

boxplot(mydata_hair$WartyClaim)
ggplot(mydata_hair, aes(x = mydata_hair$WartyClaim)) + geom_histogram(aes(y = ..density..)) + geom_density()

boxplot(mydata_hair$OrdBilling, main = "OrdBilling", col = "Red", boxwex = 0.1)
outlier_values = boxplot.stats(mydata_hair$OrdBilling)$out #outliers_value
mtext(paste("Outliers: ", paste(outlier_values, collapse= ",  ")), cex=0.6)
ggplot(mydata_hair, aes(x = mydata_hair$OrdBilling)) + geom_histogram(aes(y = ..density..)) + geom_density()


boxplot(mydata_hair$DelSpeed, main = "DelSpeed", col = "Red", boxwex = 0.1)
outlier_values = boxplot.stats(mydata_hair$DelSpeed)$out #outliers_value
mtext(paste("Outliers: ", paste(outlier_values, collapse= ",  ")), cex=0.6)
ggplot(mydata_hair, aes(x = mydata_hair$DelSpeed)) + geom_histogram(aes(y = ..density..)) + geom_density()

boxplot(mydata_hair$Satisfaction)
ggplot(mydata_hair, aes(x = mydata_hair$Satisfaction)) + geom_histogram(aes(y = ..density..)) + geom_density()
# Inference: Presence of outliers detected in Ecom, SalesFImage, OrdBilling & DelSpeed. 
# However, since the outlier values are within permissible range, i.e. within scale 1 to 10, outlier treatment is not required. 

# Check for multi-colinearity
cor(mydata_hair[1:12])
library(corrplot)
corrplot(cor(mydata_hair[1:12]), type="upper", method= "number")
# Inference: Correlation is seen between the Independent Variables and the dependent variable (Satisgaction), which is desired.
# Eg: Moderate correlation is seen between ProdQual & Satisfaction (0.49). Moderate correlation is seen between CompRes & Satisfaction (0.6), 
# Similarly, moderate correlation is seen between ProdLine, SalesFImage, OrdeBilling, DelSpeed with respect to Satisfaction (i.e, 0.55, 05., 0.52 & 0.58 respectively). 

# However, in addition to being potentially related to dependent variable, the independent variables seem to be related to each other as well. Hence the presence of multi-collinearity is very much evident, which is not desirable. 
# Eg: High correlation is seen between Ecom & SalesFImage (0.79). High correlation is seen between TechSup & WartyClaim (0.8)
# High correlation is seen between CompRes & OrdBilling (0.76). Very high correlation is seen between CompRes & DelSpeed (0.87)


# Simple Linear Regression
Model_SLM1 = lm (mydata_hair$Satisfaction~mydata_hair$ProdQual, data = mydata_hair)
summary(Model_SLM1) # RSq Value (Coefficient of Determination): 0.2365. It implies that 23.6 % of variations in Satisfaction is explained by the independent variable ProdQual.
ggplot(mydata_hair, aes(x = ProdQual, y = Satisfaction)) + geom_point() + stat_smooth(method="lm", col="red") +  
  ggtitle ("Relationship between ProdQual and Satisfaction") 

Model_SLM2 = lm (mydata_hair$Satisfaction~mydata_hair$Ecom, data = mydata_hair)
summary(Model_SLM2) # RSq Value (Coefficient of Determination): 0.07994. It implies that 7.9 % of variations in Satisfaction is explained by the independent variable Ecom.
ggplot(mydata_hair, aes(x = Ecom, y = Satisfaction)) + geom_point() + stat_smooth(method="lm", col="red") +  
  ggtitle ("Relationship between Ecom and Satisfaction")

Model_SLM3 = lm (mydata_hair$Satisfaction~mydata_hair$TechSup, data = mydata_hair)
summary(Model_SLM3) # RSq Value (Coefficient of Determination): 0.01268. It implies that 1.2 % of variations in Satisfaction is explained by the independent variable TechSup.
ggplot(mydata_hair, aes(x = TechSup, y = Satisfaction)) + geom_point() + stat_smooth(method="lm", col="red") +  
  ggtitle ("Relationship between TechSup and Satisfaction")

Model_SLM4 = lm (mydata_hair$Satisfaction~mydata_hair$CompRes, data = mydata_hair)
summary(Model_SLM4) # RSq Value (Coefficient of Determination): 0.3639. It implies that 36.3 % of variations in Satisfaction is explained by the independent variable CompRes.
ggplot(mydata_hair, aes(x = CompRes, y = Satisfaction)) + geom_point() + stat_smooth(method="lm", col="red") +  
  ggtitle ("Relationship between CompRes and Satisfaction")

Model_SLM5 = lm (mydata_hair$Satisfaction~mydata_hair$Advertising, data = mydata_hair)
summary(Model_SLM5) # RSq Value (Coefficient of Determination): 0.09282. It implies that 9.2 % of variations in Satisfaction is explained by the independent variable Advertising.
ggplot(mydata_hair, aes(x = Advertising, y = Satisfaction)) + geom_point() + stat_smooth(method="lm", col="red") +  
  ggtitle ("Relationship between Advertising and Satisfaction")

Model_SLM6 = lm (mydata_hair$Satisfaction~mydata_hair$ProdLine, data = mydata_hair)
summary(Model_SLM6) # RSq Value (Coefficient of Determination): 0.3031. It implies that 30.3 % of variations in Satisfaction is explained by the independent variable ProdLine.
ggplot(mydata_hair, aes(x = ProdLine, y = Satisfaction)) + geom_point() + stat_smooth(method="lm", col="red") +  
  ggtitle ("Relationship between ProdLine and Satisfaction")

Model_SLM7 = lm (mydata_hair$Satisfaction~mydata_hair$SalesFImage, data = mydata_hair)
summary(Model_SLM7) # RSq Value (Coefficient of Determination): 0.2502. It implies that 25.0 % of variations in Satisfaction is explained by the independent variable SalesFImage.
ggplot(mydata_hair, aes(x = SalesFImage, y = Satisfaction)) + geom_point() + stat_smooth(method="lm", col="red") +  
  ggtitle ("Relationship between SalesFImage and Satisfaction")

Model_SLM8 = lm (mydata_hair$Satisfaction~mydata_hair$ComPricing, data = mydata_hair)
summary(Model_SLM8) # RSq Value (Coefficient of Determination): 0.04339. It implies that 4.3 % of variations in Satisfaction is explained by the independent variable ComPricing.
ggplot(mydata_hair, aes(x = ComPricing, y = Satisfaction)) + geom_point() + stat_smooth(method="lm", col="red") +  
  ggtitle ("Relationship between ComPricing and Satisfaction")

Model_SLM9 = lm (mydata_hair$Satisfaction~mydata_hair$WartyClaim, data = mydata_hair)
summary(Model_SLM9) # RSq Value (Coefficient of Determination): 0.03152. It implies that 3.1 % of variations in Satisfaction is explained by the independent variable WartyClaim.
ggplot(mydata_hair, aes(x = WartyClaim, y = Satisfaction)) + geom_point() + stat_smooth(method="lm", col="red") +  
  ggtitle ("Relationship between WartyClaim and Satisfaction")

Model_SLM10 = lm (mydata_hair$Satisfaction~mydata_hair$OrdBilling, data = mydata_hair)
summary(Model_SLM10) # RSq Value (Coefficient of Determination): 0.2722. It implies that 27.2 % of variations in Satisfaction is explained by the independent variable OrdBilling.
ggplot(mydata_hair, aes(x = OrdBilling, y = Satisfaction)) + geom_point() + stat_smooth(method="lm", col="red") +  
  ggtitle ("Relationship between OrdBilling and Satisfaction")

Model_SLM11 = lm (mydata_hair$Satisfaction~mydata_hair$DelSpeed, data = mydata_hair)
summary(Model_SLM11) # RSq Value (Coefficient of Determination): 0.333. It implies that 33.3 % of variations in Satisfaction is explained by the independent variable DelSpeed.
ggplot(mydata_hair, aes(x = DelSpeed, y = Satisfaction)) + geom_point() + stat_smooth(method="lm", col="red") +  
  ggtitle ("Relationship between DelSpeed and Satisfaction")

# Factor Analysis
# Step1 â Check correlation
mydata_hair_subset = mydata_hair[,-12]
cormat = cor(mydata_hair_subset)
cormat
library(corrplot)
corrplot(cormat, type="upper", method ="number") # Plotting Correlation Matrix graphically

# Step 2 â Bartlett test for sphericity, H0: All dimensions are same
library(psych)
cortest.bartlett(cormat) # pValue = 1.79337e-96, which is less than .05 (alpha), hence proceed with dimension reduction using Factor Analysis

# Step 3 â Check sampling adequacy
KMO(cormat) # Overall MSA = 0.65, which is > 0.5, means we have adequate samples and can proceed with dimension reduction using Factor Analysis

# Step 4 â Eigen values: It provides basis for selecting number of factors 
library(nFactors)
evector = eigen(cormat)
eigen_value = evector$values 
eigen_value # Choosing number of factors for which Eigen value is greater than 1 (Kaiserâs Rule). In this case 4 factors.

# Step 5 â Scree Plot
Factor = c(1,2,3,4,5,6,7,8,9,10,11) # As total number of variables that we want to collapse is 11
Scree = data.frame(Factor, eigen_value)
plot(Scree,main="Scree Plot", col="Blue")
lines(Scree,col="Red")

# Step 6 â Extract FA
fa_mydata = fa(mydata_hair_subset, nfactors=4, rotate="none", fm="pa") # nfactors 4, i.e. number of factors to extract
fa_mydata # As we are unable to interpret any factor in a meaningful manner, hence orthogonal rotation using varimax needs to be done.
fa.diagram(fa_mydata)

fa_mydata = fa(mydata_hair_subset, nfactors=4, rotate="varimax", fm="pa") # It pushes the correlations of the loadings (lower ones to 0 and higher ones to +/-1, keeping the communality (h2) constant)
fa_mydata
# Communality (h2) for ProdQual = 0.42, implies that the ability of all the four factors to capture as much as variance in ProdQual is 42 %.
# Communality (h2) for Ecom = 0.64, implies that the ability of all the four factors to capture as much as variance in Ecom is 64 %.
# Communality (h2) for TechSup = 0.79, implies that the ability of all the four factors to capture as much as variance in TechSup is 79 %.
# Communality (h2) for CompRes = 0.84, implies that the ability of all the four factors to capture as much as variance in CompRes is 84 %.
# Communality (h2) for Advertising = 0.31, implies that the ability of all the four factors to capture as much as variance in Advertising is 31 %.
# Communality (h2) for ProdLine = 0.80, implies that the ability of all the four factors to capture as much as variance in ProdLine is 80 %.
# Communality (h2) for SalesFImage = 0.98, implies that the ability of all the four factors to capture as much as variance in SalesFImage is 98 %.
# Communality (h2) for ComPricing = 0.44, implies that the ability of all the four factors to capture as much as variance in ComPricing is 44 %.
# Communality (h2) for WartyClaim = 0.81, implies that the ability of all the four factors to capture as much as variance in WartyClaim is 81 %.
# Communality (h2) for OrdBilling = 0.62, implies that the ability of all the four factors to capture as much as variance in OrdBilling is 62 %.
# Communality (h2) for DelSpeed = 0.94, implies that the ability of all the four factors to capture as much as variance in DelSpeed is 94 %.

# Cumulative Variance (PA1): 0.24, implies that first factor explains 24 % of the variation
# Cumulative Variance (PA2): 0.42, implies that first & second factor explain 42 % of the variation
# Cumulative Variance (PA3): 0.57, implies that first, second & third factor explain 57 % of the variation
# Cumulative Variance (PA4): 0.69, implies that all four factors explain 69 % of the variation

fa.diagram(fa_mydata)
fa_mydata$scores # Gives the factor scores for all 100 observations in the given dataset

# Inference: 
# Delivery Speed, Complaint Resolution and Order & Billing collapse to PA1. The latent variable here could be termed as Sales.
# Salesforce Image, E-Commerce and Advertising collapse to PA2. The latent variable here could be termed as Marketing.
# Warranty & Claims and Technical Support collapse to PA3. The latent variable here could be termed as Service.
# Product Line, Product Quality and Competitive Pricing collapse to PA4. The latent variable here could be termed as Product.

# Multiple Linear Regression
newdata = cbind(mydata_hair[,12], fa_mydata$scores) # Creating new dataset with the factor scores and the dependent variable
newdata = as.data.frame(newdata) # Now we can use the dataset for liner regression
colnames(newdata) = c("Satisfaction", "Sales", "Marketing", "Service", "Product") # Correcting the column names for the data set
names(newdata)

# Checking correlation between the variables
cor(newdata)
corrplot(cor(newdata[1:5]), type="upper", method= "number")                                             

# Splitting the dataset into Train & Test data
library(caTools)
set.seed(123) # any random number
spl = sample.split(newdata$Satisfaction, SplitRatio = 0.7)
Train = subset(newdata, spl==T)
Test = subset(newdata, spl==F)
dim(Train)
dim(Test)

# Creating Multiple Linear Regression Model using the Train data and all the Independent Variables
Model_MLM = lm(Satisfaction~., data = Train) # It will create regression model for all the independent variables in the dataset subset_newdata
summary(Model_MLM) # Overall pValue = 2.2e-16, which is less than alpha (0.05), H0 (i.e. All Betas are zero) is rejected. 
# All individual coefficients, except for that of Service are highly significant as evident by the t-stats that have extremely low p-value. Each one of them is much less than alpha 5%. 
# Adjusted RSq value: 0.7271 , implies that 72.71 percent of variations in Satisfaction is explained by the three independent variables Sales, Marketing and Product. 
# Since the pValue of Service is 0.828, which is greater than alpha, that means it will not impact the dependent variable Satisfaction.

# Check for multi-collinearity using VIF: variance inflation factors (If vif for any variable is >4, it ascertains presence of multi-collinearity)
library(car)
vif(Model_MLM) # Based on the VIF values, we can say that we dont have multi-collinearity in the model.

# Creating Multiple Linear Regression Model using the Train data, by dropping variable Service
# Dropping Service will not impact the model much as its coefficient is almost zero. However, for future models, we may stop collecting data for that variable.
Model_MLM2 = lm(Satisfaction~Sales + Marketing + Product, data = Train) 
summary(Model_MLM2) # Overall pValue = 2.2e-16, which is less than alpha (0.05), it implies that the variables are highly significant. H0 (i.e. All Betas are zero) is rejected. 
# All individual coefficients are highly significant as evident by the t-stats that have extremely low p-value. Each one of them is much less than alpha 5%. 
# Adjusted RSq value: 0.731, implies that 73.1 percent of variations in Satisfaction is explained by the three independent variables Sales, Marketing and Product (when Service is dropped from the Model). 
# Since Model_MLM2 has higher Adjusted R-squared value than that of Model_MM1, it is a better model.
# Overall there is overwhelming evidence that Regression Model exists; meaning the linear model of Satisfaction depending upon the three independent variables Sales, Marketing and Product is robust and statistically valid. 

# Check for multi-collinearity using VIF: variance inflation factors (If vif for any variable is >4, it ascertains presence of multi-collinearity)
library(car)
vif(Model_MLM2) # Based on the VIF values, we can say that we dont have multi-collinearity in the model.

# Visualize the Model result
plot(Model_MLM2)

# Backtracking ability of the model
Prediction_Y  = predict(Model_MLM2)
plot(Train$Satisfaction, col = "blue", type = "b")
lines(Prediction_Y, col="red")

# Compute R-sq for the test data
predTest = predict(Model_MLM2, newdata = Test)
# Compute SST
SST = sum((Test$Satisfaction - mean(Train$Satisfaction))^2)
SST
# Compute SSE
SSE = sum((predTest - Test$Satisfaction)^2)
SSE
# Compute SSR
SSR = sum((predTest - mean(Train$Satisfaction))^2)
SSR
# R-Sq for the test data
SSR/SST # Rsq: 0.4696859, fraction of variation explained by the model
# Inference: The accuracy on the train set is high, but it drops when it comes to Test set. 
# Thus we can say that the Model is Over-fitting, 
# i.e. Model performance is good for the data it has seen, but not so much for the data it has not seen.

# #======================================================================= 
# #   				THE - END 
# #=======================================================================

