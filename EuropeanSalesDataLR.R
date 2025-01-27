# Get the working directory. If needed, you can set the working directory to another folder.
#getwd()
setwd("C:/Users/IBSS/Desktop/BAU/Dersler/Guz/BDA5002")

# Read the Data files from a directory  
EuropeanSalesData<-read.csv("EuropeanSales.csv",header=T)

#Show attributes  
attributes(EuropeanSalesData)

#Dropped country field for checking correlation. (characteristic value)
EuropeanSalesDropped <- subset(EuropeanSalesData, select = -c(Country))

#install lib for correlation visualization
install.packages("ggplot2")
install.packages("corrplot")
library("corrplot")

#Visualization correlation chart and matrix
cor(EuropeanSalesDropped)
corrplot.mixed(cor(EuropeanSalesDropped), order="hclust", tl.col="black")

#There is a high correlation between Population and Computer sales and
#SalesPerCapita & GDPperHead - EducationSpending
#Checking their plot
plot(EuropeanSalesData$GDPperHead,EuropeanSalesData$SalesPerCapita, ylab="Sales", xlab="GDP")
plot(EuropeanSalesData$EducationSpending,EuropeanSalesData$SalesPerCapita, ylab="Sales", xlab="Education Spending")
#Plot all measure in the dataset
plot(EuropeanSalesDropped)


#LR model for Computer Sales
#Model was created with Population - GDPperHead - SalesPerCapita
#R-Square values are 0,777 and 0,738, high and no much difference. 
#P and t values high, exception is population
ModelComSales1 <- lm(ComputerSales ~ Population + GDPperHead + SalesPerCapita, data=EuropeanSalesData)
summary(ModelComSales1)

#Model attributes and coefficients  
attributes(ModelComSales1)
ModelComSales1$coefficients

#LR2 model for Computer Sales
#Model was created with all variables
#R-Square values are 0,779 and 0,705, high but difference is higher than first model. 
#P and t values high, exception is population. Residual standart is lower than first model which is good point
ModelComSales2 <- lm(ComputerSales ~ UnemploymentRate + EducationSpending + Population + SalesPerCapita + GDPperHead, data=EuropeanSalesData)
summary(ModelComSales2)

#ModelComSales1 is better than other. I will go on with this.

# Change layout to show 4 graphs per page 
layout(matrix(c(1,3,2,4),2,2)) 
plot(ModelComSales1)
#Q-Q and Residuals Leverage shows that #20 and #21 records are outlier...

#------------------------------------------#

#LR model for SalesperCapita
#Model was created with Population - GDPperHead - SalesPerCapita
#R-Square values are 0,621 and 0,495, average and avg difference. 
#P and t values are so high
ModelSalesCap1 <- lm(SalesPerCapita  ~ Population + GDPperHead + ComputerSales, data=EuropeanSalesData)
summary(ModelSalesCap1)

#Model attributes and coefficients  
attributes(ModelComSales1)
ModelComSales1$coefficients

#LR2 model for Computer Sales
#Model was created with all variables
#R-Square values are 0,390 and 0,282, low R-square values, also gap is high. 
#P and t values are so high, Residual error is higher than first model
ModelSalesCap2 <- lm(SalesPerCapita  ~ UnemploymentRate + EducationSpending + Population + ComputerSales + GDPperHead, data=EuropeanSalesData)
summary(ModelSalesCap2)

#ModelSalesCap2 is better than other. I will go on with this.

# Change layout to show 4 graphs per page 
layout(matrix(c(1,3,2,4),2,2)) 
plot(ModelSalesCap1)
#Q-Q and Residuals Leverage shows that #6 and #20 records are outlier...

#As a result of testing models, for the Computer Sales, Model1 performed better and
# for the SalesperCapita, again model1 performed better.
