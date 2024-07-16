# Project exercises in Quantitative Finance course

# Joint probabilities of default for 3 stocks, Copulas

library('copula') # Copula functions 
library('rgl') # 3D plotting


StockPrices <- read.table("StockPrices (1).txt",header=TRUE) # Importing stock data
prices <- subset(StockPrices, yyyymmdd >= 20220124 & yyyymmdd <= 20240130)
View(prices)

# Tickers MRO, SNA, NSC
x1<-prices[,'MRO']
x2<-prices[,'SNA']
x3<-prices[, 'NSC']

x1<-diff(log(x1)) # log-transform to log-returns (because we have simple returns)
x2<-diff(log(x2)) # And transform to unit-less values
x3<-diff(log(x3))


n<-dim(prices)[1] # Number of rows in prices data frame

x<-cbind(x1,x2, x3) #cbind() adds two lists as columns next to each other
# Now we have the observed or realized values

u <- pobs(x) # Converting x to pseudo observations u


# Computation of empirical cumulative distribution functions, ECDF
F1<-ecdf(x[,1])
F2<-ecdf(x[,2])
F3<-ecdf(x[,3])

# Fitting Clayton's copula
myfit.clayton<-fitCopula(copula=claytonCopula(param=0.1,dim=3),data=u,method='itau') # dimensions to 3, because 3 x:s
coef(myfit.clayton) # Accessing the Copula estimate

mycopula.clayton <- claytonCopula(param=myfit.clayton@estimate,dim=3) # Using the Copula estimate.


# What is the probability that these stocks yield the following returns?
x1<- -0.047
x2<- -0.029
x3<- -0.015

u0<-matrix(c(F1(x1),F2(x2),F3(x3)),nrow=1)
round(u0, 4) # The values of x:s converted to pseudo-observation equals

p0.clayton<-pCopula(u=u0,copula=mycopula.clayton) # Prob(X1<=x1,X2<=x2) # The value of interest is from the pCopula() function so the value behind p0.clayton in this case
p0.clayton
round(p0.clayton,4) # Probability that all 3 stocks return below the x-values stated.
