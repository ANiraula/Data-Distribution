#Download Quarterly GDP by State from BEA
#==Release: https://bea.gov/newsreleases/regional/gdp_state/qgdpstate_newsrelease.htm
#==Tables only: https://bea.gov/newsreleases/regional/gdp_state/2018/xls/qgdpstate0518.xlsx
#Packages required: XLConnect, fitdistrplus

#Extracting downloaded and slightly pre-processed BEA data on GDP by state (2017, Q4)
GDPSt = readWorksheetFromFile("/Users/anilniraula/Downloads/qgdpstate0518.xlsx", header = FALSE,
                              sheet = 4, startRow = 4, startCol = 1, endCol = 17, endRow = 65)
#Removing regional totals
#("New England", "Midwest", "Great Lakes", "Plains", 
#"Southeast", "Southwest", "Rocky Mountain", and "Far West")
StateGDP <-as.data.frame(as.numeric(GDPSt[-c(1:4, 11, 18, 24, 32, 45, 50, 56),9])/1000) #convert GDP to $Billions
colnames(StateGDP) <- c("GDP by State")
rownames(StateGDP) <- as.list(GDPSt[-c(1:4, 11, 18, 24, 32, 45, 50, 56),1])
View(StateGDP)
#Number of non-zero values/observations
n = sum(!is.na(StateGDP))
summary(StateGDP)
#GDP by State    
#Min.   :  32.59  
#1st Qu.:  94.22  
#Median : 222.22  
#Mean   : 384.77  
#3rd Qu.: 515.00  
#Max.   :2802.29 
#Considerable difference between mean and median
#can show that the data might not be normally distributed

#To plot the data we need to convert it back to "numeric" class
GSP <- as.numeric(GDPSt[-c(1:4, 11, 18, 24, 32, 45, 50, 56),9])/1000
#View(GSP)
#Plot the data, CDF, and density 
#(historgams work best for simple visualization of underlying distribution)
hist(GSP)
#Plot CDF (i.e. cumulative density function for continuous variables)
plot(ecdf(GSP))
#Density is another relevant way to show where data points are concentrated
plot(density(GSP))
abline(v = mean(GSP), col = "green") #drawing a vertical line for the average GDP
abline(v = median(GSP), col = "blue") #drawing a vertical line for median GDP
abline(v = mean(GSP)+sd(GSP), col = "lightblue") #drawing a vertical line that is 1 standard deviation from the average GDP
abline(v = mean(GSP)-sd(GSP), col = "orange")

#Creating a binary vector of states with GDP +- one standerd deviation
St.dev <- ifelse(GSP>=mean(GSP)-sd(GSP) & GSP<=mean(GSP)+sd(GSP),1,0)
table(St.dev)# 47 out of 51 states are within this range
sum(St.dev)/length(St.dev) # 0.9215686 or 92%. Thus, 8% or 4 states are outside this range
#Chebyshev's Theorem = 1−1/k^2
1-1/3^2# At least 89% of all not normally distributed data should be with 3 sd of the mean
#Boxplot is another handy tool to spot outliers, it shows 1st, 2nd (median), and 3rd Quartiles
#as well as maximum and minimum

#Outliers could potentially be spotted by looking at data points that are 1.5×IQR (IQR = Q3-Q1)
#away from either the third or the first quartile.
boxplot(GSP) #California, Texas, and New York have the highest GDP (potential outliers), followed by Florida.
Sample1 <- StateGDP[-c(11, 40, 47),1] #Excluding these three states for a prompt visual
plot(ecdf(Sample1))

#Identify kinds of distribution that fits the data (need library "fitdistrplus")
#Run the comparative distribution overview
descdist(GSP, discrete = FALSE)
#Major types of distribution:
#"norm", "lnorm", "pois", "exp", "gamma", "nbinom", "geom", "beta", "unif" and "logis"
#Fit couple of distributions
#Try fit normal distribution
fit.norm <- fitdist(GSP, "norm")
plot(fit.norm)
fit.norm$aic #Akaike information criterion shows relative quality of statistical models for a given dataset
#Try fit lognormal distribution
fit.lognorm <- fitdist(GSP, "lnorm")
plot(fit.lognorm)
fit.lognorm$aic
#Try fit exponential distribution
fit.exp <- fitdist(GSP, "exp")
plot(fit.exp)
fit.exp$aic
#Try fit logistic distribution
fit.logis <- fitdist(GSP, "logis")
plot(fit.logis)
fit.logis$aic

#Thus, our data is best represented by lognormal distribution, folowed by exponential, logistic, and then by normal distributions
#https://en.wikipedia.org/wiki/Log-normal_distribution
