# PROBLEM_SET_2_ASSIGNMENT_RCODE ## deadline 15_10_2023
###
#######  Applied Statistical Analysis I      
###### Assignment: Problem set 2
##### Applied Statistics/ Quantitative Methods 1
###
###### Student Name: Idi Amin Da Silva
###### Student Number: 233 722 25
##################################################
## QUESTION 1:
####
#(1a) Calculate the chi-squared test statistics by hand/manually (even better if you can do "by hand in R)
####
### Answer of Question (1a):
###
# Formula: chi-squared <- sum((f_observed - f_expected)^2/(f_expected)) # I will write this formula in R 
# and creating vector for f_observed and f_expected.
#####
# Note: Select all and run
####
getwd()
setwd("C:/NewGithubFolder TCD/StatsI_Fall2023")
getwd()

f_observed <- c(14, 6, 7, 7, 7, 1)
f_expected <- c(13.50, 8.36, 5.14, 7.50, 4.64, 2.86)
chi_squared <- sum((f_observed - f_expected)^2/(f_expected))
round(chi_squared, digits=2) # Answer: chi_squared = 3.80
########################################################################
###########################
# ## Extra calculations/works:
# Create a the given table in problem set2 Question 1 using R code

traffic_light_corruption_vector <- as.table(rbind(c(14, 6, 7), c(7, 7, 1))) # table values in Rows and stored in a R object called traffic_light_corruption_vector 
rownames(traffic_light_corruption_vector) <- c("Upper Class", "Lower Class") # labeling the rows
colnames(traffic_light_corruption_vector) <- c("Not Stopped", "Bride Requested", "Stopped/Given Warning") # Labeling the columns
traffic_light_corruption_vector # Display the table
###### Answer:
#             Not Stopped     Bride Requested     Stopped/Given Warning
#Upper Class          14               6                     7

#Lower Class           7               7                     1
##################
Chisq_test_Statistic <- chisq.test(traffic_light_corruption_vector) # perform the chi-squared test
Chisq_test_Statistic
####
#### Answer:
# 	Pearson's Chi-squared test

#data:  traffic_light_corruption_vector
#X-squared = 3.7912, df = 2, p-value = 0.1502 # The chi-squared test statistics is the same as calculated
#############                                 # manually chi-sq=3.80, p-value=0.1502
###################################################################
#(i) To recall the observed frequencies (fo):

Chisq_test_Statistic$observed
### Answer:
#               Not Stopped     Bride Requested       Stopped/Given Warning
#Upper Class          14               6                     7
#Lower Class           7               7                     1
############
## (ii) To Calculate the expected frequencies (fe)

Chisq_test_Statistic$expected
## Answer:
#               Not Stopped      Bride Requested     Stopped/Given Warning
# Upper Class        13.5           8.357143              5.142857
# Lower Class         7.5           4.642857              2.857143
###############
# (iii) Calculation of pearson residual: sum((fo - fe)^2/sqrt(fe))

Chisq_test_Statistic$residuals
###
#            Not Stopped   Bride Requested     Stopped/Given Warning
#Upper Class   0.1360828      -0.8153742             0.8189230
#Lower Class  -0.1825742       1.0939393            -1.0987005
#################

### Bar Plot - Using base R 

color_names <- c("blue", "red", "green")
barplot(officers_bribe, beside=T, xlab="Class Categories", ylab="Decisions", main="Latin America Traffic Light Corruption", col=color_names)
legend(1, 2300, rownames(officers_bribe), cex=0.7, fill=color_names, bty="n")

########
############################################
######
#### Question (1b)
####
# Now calculate the p-value from the test statistic you just created (in R). What do you conclude if
## alpha = 0.1
################ Answer of Question (1b) 
###
# Formula of calculating p-value: p-value <- pchisq(chi_squared, df=(rows-1)*(columns-1), lower.tail=FALSE)
# chi_squared=3.80, df=2
###
p_value <-pchisq(3.7912, df=2, lower.tail=FALSE)
p_value # Answer: p_value = 0.1502282 
########################
### 2nd method: Using matrix notation:
#####
traffic_light_corruption <- c(14, 6, 7, 7, 7, 1) # Create a vector by introducing the element in row
officers_bribe <- matrix (traffic_light_corruption, nrow=2, byrow=TRUE) # Create a matrix with 2 rows and 3 columns
rownames(officers_bribe) <- c("Upper Class", "Lower Class")
colnames(officers_bribe) <- c("Not Stopped", "Bribe Requested", "Stopped/Given Warning")
officers_bribe
################ Answer/output:
#               Not Stopped     Bribe Requested       Stopped/Given Warning
#Upper Class          14               6                     7
#Lower Class           7               7                     1
################
# Now I am going to perform the chi-squared test using R : to extract the value the chi-squared test 
#statistics, degree of freedom and p-value

My_chi_squared_test <-chisq.test(officers_bribe)
My_chi_squared_test 
############ Answer/Output:
#	Pearson's Chi-squared test
#
# data:  officers_bribe
# X-squared = 3.7912, df = 2, p-value = 0.1502
####
# Formulating the Hypothesis testing:
#
# H0: Upper Class = Lower Class ( Two variable are statistically independent)
# Ha: Upper Class is different from Lower Class (Two Variables are statistically dependent)
##############
## Now I am interested to calculate the critical value of the chi_squared with alpha=0.1 (significance level)
# and degree of freedom (df=2) i.e., qchisqr(alpha=0.1, df=2, lower.tail=FALSE)

qchisq(0.1, df=2, lower.tail=FALSE) # Answer: 4.60517

## Conclusions:
# (a) Since the  (p-value = 0.1502) > (alpha=0.1), so there is no sufficient evidence to reject the 
# the null hypothesis (i.e., I reject the alternative hypothesis in favor of the Null Hypothesis).
# (b) Since 
#(chi-squared statistics = 3.7912) < (critical_value of chi-squared statistic with alpha=0.1 and df=2)= 4.60517
## we can reject the alternative hypothesis since the value of chi-squared statistic fell in the region of
# no-rejection (i.e., its value did not exceed the critical value of chi-squared statistic).
##########################################################################################
############
# QUESTION (1c) Calculate the standardized residuals for each cell and put them in the table below:

##### ANSWER:
# Note: I have calculated them  manually and it will be included in my report in form of the table
###
# To calculate the standardized residual for each table = (fo - fe)/sqrt(fe*(1- rowprop)(1 - colprop))
##
My_chi_squared_test$stdres
## Answer/ Output:

#           Not Stopped     Bribe Requested     Stopped/Given Warning
#Upper Class   0.3220306       -1.641957              1.523026
#Lower Class  -0.3220306        1.641957             -1.523026
################################################
###
# QUESTION (1d): How might the standardized residual help you interpret the results?
####
# ANSWER OF THE QUESTION (1d)
##
# After running the R code in the question (1c) I have found that the rows cells for upper class and lower class
# are symmetric for each columns cells and the sum of each  columns cells is equal to zero then I can state that 
# the standardized residual show that the Null hypothesis true and the observed frequencies e close to the expected
#frequencies.
############
#####
# Note: The value of the chi_squared = 3.80 is exactly the same as calculated manually
# QUESTION 2:
####
library(readr)
women <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
setwd("C:\\NewGithubFolder TCD\\StatsI_Fall2023")
View(women)
dim(women) # observations/rows=322; variables/columns=6
names(women) #"GP"         "village"    "reserved"   "female"     "irrigation" "water"   
## 
head(women)
##
class(women)
#
#str(women)
getwd()
####
####
# (2a)

# Ho: mu=0 (The reservation policy has no effect on the  number of new or repair water facilities in the village)

# Ha: mu different from zero (The reservation policy has effect on the  number of new or repair water facilities in the village)

###
# (2b)

fitted_model_women <- lm(water ~ reserved, data=women)
summary(fitted_model_women)
### Answer:
#Call:
# lm(formula = water ~ reserved, data = women)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-23.991 -14.738  -7.865   2.262 316.009 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   14.738      2.286   6.446 4.22e-10 ***
#  reserved       9.252      3.948   2.344   0.0197 *  
#  ---


#Residual standard error: 33.45 on 320 degrees of freedom
#Multiple R-squared:  0.01688,	Adjusted R-squared:  0.0138 
#F-statistic: 5.493 on 1 and 320 DF,  p-value: 0.0197
#######
# The predicted line or the best fitting line or the regression line is given by:

# water_i= 14.738 +  9.252* reserved_1; Note that the reserved_i is a binary variable/dummy variable can can take two values 0 
# and 1.
#############
# Graph of Residuals Vs Fitted , Question (2b)
plot(fitted_model_women)
####
# (2c)
# The predicted best fitting regression line is given by water_i= 14.738 +  9.252* reserved_1, the intercept water_i=14.738 
# represent the average value when reserved_i=0.
# For each increase in one unit in reserved_i=1 , the value of water_i is expected to increase by average of 9.252.
################

