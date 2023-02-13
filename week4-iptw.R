library(tableone)
library(ipw)
library(sandwich) # for robust variance estimation
library(survey)
library(MatchIt)

# read in data
data(lalonde)

# create my version of data
mydata <- lalonde %>% 
  mutate(black = as.numeric(race=="black"),
         hispan = as.numeric(race=="hispan")) %>% 
  select(-race)

# view mydata
str(mydata)

# print column names of mydata
dput(names(mydata))


###################### Question 1 ######################
#
####### What are the minimum and maximum weights? ######   
#
######################################################## 

# propensity score model
psmodel <- glm(treat ~ age+educ+black+hispan+married+nodegree+re74+re75,
               family=binomial(),data=mydata)

## value of propensity score for each subject
ps <-predict(psmodel, type = "response")

#create weights
weight<-ifelse(mydata$treat==1, 1/(ps), 1/(1-ps))
summary(weight)

###################### Question 2 ######################
# Find the standardized differences for each confounder 
# on the weighted (pseudo) population. What is the standardized
# difference for nodegree?
######################################################## 

# apply weights to data
weighteddata<-svydesign(ids = ~ 1, data = mydata, weights = ~ weight)

# weighted table 1
xvars <- c("age", "educ", "married", "nodegree", "re74", "re75", 
           "re78", "black", "hispan")
weightedtable <-svyCreateTableOne(vars = xvars, strata = "treat", 
                                  data = weighteddata, test = FALSE)
## Show table with SMD
print(weightedtable, smd = TRUE)

###################### Question 3 ######################
# Using IPTW, find the estimate and 95% confidence
# interval for the average causal effect. This can be 
# obtained from svyglm
######################################################## 
#fit a marginal structural model (risk difference)
msm <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = ~weight,
                                                    data = mydata)))
coef(msm)
confint(msm)

###################### Question 4 ######################
# Now truncate the weights at the 1st and 99th
# percentiles. This can be done with the trunc=0.01 option in svyglm. 
# Using IPTW with the truncated weights, find the
# estimate and 95% confidence interval for the average causal effect 
######################################################## 

# fit propensity score model to get weights, but truncated
weightmodel<-ipwpoint(exposure= treat, family = "binomial", link ="logit",
                      denominator= ~ age + educ + married + re74+re75+ black + hispan, 
                      data=mydata, trunc=.01)

#numeric summary of weights
summary(weightmodel$weights.trun)

#plot of weights
ipwplot(weights = weightmodel$weights.trun, logscale = FALSE,
        main = "weights", xlim = c(0, 22))

mydata$wt<-weightmodel$weights.trun
#fit a marginal structural model (risk difference)
msm <- (svyglm(re78 ~ treat, design = svydesign(~ 1, weights = weightmodel$weights.trun,
                                                    data =mydata)))
coef(msm)
confint(msm)


