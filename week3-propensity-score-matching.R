library(tableone)
library(Matching)
library(MatchIt)
library(tidyverse)

# import data
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

# select confounders
xvars <- c("age", "educ", "black", "hispan", "married", "nodegree", "re74", 
           "re75")

##########################
# look at a table 1 
##########################
table1<- CreateTableOne(vars=xvars,strata="treat", 
                        data=mydata, 
                        test=FALSE,
                        smd=TRUE) 

## include standardized mean difference (SMD) 
print(table1,smd=TRUE)

##########################
#propensity score matching
#########################

#fit a propensity score model. logistic regression

psmodel<-glm(treat~age+educ+black+hispan+married+nodegree+re74+re75,
             family=binomial(),data=mydata)

#show coefficients etc
summary(psmodel)
#create propensity score
pscore<-psmodel$fitted.values
# or pscore <- predict(psmodel, type="response")


# Q3. What are the minimum and maximum values of the estimated propensity score?     
summary(pscore)

# Q4. Now carry out propensity score matching using the Match function. 
set.seed(931139)
# Use options to specify pair matching, without replacement, no caliper. 
# Match on the propensity score itself, not logit of the propensity score.  
# Obtain the standardized differences for the matched data.
psmatch <- Match(Tr=mydata$treat, M=1, X=pscore, replace = FALSE)
matched <- mydata[unlist(psmatch[c("index.treated","index.control")]), ]

# get standardized differences
table1.matched<- CreateTableOne(vars=xvars,strata="treat", 
                        data=matched, 
                        test=FALSE,
                        smd=TRUE) 
print(table1.matched, smd=TRUE)

# Q6, redo matching with caliper=0.1
set.seed(931139)
psmatch <- Match(Tr=mydata$treat, M=1, X=pscore, replace = FALSE,
                 caliper = 0.1)
matched <- mydata[unlist(psmatch[c("index.treated","index.control")]), ]
# get standardized differences
table1.matched<- CreateTableOne(vars=xvars,strata="treat", 
                                data=matched, 
                                test=FALSE,
                                smd=TRUE) 
print(table1.matched, smd=TRUE)

# Q7. Use the matched data set (from propensity score matching with caliper=0.1) 
# to carry out the outcome analysis. 
# For the matched data, what is the mean of real earnings in 1978 
# for treated subjects minus the mean of real earnings in 1978 
# for untreated subjects? 
6151.18-4904.37

# Q8. Carry out a paired t-test for the effect of treatment on earnings. 
# What are the values of the 95% confidence interval?
re78.tr <- matched[matched$treat==1,"re78"]
re78.untr <- matched[matched$treat==0,"re78"]
t.test(x=re78.tr, y=re78.untr,
       paired = TRUE)
