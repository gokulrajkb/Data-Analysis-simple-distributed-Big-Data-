############################################################################
############################################################################

#From,

#      Gokul Kaisaravalli Bhojraj
#      Id: 80789
#      Business Intelligence

############################################################################
############################################################################

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#1) ##############################################

library(car)
data(Mroz)

#1)a

summary(Mroz)
?Mroz
attach(Mroz)


n<-length(lfp)
Y<-ifelse(lfp=="yes", 1, 0)
X<-cbind(1,age,k5,k618,wc=ifelse(wc=="yes",1,0))


source("C:/Users/Gokul/Downloads/Logit.R")

logit(X,Y,X.names=c("Constant","age","k5","k618","wc"))

reg<-glm(lfp~age+k5+k618+wc,family=binomial("logit"),data=Mroz)
summary(reg)

# age has negative impact on women going to work;
#    So as age increase probability of working decrease.
# K5 has high negative impact on women going to work;
#    So as no of younger kids increase probability of working decrease tremendously.
# k618 has negative impact on women going to work;
#    So as no of older kids increase probability of working decrease but lesser than that with younger children.
# wc has positive impact on women going to work;
#    So as women college attendance increase probability of working increases moderately.


#1)b

data <- data.frame(age = 30, k5 = 1, k618=0,wc = "yes")
pred <- predict(reg, data, type="response")
pred

## 0.6838588 ( 65.98% ) is the predicted probability that Sue works.

#1)c

data <- data.frame(age = 30, k5 = 1, k618=1 ,wc = "yes")
pred <- predict(reg, data, type="response")
pred

# 0.6598694 ( 65.98% ) would be the new predicted probability
# However there is no significant effect from 1 older child.
# as the older child is added the probability to work has some effect not much.


#1)d

data <- data.frame(age = 25, k5 = 1, k618=0,wc = "no")
pred <- predict(reg, data, type="response")
pred

## 0.5734959 is the predicted probability to work for Betty.

#1)e

data <- data.frame(age = 25, k5 = 1, k618=0,wc = "yes")
pred <- predict(reg, data, type="response")
pred

# 0.7522139 is her new predicted probability to work.
# she has a significant increase in probability of working
# when there is college attendence, there is a chance of more probability to work.

#1)f

X<-cbind(1,age,k5,k618,inc,hc,lwg,wc=ifelse(wc=="yes",1,0))

logit(X,Y,X.names=c("Constant","age","k5","k618","wc","inc","hc","lwg"))

reg1<-glm(lfp~age+k5+k618+wc+inc+hc+lwg,family=binomial("logit"),data=Mroz)
summary(reg1)

## With the increase in inc ; the probability of women working decreases tremendously;
# As there exists no necessity of working.


# 2)##########################################

#2)a

X<-cbind(1,age,k5,k618,inc,hc,lwg,wc)
source("C:/Users/Gokul/Downloads/Logit.R")
logit(X,Y,X.names=c("Constant","age","k5","k618","inc","hc","lwg","wc"))

reg_all<-glm(lfp~1+age+k5+k618+lwg+inc+wc+hc,family=binomial("logit"),data=Mroz)
summary(reg_all)

# H_0:  beta1=beta2=beta3=beta4=beta5=beta6=beta7= 0
# H_a:  beta1=beta2=beta3=beta4=beta5=beta6=beta7 != 0

reg_a<-glm(lfp~1,family=binomial("logit"),data=Mroz)
summary(reg_a)

anova(reg_a,reg_all,test='Chisq')

# change of deviance is 124.48 between complete model and model with all attributes set to 0.
# degree of freedom is 7.
# P-value is 2.2e-16.

#2)b

# H_0:   Does not depend on children 
# H_a:  Does depend on children 

reg_young_old<-glm(lfp~1+age+lwg+inc+wc+hc,family=binomial("logit"),data=Mroz)
summary(reg_young_old)

summary(reg_all)
anova(reg_young_old,reg_all,test='Chisq')

# change of deviance is 66.485 between complete model and model without number of children.
# degree of freedom is 2.
# P-value is 3.655e-15.
# So, we can say about 66.485% of effect of children on our data set.

#2)c

# H_0:   Does not depend on wifes college attendance 
# H_a:   Does depend on wifes college attendance  

reg_n0_col<-glm(lfp~1+age+k5+k618+lwg+inc+hc,family=binomial("logit"),data=Mroz)
summary(reg_n0_col)  

summary(reg_all)

anova(reg_n0_col,reg_all,test='Chisq')

# change of deviance is 12.724 between complete model and model without wife's college attendance.
# degree of freedom is 1.
# P-value is 0.000361.
# So, we can say about 12.724% of effect of Women college attendance on our data set.

########################################################################


#######################################################################