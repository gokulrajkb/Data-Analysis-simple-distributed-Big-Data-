############################################################################
############################################################################

#From,

#      Gokul Kaisaravalli Bhojraj
#      Id: 80789
#      Business Intelligence

############################################################################
############################################################################

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# 1) ###########################################################


# 1)a

R_a<- diag(1,4)
r_a <- cbind(c(0,0,0,0))
R_a
r_a

# 1)b

R_b <- rbind(c(1,0,0,0), c(0,1,0,0))
r_b <- cbind(c(0,0))
R_b
r_b

# 1)c

R_c <- rbind(c(1,0,0,0), c(0,1,0,0))
r_c <- cbind(c(1,1))
R_c
r_c

# 1)d

R_d <- rbind(c(0,1,0,0))
r_d <- 0
R_d
r_d

# 1)e

R_e <- rbind(c(0,1,1,0))
R_e

# OR

r_e <- 1
r_e

# OR
r_e <- cbind(c(0,1))
r_e

# OR
r_e <- cbind(c(1,0))
r_e

# 2) #################################################################

install.packages("AER")
library("AER")
CPS <- data("CPS1985")


exp_2 <- CPS1985$experience^2
exp_3 <- CPS1985$experience^3

# First model

Y_log <- log(CPS1985$wage)

#Second model

Y <- CPS1985$wage

# 2)a

reg1 <- lm(Y_log~education+married+gender+experience+exp_2+exp_3, data = CPS1985)
summary(reg1)

reg2 <- lm(Y~education+married+gender+experience+exp_2+exp_3, data = CPS1985)
summary(reg2)


# 2)b

par(mfrow=c(1,2))
plot(fitted(reg2),resid(reg2),xlab="Fitted values",
     ylab="Residuals")
plot(reg2,which=4)

# Removing Outliers: first model

data_no_out <- CPS1985[-170,]
Ylog_no_out <- log(data_no_out$wage)
exp_2 <- data_no_out$experience^2
exp_3 <- data_no_out$experience^3
reg3 <- lm(Ylog_no_out~education+married+gender+experience+exp_2+exp_3, data = data_no_out)
summary(reg1)
summary(reg3)

#Outliers remotion: second model

data_no_out <- CPS1985[-170,]
Y_no_out <- data_no_out$wage
exp_2 <- data_no_out$experience^2
exp_3 <- data_no_out$experience^3
reg4 <- lm(Y_no_out~education+married+gender+experience+exp_2+exp_3, data = data_no_out)
summary(reg2)
summary(reg4)


# Removing One outlier made not much of the differnce
# However it is better than before
# Since Model 1 seems to be better; I will proceed with model 1 to make more better

# Removing more Outliers: First model 

data_no_out_2 <- CPS1985[cooks.distance(reg1) < 0.01,]
Y_no_out_2 <- log(data_no_out_2$wage)
exp_2_no_out_2 <- data_no_out_2$experience^2
exp_3_no_out_2 <- data_no_out_2$experience^3
reg5 <- lm(Y_no_out_2~education+married+gender+experience+exp_2_no_out_2+exp_3_no_out_2, data = data_no_out_2)
summary(reg1)
summary(reg5)

# By removing all outliers by using Cook's distance is bigger than 0.01 
# we have 0.08 improvement in R_squared.

# 2)c

# I think Model 1 is better I will select Model 1 instead of Model 2

coeftest(reg5, vcov = vcovHC(reg5, type="HC1"))


# 2)d

# Since, after removing outliers I got better results 
# I will keep using model 1 with removed outliers


# 3) ###############################################################

# 3)a

exp_2 <- CPS1985$experience^2
exp_3 <- CPS1985$experience^3
exp_4 <- CPS1985$experience^4
exp_5 <- CPS1985$experience^5
reg6 <- lm(Y_log~education+married+gender+experience+exp_2+exp_3+exp_4+exp_5, data = CPS1985)
summary(reg6)
summary(reg1)

# Since we cannont see much of the effect on dependent variable when we us degree 5
# So it is better to use linear model than non-linear model.

# 3)b

R_6 <- rbind(c(0,0,0,0,0,1,0,0,0), c(0,0,0,0,0,0,1,0,0), c(0,0,0,0,0,0,0,1,0), c(0,0,0,0,0,0,0,0,1))
r_6 <- cbind(c(0,0,0,0))
R_6 
r_6 

V_b <- vcovHC(reg6)

linearHypothesis(reg6, hypothesis.matrix = R_6, rhs =r_6,
                 test = c("Chisq"), vcov.=V_b)

# 3)c

R_7 <- rbind(c(0,0,0,0,0,0,0,1,0), c(0,0,0,0,0,0,0,0,1))
r_7 <- cbind(c(0,0))
R_7
r_7

linearHypothesis(reg6, hypothesis.matrix = R_7, rhs =r_7,
                 test = c("Chisq"), vcov.=V_b)


# 3)d

# Since, we have seen multiple degree of experience has no much effect on
# dependent variable.So, I would prefer degree 3 over other


# 4) ###################################################################


# 4)a

wage_premium_selector<- CPS1985$gender:CPS1985$married

reg7 <- lm(Y_log~education+married+gender+experience+exp_2+exp_3+wage_premium_selector, data = CPS1985)
summary(reg7)

reg8<- lm(Y_log~education+married+gender+experience+exp_2+exp_3, data = CPS1985)
summary(reg8)

# Here we can observe that without including wage_premium_selector
# It showed positive effect on the wages
# After adding wage_premium_selector it has negative effect on wages
# Which means married women has negative effect on the dependent variable i.e wages

# 4)b


# From above model we can observe that married women have negative effect on thier
# wages, However if create a dummy for married men and as well from married women
# We have following result

married_women<-NULL
for(i in 1:length(CPS1985$married))
{
       if(CPS1985$gender[i]=="female" && CPS1985$married[i]=="yes")
              married_women<-c(married_women,1)
       else
              married_women<-c(married_women,0)
}

reg_married_women <- lm(Y_log~education+married+gender+experience+exp_2+exp_3+married_women, data = CPS1985)
summary(reg_married_women)


married_men<-NULL
for(i in 1:length(CPS1985$married))
{
  if(CPS1985$gender[i]=="male" && CPS1985$married[i]=="yes")
    married_men<-c(married_men,1)
  else
    married_men<-c(married_men,0)
}

reg_married_men <- lm(Y_log~education+married+gender+experience+exp_2+exp_3+married_men, data = CPS1985)
summary(reg_married_men)


summary(reg_married_men)
summary(reg_married_women)

# From the model we can say that there is a effect on the wages from marriage
# women have negative effect after marriage
# positive effect on wages men after marriage

# Hence,we can say that men have a wage premium from marriage

######################################################################

#######################################################################