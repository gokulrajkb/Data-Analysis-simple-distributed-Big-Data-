############################################################################
############################################################################

#From,

#      Gokul Kaisaravalli Bhojraj
#      Id: 80789
#      Business Intelligence

############################################################################
############################################################################

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# 1)

install.packages("AER")
library(AER)                                #loading library
data_CPS1985<- data(CPS1985)                # storing data to a variable

?CPS1985                                    # getting information about data table

# 1)a

summary(CPS1985)                            # knowing summary of the overall data
head(CPS1985)                               # checking the table

# 1)b

reg<-lm(wage~education,data=CPS1985,y=T)    # creating regression using lm command
summary(reg)                                # getting details of the regresssion

# 1)c

beta0_h<- summary(reg)$coef[1,1]            # getting beta0 value
beta1_h<- summary(reg)$coef[2,1]            # getting beta1 value

wage_y_h<-beta0_h+beta1_h*10                # average wage for 10 yr education
wage_y_h

################# Ploting graph for visualization #####################

wage_y_h1<-beta0_h+(beta1_h*CPS1985$education)
wage_y_h1

plot(CPS1985$education,CPS1985$wage,xlab = "education (independent)",ylab = "wage( dependent)")
points(CPS1985$education,wage_y_h1,col="red",type="l")

# 1) d


# H_o:  beta=0    (no longer wage is dependent on education)
# H_a:  beta!=0
# since P value is 2.2e-16 which is less than 0.05 ,we reject H_o i.e we accept H_a.
# which means wage is dependent on education

# 1)e

# so accoring to regression result, the value of r-squared is very low (0.1459),
# so we can say that education doesnot have high impact on wage but it has
# some effect on wage 

# 2)##############################################################

# 2)a

reg1<-lm(wage~gender,data=CPS1985,y=T)     # creating regression using lm using gender and wage
summary(reg1)

plot(CPS1985$gender,CPS1985$wage,xlab="Gender(independent)",ylab="wage( dependent)")

# 2)b

# getting summary 
summary(reg1)                              

# getting avg wage of female 
wage_female=reg1$coefficients[1]+reg1$coefficients[2]*1
wage_female
# getting avg wage of male 
wage_male=reg1$coefficients[1]+reg1$coefficients[2]*0
wage_male

# From the values you can say that male earns more than female.

# 2)c

# yes, Since the difference is significant between the wages we can say it is significant.

# 2)d

# creating dummy variables 

CPS1985$gender <-  factor(CPS1985$gender,levels = c('male', 'female'),labels = c(0, 1))  
reg2 <- lm(wage ~ gender, data = CPS1985)
summary(reg2)

##################### OR #######################

library(AER)  
data_CPS1985<- data(CPS1985)
gen_num<-NULL
for(i in 1:length(CPS1985$gender))
{
  if(CPS1985$gender[i]=="male")
  {
    gen_num<-c(gen_num,0)
  }
  else
  {
    gen_num<-c(gen_num,1)
  }
}
gen_num

reg2<-lm(wage~gen_num,data=CPS1985,y=T)  # regrssion using lm command
summary(reg2)

################ Plotting graph for visualization ########################

beta0_h<- summary(reg2)$coef[1,1]
beta1_h<- summary(reg2)$coef[2,1]

wage_y_h2<-beta0_h+(beta1_h*gen_num)
wage_y_h2

plot(gen_num,CPS1985$wage,xlab = "Gender 1= male/ 2= Female (independent)",ylab = "wage( dependent)")
points(gen_num,wage_y_h2,col="red",type="l")


# 2)e


sub_male<-subset(CPS1985,CPS1985$gender=="male")
sub_female<-subset(CPS1985,CPS1985$gender=="female")

male_avg<-mean(sub_male$wage)
female_avg<-mean(sub_female$wage)

male_avg
female_avg

#yes,they are same male= 9.9949 and female= 7.8788