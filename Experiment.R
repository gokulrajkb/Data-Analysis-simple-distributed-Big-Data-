############################################################################
############################################################################

#From,

#      Gokul Kaisaravalli Bhojraj
#      Id: 80789
#      Business Intelligence

############################################################################
############################################################################

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

###############################################################

# 1)

x1<-seq(0,30,0.01)
y1<-sin(x1)
plot(x1,y1, xlim=c(0,6),ylim=c(-1,1))
abline(h=0,col="red")

numIn<-0
pival<-c()
for(i in 1:100)
{
  x<- runif(1,min=0,max=1)
  y<- runif(1,min=0,max=1)
  points(x,y,col="red")
  if (y<=sin(x))
  {
    numIn<- numIn+1
  }
  else
  {
    numIn<- numIn
  }
  pival[i]<-4*(numIn/i)
}
pival


##############################################################

# 2)

count<-0
output<-0

for(i in 1:1000)
{
  output<-sample(x=1:6,replace=T,size=2)
  
  if(output[1]==output[2])
  {
    count<-count+1
  }
   else
   {
     count<-count
   }
}
prob<-count/(1000)

count

prob

##############################################################

# 3)


prod_weight_mean<-c()
observed_weight_sample<-c(119,120,131,209,210,337,332,287,146,129,
                          232,169,208,253,142,105,419,179,324,287)
for(i in 1:200)
{
  
  sample_1<-sample(observed_weight_sample,size=length(observed_weight_sample),replace=T)
  prod_weight_mean[i]<-mean(sample_1)
}
Overall_mean_weight<-mean(prod_weight_mean)
hist(prod_weight_mean)

prod_weight_mean
quantile(prod_weight_mean,c(0.05,0.95))
Overall_mean_weight

###########################################################

# 4)

sam_beta<-c()
sam_alpha<-c()
sam_beta_mean<-0
sam_alpha_mean<-0
Id<-c(1,2,3,4,5)
sam_x<-c(1,2,3,5,4)
sam_y<-c(13,14,15,18,22)

data1<-data.frame(Id,sam_x,sam_y)
data1

for(i in 1:200)
{
    indexRow <- sample(1:5, size = 5, replace = T)
    sample1 <- data1[indexRow, ]
    sam_beta[i]<- coef(lm(sam_y~ sam_x, data = sample1))[2]
    sam_alpha[i]<-coef(lm(sam_y~ sam_x, data = sample1))[1]
    summary(lm(sam_y~ sam_x, data = sample1))
}
sam_beta_mean<-mean(sam_beta)
sam_alpha_mean<-mean(sam_alpha)

sam_beta
sam_alpha

par(mfrow=c(2,1))
hist(sam_beta)
hist(sam_alpha)
par(mfrow=c(1,1))
quantile(sam_beta,c(0.05,0.95))
quantile(sam_alpha,c(0.05,0.95))

sam_beta_mean
sam_alpha_mean

 ##################################################################

#5)

#   H0: Children NOT affected from bottled milk.
#   H1: Children affected from bottled milk.

id<-seq(1,19,1)
bottle <- c(0,1,1,0,0,0,1,0,0,0,1,1,0,1,1,1,1,0,1)
age <- c(9,14,15,10,12,6,19,10,8,6,12,13,20,13,16,14,9,12,12)
d_f<-data.frame(id,bottle,age)

sam_avg_bottle<-0
sam_avg_no_bottle<-0
sam_avg<-c()
bottle_sam<-c()
n<-0
m<-0

for(i in 1:200)
{
  
   id_sam<-sample(0:1,size=19,replace=T)
   d_f_sam<-data.frame(id,bottle,id_sam,age)
   bottle_sam<-d_f_sam$id_sam==1
   for(j in 1:19)
   {
       if(bottle_sam[j]==TRUE)
       {
          sam_avg_bottle<-sam_avg_bottle+d_f_sam$age[j]
          n<-n+1
       }
       else
       {
          sam_avg_no_bottle<-sam_avg_no_bottle+d_f_sam$age[j]
          m<-m+1
        }
     }
   sam_avg[i]<- (sam_avg_bottle/n)- (sam_avg_no_bottle/m)
}
sam_avg
avg<-mean(sam_avg)
hist(sam_avg)
quantile(sam_avg,c(0.025,0.975))
avg

# The actual average is 3.367, we need to compare our
# experimental value to this(3.367) and decide whether or not to reject the Ho.

############################################################################
               #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
###########################################################################

