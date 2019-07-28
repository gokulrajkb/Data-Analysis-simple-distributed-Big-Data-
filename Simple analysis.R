############################################################################
############################################################################

#From,

#      Gokul Kaisaravalli Bhojraj
#      Id: 80789
#      Business Intelligence

############################################################################
############################################################################

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# 1)a

###########################


vec1<-c(0,2,3,0,2,11,0,7,NA)  # creating vector involving NA
 
vec1<-vec1[-9]               # removing NA and reassigning to same vector

# OR

vec1<-vec1[1:8]

# OR

vec1<-vec1[!is.na(vec1)]

# OR

for(i in 1: length(vec1))
{
    if(is.na(vec1[i]))
    {
         vec1 <-  vec1[-i]
    }
}
vec1



# 1)b

###################################

vec2<-c(T,F,F,T,F,F,T,F)    # creating LOGICAL vector


# 1)c

####################################

#  checks each value in vec2 and each time 
#  when 0 is found it is stored to zero vector

zeros<-NULL                   #
for(i in 1:length(vec2))
{
  if(!isFALSE(vec2[i]))
  {
    zeros<-c(zeros,vec1[i])
  }
}
zeros

# 1)d

####################################

# used length function to calculate number of zeros

length(zeros)

# OR

count<-0
for(i in 1 :length(vec1))
{
  if(vec1[i]==0)
  {
    count=count+1
  }
}
count

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# 2)a

#######################################

#creating data frame from using given data fromthe table

year<-c(2003,2004,2005,2006,2007,2008,2009,2010,2011)
Gender<-c("men","men","men","men","men","men","men","men","men",
        "women","women","women","women","women","women","women","women","women")
w<-c(120 ,122 ,124 ,130 ,136 ,140 ,143 ,150 ,155 ,
     109 ,112 ,115 ,121 ,128 ,132 ,135 ,140 ,148)

 # OR

year<-seq(2003,2011,1)
Gender<-rep(c("men","women"),c(9,9))
w<-c(120 ,122 ,124 ,130 ,136 ,140 ,143 ,150 ,155 ,
     109 ,112 ,115 ,121 ,128 ,132 ,135 ,140 ,148)
data<-data.frame(year,w,Gender)

str(data)             # just to check the structure

# 2)b

######################################

# exporting the data present in data frame to the csv file

write.table(data,file = "C:/Users/Gokul/Desktop/MS/Statistical R/filesstored111.csv", 
            sep = ",", row.names = F, col.names = TRUE,quote = FALSE)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# 3)a

#####################################

library(XLConnect)

?readWorksheetFromFile


path<-"c:/Users/Gokul/Downloads/Freedman.xlsx"
Freedman<-readWorksheetFromFile(path,sheet=1,header=TRUE) # importing data table

head(Freedman)
class(Freedman)
names(Freedman)
typeof(Freedman)
Freedman[1]

# 3)b

#######################################

summary(Freedman)

# 3)c

#########################################

# converts the data type into numeric from other kinds

Freedman$density<-as.numeric(Freedman$density)
Freedman$crime<-as.numeric(Freedman$crime)
Freedman$population<-as.numeric(Freedman$population)
Freedman$nonwhite<-as.numeric(Freedman$nonwhite)

        
typeof(Freedman$population)   # checking the type of the variable
typeof(Freedman$nonwhite)
typeof(Freedman$density)
typeof(Freedman$crime)

# 3)d

##########################################

#displaying the values side by side, one taken from data we have inside R
# and the other directly from the data table, just to see they are same or not

evaluate<-data.frame(Freedman[2],Freedman$population)
evaluate

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# 4)a

########################################

install.packages(car)
library(car)

# 4)b

#######################################

??Prestige
?Prestige


# 4)c

#########################################

head(Prestige)
Prestige<-carData::Prestige

# creating subset with women incumbents greater than 50

sub_Prestige<-subset(Prestige,women>50)
sub_Prestige

# 4)d

##########################################

# calculating avg using subset with women incumbents greater than 50 that
# we have already created

avg<-mean(sub_Prestige[,4])
avg

# 4)e

#########################################

# creating subset with women incumbents less than 50
# calculating avg using subset with women incumbents less than 50 that
# we have already created

sub1_Prestige<-subset(Prestige,women<50)
avg1<-mean(sub1_Prestige[,4])
avg1

# 4)f

############################################

# here I use two for loops
# one keep track of kind of occupation
# other keeps track of all the prestige scores
# later I match the occupation and store it to a variable ( to create sum)
# then divide it by the number of prestige score of the corresponding occupation
# then for each type of occupation the calculted avg is stored in a vector

new_Prestige<-na.omit(Prestige)

level<-levels(new_Prestige$type)
arr<-0
avg_all_type<-0
for(i in 1:length(level))
{
  arr[i]<-level[i]
  sum<-0
  count<-0
  for(j in 1:length(new_Prestige$type))
  {
    if(arr[i]==new_Prestige$type[j])
    {
      sum<-sum+new_Prestige$prestige[j]
      count<-count+1
    }
    mean<-sum/count
    
  }
  avg_all_type[i]<-mean
}
avg_all_type

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@