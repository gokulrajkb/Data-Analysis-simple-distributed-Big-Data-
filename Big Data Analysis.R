############################################################################
############################################################################

#From,

#      Gokul Kaisaravalli Bhojraj
#      Id: 80789
#      Business Intelligence

############################################################################
############################################################################

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



# 1)###################################################

library(ff)
library(ffbase)

# 1)a

system.time(ffx08<-
              read.csv.ffdf(file="/Users/Gokul/Desktop/MS/Statistical R/leture/2008.csv.bz2", header=TRUE,
                            na.string=c("",NA),colClasses=
                              c(Month="factor",DayOfWeek="factor", Year="factor")))
system.time(ffx07<-
              read.csv.ffdf(file="/Users/Gokul/Desktop/MS/Statistical R/leture/2008.csv.bz2", header=TRUE,
                            na.string=c("",NA),colClasses=
                              c(Month="factor",DayOfWeek="factor", Year="factor")))

# 1)b

save.ffdf(ffx07, dir = "/Users/Gokul/Desktop/MS/2007.csv.bz2",
          clone = FALSE, relativepath = TRUE,overwrite = TRUE)

save.ffdf(ffx08, dir = "/Users/Gokul/Desktop/MS/2008.csv.bz2",
          clone = FALSE, relativepath = TRUE,overwrite = TRUE)


load.ffdf(dir="/Users/Gokul/Desktop/MS/2008.csv.bz2")
load.ffdf(dir="/Users/Gokul/Desktop/MS/2007.csv.bz2")

summary(ffx07)
summary(ffx08)

# 1)c

sub_ffx08<-subset(ffx08,Month==12)

sub_ffx08$Month<-droplevels(sub_ffx08$Month)

summary(ffx08)


# 2 ################################################################

# 2)a

# 2)a 1

source("C:/Users/Gokul/Downloads/Chunk_lm.R")

# 2008

load.ffdf(dir="/Users/Gokul/Desktop/MS/2008.csv.bz2")

form<-ArrDelay~Origin+Distance

system.time(reg1<-Chunk_lm(form,data=ffx08,chunkSize=75000,sandwich=FALSE,beta_h=NULL,cores=4))

##  system.time(reg <-biglm(form, data=ffx08,sandwich=FALSE))

# 2007

load.ffdf(dir="/Users/Gokul/Desktop/MS/2007.csv.bz2")

form2<-ArrDelay~Origin+Distance

system.time(reg2<-Chunk_lm(form2,data=ffx07,chunkSize=75000,sandwich=FALSE,beta_h=NULL,cores=4))

save(reg1,reg2,file="reg_7_8.RData")


source("C:/Users/Gokul/Downloads/Predicted_airports.R")

pred_7<-pred_airports(beta_h=reg2$coef,data=ffx07,fix_num=mean(ffx07$Distance[]))

pred_8<-pred_airports(beta_h=reg1$coef,data=ffx08,fix_num=mean(ffx08$Distance[]))

head(pred_7)

# 2)a 2


pred_data<-data.frame(Origin=c(names(pred_7),names(pred_8)),
rbind(data.frame(pred=pred_7,year=2007),data.frame(pred=pred_8,year=2008)))

names(pred_data)<-c("Origin","pred","year")

# 2)b

library(XLConnect)

airports<-read.table(file="C:/Users/Gokul/Desktop/MS/Statistical R/leture/airports.csv",
                     header=TRUE,sep= ",",dec=".",na.string="NA")

head(airports)

# 2)c

?merge

plot_data<-merge(airports,pred_data,by.x="iata",by.y = "Origin")

# 3) #############################################################

# 3)a

install.packages("ggplot2")
library(ggplot2)
library(maps)

# 3)b

map.us <- map_data(map = "state")
p_1 <- ggplot()
p_1 <- p_1 + geom_polygon(data=map.us,
                        aes(x = long, y = lat,group=group),fill = grey(0.5))
p_1

# 3)c

p_1<-p_1+geom_point (data = plot_data,
                   aes (x = long, y = lat),pch = 16)
p_1

#Spliting in to two plots based on year

p_1<-p_1 + facet_grid(. ~ year)
p_1

# 3)d

plot_sub<-subset(plot_data,lat<50&lat>25)

rm(p_1)
map.us <- map_data(map = "state")
p_1 <- ggplot()
p_1 <- p_1 + geom_polygon(data=map.us,
                        aes(x = long, y = lat,group=group),fill = grey(0.5))
p_1


p_1<-p_1+geom_point (data = plot_sub,
                   aes (x = long, y = lat),pch = 16)
p_1

#Spliting in to two plots based on year

p_1<-p_1 + facet_grid(. ~ year)
p_1


# 3)e

p_1<-p_1+geom_point (data = plot_sub,aes (x = long,y = lat,colour =pred
),pch = 16)+theme(legend.position=c(.5, .175))+labs(colour="Color")+
scale_colour_gradient(low = "#56B1F7", high = "#132B43")
p_1

# 3)f

p_1<-p_1+geom_text(data = plot_sub,aes (
  x = long, 
  y = lat,label=round(pred,0)
),size=3.2,vjust=0.6)
p_1

rm(p_1)
p_1 <- ggplot()
p_1 <- p_1 + geom_polygon(data=map.us, aes(x = long, y = lat,group=group),fill = grey(0.5))
p_1<-p_1+geom_point (data = plot_sub,aes (x = long, y = lat,colour =pred
),pch = 16) +theme(legend.position=c(.5, .175))+labs(colour="Color")+
  scale_colour_gradient(low = "#56B1F7", high = "#132B43")
p_1<-p_1 + facet_grid(. ~ year)

#taking Top 1% add delay

plot_sub2<-subset(plot_sub,pred>=quantile(pred, probs = 0.99))
p_1<-p_1+geom_text(data = plot_sub2,aes (
  x = long, 
  y = lat,label=round(pred,0)
),size=3.2,vjust=0.6)
p_1

# 3)g

p_1<-p_1+geom_text(data = plot_sub2,aes (
  x = long, 
  y = lat,
  label=iata
),size=3.2,vjust=-0.5)
p_1

# 3)h

numb07<-table(ffx07$Origin[])
numb08<-table(ffx08$Origin[])

#Making a data.frame with the information from table

flights<-data.frame(c(dimnames(numb07)[[1]],dimnames(numb08)[[1]]),
                    rbind(cbind(as.numeric(numb07),2007),cbind(as.numeric(numb08),2008)))
names(flights)<-c("Origin","numb","year")

#Merging with plot_sub

plot_sub2<-merge(plot_sub,flights,by.x=c("iata","year"),by.y=c("Origin","year"))

# 3)i

rm(p_1)
p_1 <- ggplot()
p_1 <- p_1 + geom_polygon(data=map.us,
                        aes(x = long, y = lat,group=group),fill = grey(0.5))
p_1<-p_1+geom_point (
  data = plot_sub2,
  aes (x = long, y = lat,colour = pred,size =numb/1000),
  pch = 16)+labs(size = "Flights (k)",colour="Delay")+
  theme(legend.position=c(0.5, .25))+
  scale_colour_gradient(low = "#56B1F7", high = "#132B43")
p_1<-p_1 + facet_grid(. ~ year)
p_1

# 4 ####################################################################

plot_sub3<-subset(plot_sub2,(numb>=quantile(numb, probs = 0.95)&year==2007)|(numb>=quantile(numb, probs = 0.95)&year==2008))

p_1<-p_1+geom_text(data = plot_sub3,aes (
  x = long, 
  y = lat,label=round(pred,0)
),size=3.2,vjust=0.6)

p_1<-p_1+geom_text(data = plot_sub3,aes (
  x = long, 
  y = lat,
  label=iata
),size=3.2,vjust=1.7)

#Ading city names

p_1<-p_1+geom_text(data = plot_sub3,aes (
  x = long, 
  y = lat,
  label=city
),size=3.2,vjust=-0.5)
p_1

# saving the map

ggsave ("C:/Users/Gokul/Desktop/MS/Statistical R/leture/map.pdf", plot =p_1)

####################################################################

###################################################################