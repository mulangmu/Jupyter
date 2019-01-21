## R TO READ SMAP SOILMOISTURE FROM EXCEL
## WRITTEN BY YANG LANG 1/17/2019
##RUN SCRIPTS ON WINDOWS
#source("D:\\data\\soilmoisture\\R_code\\r_to_read_smap_of_soilmoisture_from_excel.R")


library(readxl)
setwd("D:\\data\\soilmoisture")
smapdata=read_excel("D:\\data\\soilmoisture\\SMAP\\smapdata.xlsx")
## LIST ALL .XLSX FILES
time3<-seq(as.Date('2014-12-01'),as.Date('2018-11-30'), by='day')
length(time3)
n=array(1:length(time3))
n[1:length(time3)]=NA
library("zoo") # FOR LIBRARY XTS
library("xts")
sm=xts(n,time3)
smap=xts(smapdata,time3)
#as.numeric(smap)#COULD NOT TRANSFER TO INTERGER
smap=cbind(sm,smap)
smap=merge(sm,smap)
smap[smap==-999.00000]<-NA
smap=apply.daily(smap,mean,na.rm=T)
smap=smap[,3:39]

##CORRELATION COEFFICIENTS
##FOR LOOP EVERY STATIONS
for(i in 1:37){
	print(i)
	smap_cor=cbind(svwc_ddd[,i],smap[,i]) ##MUST HAVE THE SAME LENGTH
	smap_cor=apply.daily(smap_cor,mean,na.rm=T) ##MUST HAVE THE SAME TIMEZONE
##DELETE THE NA RAW
	smap_co=na.omit(smap_cor)
	smap_r[i]=cor(smap_co)[1,2]	
}

cor.test(smap_cor[,1],smap_cor[,2])


##STATISTICS PACKAGE
install.packages("Metrics")
library("Metrics")
##
bias()
mae()
rmse()

for(i in 1:37){
	print(i)
	smap_cor=cbind(svwc_ddd[,i],smap[,i]) ##MUST HAVE THE SAME LENGTH
	smap_cor=apply.daily(smap_cor,mean,na.rm=T) ##MUST HAVE THE SAME TIMEZONE
##DELETE THE NA RAW
	smap_co=na.omit(smap_cor)
	x=smap_co[,1]
	y=smap_co[,2]
	x_mean[i]=mean(x,na.rm=T)
	smap_bias[i]=bias(x,y)
	smap_mae[i]=mae(x,y)
	smap_rmse[i]=rmse(x,y)
	
}
print(smap_r)
print(smap_bias)
print(smap_mae)
print(smap_rmse)


smap_r=data.frame(smap_r)
smap_bias=data.frame(smap_bias)
smap_mae=data.frame(smap_mae)
smap_rmse=data.frame(smap_rmse)

smap_stat=cbind(smap_r,smap_bias,smap_mae,smap_rmse)
write.csv(x_mean,"smap_statistic.csv")


##SAVE THE BOXPLOT AS AN IMAGE
png()
boxplot(smap_stat)
dev.off()

##SAVE THE TIMESERIES PLOT
for(i in 1:37){
	print(i)
	smap_cor=cbind(svwc_ddd[,i],smap[,i]) ##MUST HAVE THE SAME LENGTH
	smap_cor=apply.daily(smap_cor,mean,na.rm=T) ##MUST HAVE THE SAME TIMEZONE
##DELETE THE NA RAW
	smap_co=na.omit(smap_cor)
	png()
	plot(smap_co,type="b")
	dev.off()
}


##SCATTER PLOT
 library("ggplot2")
ggplot(smap_cor,aes(x=hor_data56586, y=smap_56586))+geom_point()


##ADD LEGEND FUNCTION
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
    mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

png(width=14,height=9,units="in",res=400)
par(mfrow=c(2,1))
#plot(smap_cor,pch=18,type="p",main=substring(names(smap_cor[1,1]),9,13))
#plot(smap_cor[800:1100],pch=18,type="p",main=substring(names(smap_cor[1,1]),9,13))
plot(smap_cor,pch=18,type="p",main="WEISHAN",9,13)
plot(smap_cor[800:1100],pch=18,type="p",main="WEISHAN",9,13)
add_legend("top", legend=c("OBS","SMAP"), pch=18, col=c("black","red"),horiz=TRUE, bty='n', cex=1.0)
dev.off()



