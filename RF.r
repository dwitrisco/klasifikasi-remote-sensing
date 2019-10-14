library(sp)
library(caret)
library(ggplot2)
library(rpart)
library(randomForest)
library(ROCR)        
library(rpart.utils) 
library(rpart.plot)  
library(party)      
library(tidyr)

#====================================================
#AKURASI 0,7273
#====================================================
#a. bagi data
data1<-read.csv('D:/2. bahan thesis/2. syntax/olah/imput.full 4.rsquare.csv', header=TRUE, sep = ';')
dim(data1)
data<-data1[,-c(1,2)]

data$makEVI<-apply(data[,c(8,19,30,41)],1,max)
data$minEVI<-apply(data[,c(8,19,30,41)],1,min)
data$makNDWI<-apply(data[,c(11,22,33,44)],1,max)
data$minNDWI<-apply(data[,c(11,22,33,44)],1,min)

data$varEVI<-apply(data[,c(8,19,30,41)],1,var)
data$varNDVI<-apply(data[,c(9,20,31,42)],1,var)
data$varNDBI<-apply(data[,c(10,21,32,43)],1,var)
data$varNDWI<-apply(data[,c(11,22,33,44)],1,var)

data$selEVI1<-data[,8]-data[,19]
data$selEVI2<-data[,8]-data[,30]
data$selEVI3<-data[,8]-data[,41]
data$Y<-as.factor(data1$label)
data$Z<-as.factor(data1$kelas)
data$selNDWI1<-data[,11]-data[,22]
data$selNDWI2<-data[,11]-data[,33]
data$selNDWI3<-data[,11]-data[,44]
data$makNDBI<-apply(data[,c(10,21,32,43)],1,max)
data$minNDBI<-apply(data[,c(10,21,32,43)],1,min)

data$a12<-(data$a1-data$a2)/(data$a1+data$a2)
data$var1<-apply(data[,c(1,12,23,34)],1,var)
data$selA1<-data[,1]-data[,12]
data$selA2<-data[,1]-data[,23]
data$selA3<-data[,1]-data[,34]
data$meanNDWI<-apply(data[,c(11,22,33,44)],1,mean)
data$a35<-(data$a3-data$a5)/(data$a3+data$a5)
data$a37<-(data$a3-data$a7)/(data$a3+data$a7)
data$b37<-(data$b3-data$b7)/(data$b3+data$b7)
data$c37<-(data$c3-data$c7)/(data$c3+data$c7)
data$d37<-(data$d3-data$d7)/(data$d3+data$d7)
data$var37<-apply(data[,c(103:106)],1,var)
data$max.aNDWI<-data$makNDWI-data$aNDWI
data$max.plus.aNDWI<-data$makNDWI+data$aNDWI
data$meanEVI<-apply(data[,c(8,19,30,41)],1,mean)
data$mean.sel<-apply(data[,c(86:88)],1,mean)
data$var5<-apply(data[,c(5,16,27,38)],1,var)
data$a56<-(data$a5-data$a6)
data$b56<-(data$b5-data$b6)
data$c56<-(data$c5-data$c6)
data$d56<-(data$d5-data$d6)
data$var56<-apply(data[,c(113:116)],1,var)
data$sel51<-(data$a5-data$b5)
data$selEVmaks<-(data$aEVI-data$makEVI)
data$sel51xa<-data$sel51*data$a5
data$makEVI<-apply(data[,c(8,19,30,41)],1,max)
data$lag<-ifelse(data$makEVI=data[,8],1,ifelse(data$makEVI=data[,19],2,ifelse(data$makEVI=data[,30],3,4)))

data$EVINDBI<-data$aEVI-data$aNDBI
data$EVINDWI<-data$aEVI-data$aNDWI
#-----------------------------------
set.seed((10))
acak <- sample(1:nrow(data), 110)
training <- data[-acak,]
validasi <- data[acak,]

#b. tambahkan label di kanan
dt.y<-training
ds.y <-validasi

#c. random forest
dataN<-dt.y[,c(1:11,89)]
dataM<-ds.y[,c(1:11,89)]

dataN<-dt.y[,c(1,6,7,103,89)]
dataM<-ds.y[,c(1,6,7,103,89)]

model.forest <- randomForest(as.factor(Y)~., data=dataN, na.action = na.omit)
pred.forest <- predict(model.forest, dataM)
eval.rf <- confusionMatrix(pred.forest, dataM$Y, positive = "1")
eval.rf

varImpPlot(model.forest)

d<-cbind(pred.forest,ds.y)
write.csv(d, 'D:/2. bahan thesis/2. syntax/olah/periksa prediksi bulan.csv', row.names=TRUE, na="NA")

#cek grafik generatif
pred.forest <- predict(model.forest, dataN)
eval.rf <- confusionMatrix(pred.forest, dataN$Y, positive = "1")
eval.rf
d<-cbind(pred.forest,dt.y)
write.csv(d, 'D:/2. bahan thesis/2. syntax/olah/periksa prediksi train bulan.csv', row.names=TRUE, na="NA")

par(mfrow=c(1,2))
boxplot(data[,11]~data[,89],ylab='', main='36 NDWI',cex=1.2)
boxplot(data[,84]~data[,89],ylab='', main='37',cex=1.2)

par(mfrow=c(1,4))
boxplot(data[,1]~data[,89],ylab='', main='Band A1',cex=1.2)
boxplot(data[,12]~data[,89],ylab='',main='Band A2')
boxplot(data[,23]~data[,89],ylab='',main='Band A3')
boxplot(data[,24]~data[,89],ylab='',main='Band A4')

boxplot(data[,1]~data[,89],ylab='',axes=FALSE,main='Band A1',cex=1.2)
boxplot(data[,12]~data[,89],ylab='',axes=FALSE,main='Band A2')
boxplot(data[,23]~data[,89],ylab='',axes=FALSE,main='Band A3')
boxplot(data[,24]~data[,89],ylab='',axes=FALSE,main='Band A4')


#==========
#boxplot
#==========
EVI<-data[,c(41,30,19,8,77,89)]
EVI1<-EVI[EVI$Y==1,]
med1<-apply(EVI1[,c(1,2,3,4)], 2, FUN = median)
EVI2<-EVI[EVI$Y==2,]
med2<-apply(EVI2[,c(1,2,3,4)], 2, FUN = median)
EVI3<-EVI[EVI$Y==3,]
med3<-apply(EVI3[,c(1,2,3,4)], 2, FUN = median)
EVI4<-EVI[EVI$Y==4,]
med4<-apply(EVI4[,c(1,2,3,4)], 2, FUN = median)
EVI5<-EVI[EVI$Y==5,]
med5<-apply(EVI5[,c(1,2,3,4)], 2, FUN = median)
EVI7<-EVI[EVI$Y==7,]
med7<-apply(EVI7[,c(1,2,3,4)], 2, FUN = median)
EVI8<-EVI[EVI$Y==8,]
med8<-apply(EVI8[,c(1,2,3,4)], 2, FUN = median)
EVI9<-EVI[EVI$Y==9,]
med9<-apply(EVI9[,c(1,2,3,4)], 2, FUN = median)

#=
#NDVI
NDVI<-data[,c(42,31,20,9,77,89)]
NDVI1<-NDVI[NDVI$Y==1,]
medNDVI1<-apply(NDVI1[,c(1,2,3,4)], 2, FUN = median)
NDVI2<-NDVI[NDVI$Y==2,]
medNDVI2<-apply(NDVI2[,c(1,2,3,4)], 2, FUN = median)
NDVI3<-NDVI[NDVI$Y==3,]
medNDVI3<-apply(NDVI3[,c(1,2,3,4)], 2, FUN = median)
NDVI4<-NDVI[NDVI$Y==4,]
medNDVI4<-apply(NDVI4[,c(1,2,3,4)], 2, FUN = median)
NDVI5<-NDVI[NDVI$Y==5,]
medNDVI5<-apply(NDVI5[,c(1,2,3,4)], 2, FUN = median)
NDVI7<-NDVI[NDVI$Y==7,]
medNDVI7<-apply(NDVI7[,c(1,2,3,4)], 2, FUN = median)
NDVI8<-NDVI[NDVI$Y==8,]
medNDVI8<-apply(NDVI8[,c(1,2,3,4)], 2, FUN = median)
NDVI9<-NDVI[NDVI$Y==9,]
medNDVI9<-apply(NDVI9[,c(1,2,3,4)], 2, FUN = median)
#NDBI
NDBI<-data[,c(43,32,21,10,77,89)]
NDBI1<-NDBI[NDBI$Y==1,]
medNDBI1<-apply(NDBI1[,c(1,2,3,4)], 2, FUN = median)
NDBI2<-NDBI[NDBI$Y==2,]
medNDBI2<-apply(NDBI2[,c(1,2,3,4)], 2, FUN = median)
NDBI3<-NDBI[NDBI$Y==3,]
medNDBI3<-apply(NDBI3[,c(1,2,3,4)], 2, FUN = median)
NDBI4<-NDBI[NDBI$Y==4,]
medNDBI4<-apply(NDBI4[,c(1,2,3,4)], 2, FUN = median)
NDBI5<-NDBI[NDBI$Y==5,]
medNDBI5<-apply(NDBI5[,c(1,2,3,4)], 2, FUN = median)
NDBI7<-NDBI[NDBI$Y==7,]
medNDBI7<-apply(NDBI7[,c(1,2,3,4)], 2, FUN = median)
NDBI8<-NDBI[NDBI$Y==8,]
medNDBI8<-apply(NDBI8[,c(1,2,3,4)], 2, FUN = median)
NDBI9<-NDBI[NDBI$Y==9,]
medNDBI9<-apply(NDBI9[,c(1,2,3,4)], 2, FUN = median)
#NDWI
NDWI<-data[,c(44,33,22,11,77,89)]
NDWI1<-NDWI[NDWI$Y==1,]
medNDWI1<-apply(NDWI1[,c(1,2,3,4)], 2, FUN = median)
NDWI2<-NDWI[NDWI$Y==2,]
medNDWI2<-apply(NDWI2[,c(1,2,3,4)], 2, FUN = median)
NDWI3<-NDWI[NDWI$Y==3,]
medNDWI3<-apply(NDWI3[,c(1,2,3,4)], 2, FUN = median)
NDWI4<-NDWI[NDWI$Y==4,]
medNDWI4<-apply(NDWI4[,c(1,2,3,4)], 2, FUN = median)
NDWI5<-NDWI[NDWI$Y==5,]
medNDWI5<-apply(NDWI5[,c(1,2,3,4)], 2, FUN = median)
NDWI7<-NDWI[NDWI$Y==7,]
medNDWI7<-apply(NDWI7[,c(1,2,3,4)], 2, FUN = median)
NDWI8<-NDWI[NDWI$Y==8,]
medNDWI8<-apply(NDWI8[,c(1,2,3,4)], 2, FUN = median)
NDWI9<-NDWI[NDWI$Y==9,]
medNDWI9<-apply(NDWI9[,c(1,2,3,4)], 2, FUN = median)


#=======================
#================================================
#UNTUK SEMHAS
par(mfrow=c(2,5))
par(mai=c(0.3,0.3,0.3,0.2))
par(mgp=c(2,1,0))
boxplot(EVI1[,-c(5,6)], main='Vegetatif Awal',border='white',lwd=0.1,
        names = c("t-3","t-2","t-1","t"), ylim=c(-0.7,0.9),
        xlab='periode')
lines(med1, type = "o", col = "green",lwd=2,lty=3)
lines(medNDVI1, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medNDBI1, type = "o", col = "brown",lwd=2,lty=4)
lines(medNDWI1, type = "o", col = "blue",lwd=1)

boxplot(EVI2[,-c(5,6)], main='Vegetatif Akhir',border='white',lwd=0.1,
        names = c("t-3","t-2","t-1","t"), ylim=c(-0.7,0.9),
        xlab='periode')
lines(med2, type = "o", col = "green",lwd=2,lty=3)
lines(medNDVI2, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medNDBI2, type = "o", col = "brown",lwd=2,lty=4)
lines(medNDWI2, type = "o", col = "blue",lwd=1)

boxplot(EVI3[,-c(5,6)], main='Generatif Awal',border='white',lwd=0.1,
        names = c("t-3","t-2","t-1","t"), ylim=c(-0.7,0.9),
        xlab='periode')
lines(med3, type = "o", col = "green",lwd=2,lty=3)
lines(medNDVI3, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medNDBI3, type = "o", col = "brown",lwd=2,lty=4)
lines(medNDWI3, type = "o", col = "blue",lwd=1)

boxplot(EVI9[,-c(5,6)], main='Generatif Akhir',border='white',lwd=0.1,
        names = c("t-3","t-2","t-1","t"), ylim=c(-0.7,0.9),
        xlab='periode')
lines(med9, type = "o", col = "green",lwd=2,lty=3)
lines(medNDVI9, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medNDBI9, type = "o", col = "brown",lwd=2,lty=4)
lines(medNDWI9, type = "o", col = "blue",lwd=1)

boxplot(EVI4[,-c(5,6)], main='Panen',border='white',lwd=0.1,
        names = c("t-3","t-2","t-1","t"), ylim=c(-0.7,0.9),
        xlab='periode')
lines(med4, type = "o", col = "green",lwd=2,lty=3)
lines(medNDVI4, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medNDBI4, type = "o", col = "brown",lwd=2,lty=4)
lines(medNDWI4, type = "o", col = "blue",lwd=1)

boxplot(EVI5[,-c(5,6)], main='Bera/Persiapan',border='white',lwd=0.1,
        names = c("t-3","t-2","t-1","t"), ylim=c(-0.7,0.9),
        xlab='periode')
lines(med5, type = "o", col = "green",lwd=2,lty=3)
lines(medNDVI5, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medNDBI5, type = "o", col = "brown",lwd=2,lty=4)
lines(medNDWI5, type = "o", col = "blue",lwd=1)

boxplot(EVI7[,-c(5,6)], main='Sawah Bkn Padi',border='white',lwd=0.1,
        names = c("t-3","t-2","t-1","t"), ylim=c(-0.7,0.9),
        xlab='periode')
lines(med7, type = "o", col = "green",lwd=2,lty=3)
lines(medNDVI7, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medNDBI7, type = "o", col = "brown",lwd=2,lty=4)
lines(medNDWI7, type = "o", col = "blue",lwd=1)

boxplot(EVI8[,-c(5,6)], main='Bukan Sawah',border='white',lwd=0.1,
        names = c("t-3","t-2","t-1","t"), ylim=c(-0.7,0.9),
        xlab='periode')
lines(med8, type = "o", col = "green",lwd=2,lty=3)
lines(medNDVI8, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medNDBI8, type = "o", col = "brown",lwd=2,lty=4)
lines(medNDWI8, type = "o", col = "blue",lwd=1)

boxplot(EVI8[,-c(5,6)],border='white',col='white', xlab='', ylab='',
        axes=F, ylim=c(0,1.5))
segments(0.1,1.3,2.2,1.3, lty=2, lwd=2, col='darkgreen')
segments(0.1,0.9,2.2,0.9, lty=3, lwd=2, col='green')
segments(0.1,0.5,2.2,0.5, lty=4, lwd=2, col='brown')
segments(0.1,0.1,2.2,0.1, lty=1, lwd=1, col='blue')
text(2.3,1.3, pos=4, 'NDVI', cex=1.2)
text(2.3,0.9, pos=4, 'EVI', cex=1.2)
text(2.3,0.5, pos=4, 'NDBI', cex=1.2)
text(2.3,0.1, pos=4, 'NDWI', cex=1.2)

#------------
#UNTUK SEMHAS BAND
#Band1
Band1<-data[,c(1,12,23,34,77,89)]
Band11<-Band1[Band1$Y==1,]
medBand11<-apply(Band11[,c(1,2,3,4)], 2, FUN = median)
Band12<-Band1[Band1$Y==2,]
medBand12<-apply(Band12[,c(1,2,3,4)], 2, FUN = median)
Band13<-Band1[Band1$Y==3,]
medBand13<-apply(Band13[,c(1,2,3,4)], 2, FUN = median)
Band14<-Band1[Band1$Y==4,]
medBand14<-apply(Band14[,c(1,2,3,4)], 2, FUN = median)
Band15<-Band1[Band1$Y==5,]
medBand15<-apply(Band15[,c(1,2,3,4)], 2, FUN = median)
Band17<-Band1[Band1$Y==7,]
medBand17<-apply(Band17[,c(1,2,3,4)], 2, FUN = median)
Band18<-Band1[Band1$Y==8,]
medBand18<-apply(Band18[,c(1,2,3,4)], 2, FUN = median)
Band19<-Band1[Band1$Y==9,]
medBand19<-apply(Band19[,c(1,2,3,4)], 2, FUN = median)

Band2<-data[,c(2,13,24,35,77,89)]
Band21<-Band2[Band2$Y==1,]
medBand21<-apply(Band21[,c(1,2,3,4)], 2, FUN = median)
Band22<-Band2[Band2$Y==2,]
medBand22<-apply(Band22[,c(1,2,3,4)], 2, FUN = median)
Band23<-Band2[Band2$Y==3,]
medBand23<-apply(Band23[,c(1,2,3,4)], 2, FUN = median)
Band24<-Band2[Band2$Y==4,]
medBand24<-apply(Band24[,c(1,2,3,4)], 2, FUN = median)
Band25<-Band2[Band2$Y==5,]
medBand25<-apply(Band25[,c(1,2,3,4)], 2, FUN = median)
Band27<-Band2[Band2$Y==7,]
medBand27<-apply(Band27[,c(1,2,3,4)], 2, FUN = median)
Band28<-Band2[Band2$Y==8,]
medBand28<-apply(Band28[,c(1,2,3,4)], 2, FUN = median)
Band29<-Band2[Band2$Y==9,]
medBand29<-apply(Band29[,c(1,2,3,4)], 2, FUN = median)

Band3<-data[,c(3,14,25,36,77,89)]
Band31<-Band3[Band3$Y==1,]
medBand31<-apply(Band31[,c(1,2,3,4)], 2, FUN = median)
Band32<-Band3[Band3$Y==2,]
medBand32<-apply(Band32[,c(1,2,3,4)], 2, FUN = median)
Band33<-Band3[Band3$Y==3,]
medBand33<-apply(Band33[,c(1,2,3,4)], 2, FUN = median)
Band34<-Band3[Band3$Y==4,]
medBand34<-apply(Band34[,c(1,2,3,4)], 2, FUN = median)
Band35<-Band3[Band3$Y==5,]
medBand35<-apply(Band35[,c(1,2,3,4)], 2, FUN = median)
Band37<-Band3[Band3$Y==7,]
medBand37<-apply(Band37[,c(1,2,3,4)], 2, FUN = median)
Band38<-Band3[Band3$Y==8,]
medBand38<-apply(Band38[,c(1,2,3,4)], 2, FUN = median)
Band39<-Band3[Band3$Y==9,]
medBand39<-apply(Band39[,c(1,2,3,4)], 2, FUN = median)

Band4<-data[,c(4,15,26,37,77,89)]
Band41<-Band4[Band4$Y==1,]
medBand41<-apply(Band41[,c(1,2,3,4)], 2, FUN = median)
Band42<-Band4[Band4$Y==2,]
medBand42<-apply(Band42[,c(1,2,3,4)], 2, FUN = median)
Band43<-Band4[Band4$Y==3,]
medBand43<-apply(Band43[,c(1,2,3,4)], 2, FUN = median)
Band44<-Band4[Band4$Y==4,]
medBand44<-apply(Band44[,c(1,2,3,4)], 2, FUN = median)
Band45<-Band4[Band4$Y==5,]
medBand45<-apply(Band45[,c(1,2,3,4)], 2, FUN = median)
Band47<-Band4[Band4$Y==7,]
medBand47<-apply(Band47[,c(1,2,3,4)], 2, FUN = median)
Band48<-Band4[Band4$Y==8,]
medBand48<-apply(Band48[,c(1,2,3,4)], 2, FUN = median)
Band49<-Band4[Band4$Y==9,]
medBand49<-apply(Band49[,c(1,2,3,4)], 2, FUN = median)

Band5<-data[,c(5,16,27,38,77,89)]
Band51<-Band5[Band5$Y==1,]
medBand51<-apply(Band51[,c(1,2,3,4)], 2, FUN = median)
Band52<-Band5[Band5$Y==2,]
medBand52<-apply(Band52[,c(1,2,3,4)], 2, FUN = median)
Band53<-Band5[Band5$Y==3,]
medBand53<-apply(Band53[,c(1,2,3,4)], 2, FUN = median)
Band54<-Band5[Band5$Y==4,]
medBand54<-apply(Band54[,c(1,2,3,4)], 2, FUN = median)
Band55<-Band5[Band5$Y==5,]
medBand55<-apply(Band55[,c(1,2,3,4)], 2, FUN = median)
Band57<-Band5[Band5$Y==7,]
medBand57<-apply(Band57[,c(1,2,3,4)], 2, FUN = median)
Band58<-Band5[Band5$Y==8,]
medBand58<-apply(Band58[,c(1,2,3,4)], 2, FUN = median)
Band59<-Band5[Band5$Y==9,]
medBand59<-apply(Band59[,c(1,2,3,4)], 2, FUN = median)

Band6<-data[,c(6,17,28,39,77,89)]
Band61<-Band6[Band6$Y==1,]
medBand61<-apply(Band61[,c(1,2,3,4)], 2, FUN = median)
Band62<-Band6[Band6$Y==2,]
medBand62<-apply(Band62[,c(1,2,3,4)], 2, FUN = median)
Band63<-Band6[Band6$Y==3,]
medBand63<-apply(Band63[,c(1,2,3,4)], 2, FUN = median)
Band64<-Band6[Band6$Y==4,]
medBand64<-apply(Band64[,c(1,2,3,4)], 2, FUN = median)
Band65<-Band6[Band6$Y==5,]
medBand65<-apply(Band65[,c(1,2,3,4)], 2, FUN = median)
Band67<-Band6[Band6$Y==7,]
medBand67<-apply(Band67[,c(1,2,3,4)], 2, FUN = median)
Band68<-Band6[Band6$Y==8,]
medBand68<-apply(Band68[,c(1,2,3,4)], 2, FUN = median)
Band69<-Band6[Band6$Y==9,]
medBand69<-apply(Band69[,c(1,2,3,4)], 2, FUN = median)

Band7<-data[,c(7,18,29,40,77,89)]
Band71<-Band7[Band7$Y==1,]
medBand71<-apply(Band71[,c(1,2,3,4)], 2, FUN = median)
Band72<-Band7[Band7$Y==2,]
medBand72<-apply(Band72[,c(1,2,3,4)], 2, FUN = median)
Band73<-Band7[Band7$Y==3,]
medBand73<-apply(Band73[,c(1,2,3,4)], 2, FUN = median)
Band74<-Band7[Band7$Y==4,]
medBand74<-apply(Band74[,c(1,2,3,4)], 2, FUN = median)
Band75<-Band7[Band7$Y==5,]
medBand75<-apply(Band75[,c(1,2,3,4)], 2, FUN = median)
Band77<-Band7[Band7$Y==7,]
medBand77<-apply(Band77[,c(1,2,3,4)], 2, FUN = median)
Band78<-Band7[Band7$Y==8,]
medBand78<-apply(Band78[,c(1,2,3,4)], 2, FUN = median)
Band79<-Band7[Band7$Y==9,]
medBand79<-apply(Band79[,c(1,2,3,4)], 2, FUN = median)

#--------------------
par(mfrow=c(3,3))
par(mai=c(0.5,0.3,0.3,0.2))
par(mgp=c(2,1,0))
boxplot(Band11[,-c(5,6)], main='Vegetatif Awal',border='white',lwd=0.1,
        names = c("t", "t-1", "t-2", "t-3"), ylim=c(-0.1,0.6),
        xlab='periode')
lines(medBand11, type = "o", col = "green",lwd=2,lty=3)
lines(medBand21, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medBand31, type = "o", col = "brown",lwd=2,lty=4)
lines(medBand41, type = "o", col = "blue",lwd=1)
lines(medBand51, type = "o", col = "red",lwd=2,lty=3)
lines(medBand61, type = "o", col = "purple",lwd=2,lty=2)
lines(medBand71, type = "o", col = "orange",lwd=2,lty=4)


boxplot(Band12[,-c(5,6)], main='Vegetatif Akhir',border='white',lwd=0.1,
        names = c("t", "t-1", "t-2", "t-3"), ylim=c(-0.1,0.6),
        xlab='periode')
lines(medBand12, type = "o", col = "green",lwd=2,lty=3)
lines(medBand22, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medBand32, type = "o", col = "brown",lwd=2,lty=4)
lines(medBand42, type = "o", col = "blue",lwd=1)
lines(medBand52, type = "o", col = "red",lwd=2,lty=3)
lines(medBand62, type = "o", col = "purple",lwd=2,lty=2)
lines(medBand72, type = "o", col = "orange",lwd=2,lty=4)


boxplot(Band13[,-c(5,6)], main='Generatif Awal',border='white',lwd=0.1,
        names = c("t", "t-1", "t-2", "t-3"), ylim=c(-0.1,0.6),
        xlab='periode')
lines(medBand13, type = "o", col = "green",lwd=2,lty=3)
lines(medBand23, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medBand33, type = "o", col = "brown",lwd=2,lty=4)
lines(medBand43, type = "o", col = "blue",lwd=1)
lines(medBand53, type = "o", col = "red",lwd=2,lty=3)
lines(medBand63, type = "o", col = "purple",lwd=2,lty=2)
lines(medBand73, type = "o", col = "orange",lwd=2,lty=4)


boxplot(Band14[,-c(5,6)], main='Panen',border='white',lwd=0.1,
        names = c("t", "t-1", "t-2", "t-3"), ylim=c(-0.1,0.6),
        xlab='periode')
lines(medBand14, type = "o", col = "green",lwd=2,lty=3)
lines(medBand24, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medBand34, type = "o", col = "brown",lwd=2,lty=4)
lines(medBand44, type = "o", col = "blue",lwd=1)
lines(medBand54, type = "o", col = "red",lwd=2,lty=3)
lines(medBand64, type = "o", col = "purple",lwd=2,lty=2)
lines(medBand74, type = "o", col = "orange",lwd=2,lty=4)


boxplot(Band15[,-c(5,6)], main='Bera/Persiapan Lahan',border='white',lwd=0.1,
        names = c("t", "t-1", "t-2", "t-3"), ylim=c(-0.1,0.6),
        xlab='periode')
lines(medBand15, type = "o", col = "green",lwd=2,lty=3)
lines(medBand25, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medBand35, type = "o", col = "brown",lwd=2,lty=4)
lines(medBand45, type = "o", col = "blue",lwd=1)
lines(medBand55, type = "o", col = "red",lwd=2,lty=3)
lines(medBand65, type = "o", col = "purple",lwd=2,lty=2)
lines(medBand75, type = "o", col = "orange",lwd=2,lty=4)


boxplot(Band17[,-c(5,6)], main='Sawah Bukan Padi',border='white',lwd=0.1,
        names = c("t", "t-1", "t-2", "t-3"), ylim=c(-0.1,0.6),
        xlab='periode')
lines(medBand17, type = "o", col = "green",lwd=2,lty=3)
lines(medBand27, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medBand37, type = "o", col = "brown",lwd=2,lty=4)
lines(medBand47, type = "o", col = "blue",lwd=1)
lines(medBand57, type = "o", col = "red",lwd=2,lty=3)
lines(medBand67, type = "o", col = "purple",lwd=2,lty=2)
lines(medBand77, type = "o", col = "orange",lwd=2,lty=4)


boxplot(Band18[,-c(5,6)], main='Bukan Sawah',border='white',lwd=0.1,
        names = c("t", "t-1", "t-2", "t-3"), ylim=c(-0.1,0.6),
        xlab='periode')
lines(medBand18, type = "o", col = "green",lwd=2,lty=3)
lines(medBand28, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medBand38, type = "o", col = "brown",lwd=2,lty=4)
lines(medBand48, type = "o", col = "blue",lwd=1)
lines(medBand58, type = "o", col = "red",lwd=2,lty=3)
lines(medBand68, type = "o", col = "purple",lwd=2,lty=2)
lines(medBand78, type = "o", col = "orange",lwd=2,lty=4)

boxplot(Band19[,-c(5,6)], main='Generatif Akhir',border='white',lwd=0.1,
        names = c("t", "t-1", "t-2", "t-3"), ylim=c(-0.1,0.6),
        xlab='periode')
lines(medBand19, type = "o", col = "green",lwd=2,lty=3)
lines(medBand29, type = "o", col = "darkgreen",lwd=2,lty=2)
lines(medBand39, type = "o", col = "brown",lwd=2,lty=4)
lines(medBand49, type = "o", col = "blue",lwd=1)
lines(medBand59, type = "o", col = "red",lwd=2,lty=3)
lines(medBand69, type = "o", col = "purple",lwd=2,lty=2)
lines(medBand79, type = "o", col = "orange",lwd=2,lty=4)


boxplot(Band19[,-c(5,6)],border='white',col='white', xlab='', ylab='',
        axes=F, ylim=c(0,1))
segments(1,0.8,2,0.8, lty=2, lwd=2, col='darkgreen')
segments(1,0.6,2,0.6, lty=3, lwd=2, col='green')
segments(1,0.4,2,0.4, lty=4, lwd=2, col='brown')
segments(1,0.2,2,0.2, lty=1, lwd=1, col='blue')
text(2.3,0.8, pos=4, 'NDVI', cex=1.2)
text(2.3,0.6, pos=4, 'EVI', cex=1.2)
text(2.3,0.4, pos=4, 'NDBI', cex=1.2)
text(2.3,0.2, pos=4, 'NDWI', cex=1.2)

#===============
par(mfrow=c(3,3))
par(mai=c(0.5,0.3,0.3,0.2))
boxplot(Band11[,1:4], main='band 1 kelas 1',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band12[,1:4], main='band 1 kelas 2',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band13[,1:4], main='band 1 kelas 3',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band19[,1:4], main='band 1 kelas 9',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band14[,1:4], main='band 1 kelas 4',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band15[,1:4], main='band 1 kelas 5',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band17[,1:4], main='band 1 kelas 7',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band18[,1:4], main='band 1 kelas 8',names = c("t", "t-1", "t-2", "t-3"))

par(mfrow=c(3,3))
par(mai=c(0.5,0.3,0.3,0.2))
boxplot(Band21[,1:4], main='band 2 kelas 1',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band22[,1:4], main='band 2 kelas 2',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band23[,1:4], main='band 2 kelas 3',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band29[,1:4], main='band 2 kelas 9',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band24[,1:4], main='band 2 kelas 4',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band25[,1:4], main='band 2 kelas 5',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band27[,1:4], main='band 2 kelas 7',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band28[,1:4], main='band 2 kelas 8',names = c("t", "t-1", "t-2", "t-3"))

par(mfrow=c(3,3))
par(mai=c(0.5,0.3,0.3,0.2))
boxplot(Band31[,1:4], main='band 3 kelas 1',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band32[,1:4], main='band 3 kelas 2',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band33[,1:4], main='band 3 kelas 3',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band39[,1:4], main='band 3 kelas 9',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band34[,1:4], main='band 3 kelas 4',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band35[,1:4], main='band 3 kelas 5',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band37[,1:4], main='band 3 kelas 7',names = c("t", "t-1", "t-2", "t-3"))
boxplot(Band38[,1:4], main='band 3 kelas 8',names = c("t", "t-1", "t-2", "t-3"))
