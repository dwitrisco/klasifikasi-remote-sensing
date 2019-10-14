library(raster)
library(rgdal)
library(rgeos)
library(PBSmapping)
library(sp)

setwd('D:/2. bahan thesis/landsat8')

#------------------------
#rstart memori
#rm(list=ls())
#rs.restartR()
#---------------

#-------------------------------------------------------------------
#-------------------------------------------------------------------
#potong raster
e <- extent(160000, 220000, -960000, -860000)

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#opern SHP
bwi_sampel<-readOGR('D:/2. bahan thesis/6. data excel/buat shp segmen', layer='bwi_sampel' )
bwi<-as.data.frame(bwi_sampel)
extent(bwi_sampel)


#--------------------------------------------------------------------
#--------------------------------------------------------------------
#open data
setwd('D:/2. bahan thesis/landsat8/image/banyuwangi')

sep027=stack('LC08_117066_20170902.Tif')
sep187=stack('LC08_117066_20170918.Tif')
okt047=stack('LC08_117066_20171004.Tif')
okt207=stack("LC08_117066_20171020.Tif")
nov057=stack('LC08_117066_20171105.Tif')
des077=stack('LC08_117066_20171207.Tif')
des237=stack('LC08_117066_20171223.Tif')
jan088=stack('LC08_117066_20180108.Tif')

feb098=stack('LC08_117066_20180209.Tif')
feb258=stack('LC08_117066_20180225.Tif')
mar138=stack('LC08_117066_20180313.Tif')
mar298=stack("LC08_117066_20180329.Tif")
apr148=stack('LC08_117066_20180414.Tif')
apr308=stack('LC08_117066_20180430.Tif')

mei168=stack('LC08_117066_20180516.Tif')
jun018=stack('LC08_117066_20180601.Tif')
jun178=stack("LC08_117066_20180617.Tif")
jul038=stack('LC08_117066_20180703.Tif')
ags048=stack('LC08_117066_20180804.Tif')
ags208=stack('LC08_117066_20180820.Tif')

sep058=stack('LC08_117066_20180905.Tif')
sep218=stack("LC08_117066_20180921.Tif")
okt078=stack('LC08_117066_20181007.Tif')
okt238=stack('LC08_117066_20181023.Tif')
nov088=stack('LC08_117066_20181108.Tif')
nov248=stack('LC08_117066_20181124.Tif')
des108=stack("LC08_117066_20181210.Tif")
des268=stack('LC08_117066_20181226.Tif')
jan119=stack('LC08_117066_20190111.Tif')
jan279=stack('LC08_117066_20190127.Tif')

#POTONG
esep027=crop(sep027,e)
esep187=crop(sep187,e)
eokt047=crop(okt047,e)
eokt207=crop(okt207,e)
enov057=crop(nov057,e)
edes077=crop(des077,e)
edes237=crop(des237,e)
ejan088=crop(jan088,e)


efeb098=crop(feb098,e)
efeb258=crop(feb258,e)
emar138=crop(mar138,e)
emar298=crop(mar298,e)
eapr148=crop(apr148,e)
eapr308=crop(apr308,e)

emei168=crop(mei168,e)
ejun018=crop(jun018,e)
ejun178=crop(jun178,e)
ejul038=crop(jul038,e)
eags048=crop(ags048,e)
eags208=crop(ags208,e)

esep058=crop(sep058,e)
esep218=crop(sep218,e)
eokt078=crop(okt078,e)
eokt238=crop(okt238,e)
enov088=crop(nov088,e)
enov248=crop(nov248,e)
edes108=crop(des108,e)
edes268=crop(des268,e)
ejan119=crop(jan119,e)
ejan279=crop(jan279,e)


#----------------------------------------------------------------
#-------------------------------------------------------------------
#ekstract
e.sep027<-extract(esep027,bwi_sampel, df=TRUE)
df.e.sep027<-as.data.frame(e.sep027)
write.csv(df.e.sep027, 'D:/2. bahan thesis/landsat8/e.sep027.csv')

e.sep187=extract(esep187,bwi_sampel, df=TRUE)
df.e.sep187<-as.data.frame(e.sep187)
write.csv(df.e.sep187, 'D:/2. bahan thesis/landsat8/e.sep187.csv')

e.okt047=extract(eokt047,bwi_sampel, df=TRUE)
df.e.okt047<-as.data.frame(e.okt047)
write.csv(df.e.okt047, 'D:/2. bahan thesis/landsat8/e.okt047.csv')

e.okt207=extract(eokt207,bwi_sampel, df=TRUE)
df.e.okt207<-as.data.frame(e.okt207)
write.csv(df.e.okt207, 'D:/2. bahan thesis/landsat8/e.okt207.csv')

e.nov057=extract(enov057,bwi_sampel, df=TRUE)
df.e.nov057<-as.data.frame(e.nov057)
write.csv(df.e.nov057, 'D:/2. bahan thesis/landsat8/e.nov057.csv')

e.des077=extract(edes077,bwi_sampel, df=TRUE)
df.e.des077<-as.data.frame(e.des077)
write.csv(df.e.des077, 'D:/2. bahan thesis/landsat8/e.des077.csv')

e.des237=extract(edes237,bwi_sampel, df=TRUE)
df.e.des237<-as.data.frame(e.des237)
write.csv(df.e.des237, 'D:/2. bahan thesis/landsat8/e.des237.csv')

e.jan088=extract(ejan088,bwi_sampel, df=TRUE)
df.e.jan088<-as.data.frame(e.jan088)
write.csv(df.e.jan088, 'D:/2. bahan thesis/landsat8/e.jan088.csv')

e.feb098=extract(efeb098,bwi_sampel, df=TRUE)
e.feb258=extract(efeb258,bwi_sampel, df=TRUE)
e.mar138=extract(emar138,bwi_sampel, df=TRUE)
e.mar298=extract(emar298,bwi_sampel, df=TRUE)

e.apr148=extract(eapr148,bwi_sampel, df=TRUE)
df.e.apr148<-as.data.frame(e.apr148)
write.csv(df.e.apr148, 'D:/2. bahan thesis/landsat8/e.apr148.csv')

e.apr308=extract(eapr308,bwi_sampel, df=TRUE)
df.e.apr308<-as.data.frame(e.apr308)
write.csv(df.e.apr308, 'D:/2. bahan thesis/landsat8/e.apr308.csv')

e.mei168=extract(emei168,bwi_sampel, df=TRUE)
df.e.mei168<-as.data.frame(e.mei168)
write.csv(df.e.mei168, 'D:/2. bahan thesis/landsat8/e.mei168.csv')

e.jun018=extract(ejun018,bwi_sampel, df=TRUE)
df.e.jun018<-as.data.frame(e.jun018)
write.csv(df.e.jun018, 'D:/2. bahan thesis/landsat8/e.jun018.csv')

e.jun178=extract(ejun178,bwi_sampel, df=TRUE)
df.e.jun178<-as.data.frame(e.jun178)
write.csv(df.e.jun178, 'D:/2. bahan thesis/landsat8/e.jun178.csv')

e.jul038=extract(ejul038,bwi_sampel, df=TRUE)
df.e.jul038<-as.data.frame(e.jul038)
write.csv(df.e.jul038, 'D:/2. bahan thesis/landsat8/e.jul038.csv')

e.ags048=extract(eags048,bwi_sampel, df=TRUE)
df.e.ags048<-as.data.frame(e.ags048)
write.csv(df.e.ags048, 'D:/2. bahan thesis/landsat8/e.ags048.csv')

e.ags208=extract(eags208,bwi_sampel, df=TRUE)
df.e.ags208<-as.data.frame(e.ags208)
write.csv(df.e.ags208, 'D:/2. bahan thesis/landsat8/e.ags208.csv')


e.sep058=extract(esep058,bwi_sampel, df=TRUE)
df.e.sep058<-as.data.frame(e.sep058)
write.csv(df.e.sep058, 'D:/2. bahan thesis/landsat8/e.sep058.csv')

e.sep218=extract(esep218,bwi_sampel, df=TRUE)
df.e.sep218<-as.data.frame(e.sep218)
write.csv(df.e.sep218, 'D:/2. bahan thesis/landsat8/e.sep218.csv')

e.okt078=extract(eokt078,bwi_sampel, df=TRUE)
df.e.okt078<-as.data.frame(e.okt078)
write.csv(df.e.okt078, 'D:/2. bahan thesis/landsat8/e.okt078.csv')

e.okt238=extract(eokt238,bwi_sampel, df=TRUE)
df.e.okt238<-as.data.frame(e.okt238)
write.csv(df.e.okt238, 'D:/2. bahan thesis/landsat8/e.okt238.csv')

e.nov088=extract(enov088,bwi_sampel, df=TRUE)
df.e.nov088<-as.data.frame(e.nov088)
write.csv(df.e.nov088, 'D:/2. bahan thesis/landsat8/e.nov088.csv')

e.nov248=extract(enov248,bwi_sampel, df=TRUE)
df.e.nov248<-as.data.frame(e.nov248)
write.csv(df.e.nov248, 'D:/2. bahan thesis/landsat8/e.nov248.csv')

e.des108=extract(edes108,bwi_sampel, df=TRUE)
df.e.des108<-as.data.frame(e.des108)
write.csv(df.e.des108, 'D:/2. bahan thesis/landsat8/e.des108.csv')

e.des268=extract(edes268,bwi_sampel, df=TRUE)
df.e.des268<-as.data.frame(e.des268)
write.csv(df.e.des268, 'D:/2. bahan thesis/landsat8/e.des268.csv')

e.jan119=extract(ejan119,bwi_sampel, df=TRUE)
df.e.jan119<-as.data.frame(e.jan119)
write.csv(df.e.jan119, 'D:/2. bahan thesis/landsat8/e.jan119.csv')

e.jan279=extract(ejan279,bwi_sampel, df=TRUE)
df.e.jan279<-as.data.frame(e.jan279)
write.csv(df.e.jan279, 'D:/2. bahan thesis/landsat8/e.jan279.csv')

#------------------------
#rstart memori
#rm(list=ls())
#rs.restartR()
#---------------

