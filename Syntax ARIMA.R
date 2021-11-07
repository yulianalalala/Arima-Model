install.packages("tseries")
library(tseries)
install.packages("forecast")
library(forecast)
mydata<- read.csv("D:/data_ujian.csv",header=T, sep=";")
attach(mydata)
mydata
Y<-ts(mydata[,2],start = c(2018,1), frequency=30)
plot(Y, xlab="Bulan", ylab="Jumlah Permintaan")
install.packages("MASS")
library(MASS)
transformasi=BoxCox.lambda(Y)
transformasi
adf.test(Y)
diff1=diff(Y)
adf.test(diff1)
diff2=diff(diff1)
adf.test(diff2)
acf(diff2)
pacf(diff2)
forecast1 <- arima(Y, order=c(1,2,1))
forecast1
forecast2 <- arima(Y, order=c(1,2,0)) 
forecast2
forecast3<- arima(Y, order=c(0,2,1)) 
forecast3
forecast4<- arima(Y, order=c(2,2,1)) 
forecast4
forecast5<- arima(Y, order=c(2,2,0))
forecast5
forecast6<- arima(Y, order=c(1,2,2)) 
forecast6
forecast7<- arima(Y, order=c(0,2,2)) 
forecast7
forecast8<- arima(Y, order=c(2,2,2))
forecast8

##Uji signifikansi
printstatarima<-function (Y,digits=4, se=T,...){
  if(length(Y$coef)>0){
    cat("\nCoefficients:\n")
    coef<-round(Y$coef, digits=digits)
    if(se&&nrow(Y$var.coef)){
      ses<-rep(0,length(coef))
      ses[Y$mask]<-round(sqrt(diag(Y$var.coef)), digits=digits)
      coef<-matrix(coef,1,dimnames=list(NULL,names(coef)))
      coef<-rbind(coef,s.e.=ses)
      statt<-coef[1,]/ses
      pval<-2*pt(abs(statt),df=length(Y$residuals)-1,lower.tail=F)
      coef<-rbind(coef, t=round(statt, digits=digits),sign.=round(pval,digits=digits))
      coef<-t(coef)
    }
    print.default(coef,print.gap = 2)
  }
}
printstatarima
printstatarima(forecast1)
printstatarima(forecast2)
printstatarima(forecast3)
printstatarima(forecast4)
printstatarima(forecast5)
printstatarima(forecast6)
printstatarima(forecast7)
printstatarima(forecast8)

##normalitas
r1 = residuals(forecast1)
n1=length(r1)
n1
mean1=mean(r1)
sd1=sd(r1)
res1=rnorm(n1,mean1,sd1)
hasil1=ks.test(r1,res1)
hasil1

r5 = residuals(forecast5)
n5=length(r5)
n5
mean5=mean(r5)
sd5=sd(r5)
res5=rnorm(n5,mean5,sd5)
hasil5=ks.test(r5,res5)
hasil5

##Uji White Noise
wn1=Box.test(r1,lag=1, type=c("Ljung-Box"))
wn1

wn5=Box.test(r5,lag=1, type=c("Ljung-Box"))
wn5

Prediksi=forecast(forecast5,h=3)
Prediksi
plot(Prediksi)
plot(Prediksi, main = "Permintaan PT. Pertama Putra",col.main = "darkgreen")
autoplot(forecast(Prediksi))

futurVal <- forecast.Arima(Prediksi,h=3, level=c(99.5))
plot.forecast(Prediksi)
install.packages("FitAR")
library(FitAR)

----------------------------------------------------------------------------

acf(diff1)
pacf(diff1)
forecast1 <- arima(Y, order=c(1,1,2))
forecast1
forecast2 <- arima(Y, order=c(0,1,2))
forecast2
forecast3<- arima(Y, order=c(1,1,1))
forecast3
forecast4<- arima(Y, order=c(0,1,1))
forecast4
forecast5<- arima(Y, order=c(1,1,0))
forecast5
forecast6<- arima(Y, order=c(1,2,2)) # fit an ARIMA(0,1,1) model
forecast6
forecast7<- arima(Y, order=c(0,2,2)) # fit an ARIMA(0,1,1) model
forecast7
forecast8<- arima(Y, order=c(2,2,2)) # fit an ARIMA(0,1,1) model
forecast8

##Uji signifikansi
printstatarima<-function (Y,digits=4, se=T,...){
  if(length(Y$coef)>0){
    cat("\nCoefficients:\n")
    coef<-round(Y$coef, digits=digits)
    if(se&&nrow(Y$var.coef)){
      ses<-rep(0,length(coef))
      ses[Y$mask]<-round(sqrt(diag(Y$var.coef)), digits=digits)
      coef<-matrix(coef,1,dimnames=list(NULL,names(coef)))
      coef<-rbind(coef,s.e.=ses)
      statt<-coef[1,]/ses
      pval<-2*pt(abs(statt),df=length(Y$residuals)-1,lower.tail=F)
      coef<-rbind(coef, t=round(statt, digits=digits),sign.=round(pval,digits=digits))
      coef<-t(coef)
    }
    print.default(coef,print.gap = 2)
  }
}
printstatarima
printstatarima(forecast1)
printstatarima(forecast2)
printstatarima(forecast3)
printstatarima(forecast4)
printstatarima(forecast5)
printstatarima(forecast6)
printstatarima(forecast7)
printstatarima(forecast8)

##normalitas
r4 = residuals(forecast4)
n4=length(r4)
n4
mean4=mean(r4)
sd4=sd(r4)
res4=rnorm(n4,mean4,sd4)
hasil4=ks.test(r4,res4)
hasil4


##Uji White Noise
wn5=Box.test(r5,lag=1, type=c("Ljung-Box"))
wn5

wn4=Box.test(r4,lag=1, type=c("Ljung-Box"))
wn4

Prediksi=forecast(forecast5,h=3)
Prediksi
autoplot(forecast(Prediksi))

futurVal <- forecast.Arima(Prediksi,h=3, level=c(99.5))
plot.forecast(Prediksi)
install.packages("FitAR")
library(FitAR)

