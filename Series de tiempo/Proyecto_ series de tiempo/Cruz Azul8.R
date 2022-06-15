#############SERIES DE TIEMPO#############
##"Modelo de series de tiempo. ##
##Para el subcampeonato del Cruz Azul en la Liga MX"##

##Integrantes del equipo:
###Camargo Salas Mario Alberto
###García Sánchez Cecilia Daniela 
###Ibarra Guerrero Javier Alonso 
###Rodríguez Becerril Kenia 
###Rojas Gutiérrez Rodolfo Emmanuel 
###Tirado Pellón Diana Karina 

rm(list = ls())
##Librerías 
##install.packages("forecast")
library(forecast)
library(png)
library(tseries)
library(readxl)
library(lmtest)
library(stats4)
library(strucchange)
library(FinTS)
library(latex2exp)
library(cumstats) 
library(dplyr)
library(nnet)

CA<-read.csv(file.choose(),  colClasses = c("character", "character", rep("numeric", 9),"factor"))
CA<-CA[1:48,]
ca<-tbl_df(CA)
Ca<-mutate(ca,Logro = factor(Logro,levels = levels(Logro), 
                             labels = c(1,4,5,3,2)))
Ca<-select(Ca,c(JG,JE,JP,DIF,Logro))
cf<-mutate(Ca,Logro = relevel(Logro, ref = 5))

rm(Ca,ca)
##install.packages("rugarch")

Fn <- function(x, muestra) sapply(x, function(z) mean(muestra <= z))
Fn.Bayes <- function(x, muestra) qbeta(1/2, 1+length(muestra)*Fn(x,muestra), 1+length(muestra)*(1-Fn(x,muestra)))
Binf <- function(x, muestra, alfa) qbeta(alfa/2, 1+length(muestra)*Fn(x,muestra), 1+length(muestra)*(1-Fn(x,muestra)))
Bsup <- function(x, muestra, alfa) qbeta(1-alfa/2, 1+length(muestra)*Fn(x,muestra), 1+length(muestra)*(1-Fn(x,muestra)))


x <- seq(min(CA$DIF) - sd(CA$DIF), max(CA$DIF) + sd(CA$DIF), length.out = 10000)

plot(x, Fn(x, CA$DIF), pch = 20, ylab = "F(x)", xlab = "Diferencia de goles", col = colors()[566], main = "Función de distribución empírica de las diferencias de goles")

lines(x, Fn.Bayes(x, CA$DIF), col = colors()[121], lwd = 2)
lines(x, Binf(x, CA$DIF, 0.05), col = colors()[333], lwd = 2)
lines(x, Bsup(x, CA$DIF, 0.05), col = colors()[333], lwd = 2)

axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd = 2)



x <- seq(min(CA$PTS) - sd(CA$PTS), max(CA$PTS) + sd(CA$PTS), length.out = 10000)

plot(x, Fn(x, CA$PTS), pch = 20, ylab = "F(x)", xlab = "Puntos obtenidos", col = colors()[566], lwd = .3, main = "Función de distribución empírica de los puntos obtenidos por el Cruz Azul")

lines(x, Fn.Bayes(x, CA$PTS), col = colors()[121], lwd = 2)
lines(x, Binf(x, CA$PTS, 0.05), col = colors()[333], lwd = 2)
lines(x, Bsup(x, CA$PTS, 0.05), col = colors()[333], lwd = 2)

axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd = 2)


##Modelo multinomial
model <- multinom(Logro ~ .-1, data=cf)

A<- as.matrix(summary(model)$coeff)
A

cf<-select(cf,JG:DIF)
cf<-mutate(cf,uno = rep(1,nrow(cf)))
cf<-as.matrix(select(cf,c(JG,JE,JP,DIF)))
dim(cf) 

A%*%cf[1,]

p.cruz<-sapply(1:nrow(cf),function(x) (exp(A[1,]%*%cf[x,]) + 1)/(1+sum(exp(A%*%cf[x,]))))


ts <- ts(rev(p.cruz),start = c(1996,1), frequency = 2)


par(mfrow = c(1,1)) 
plot.ts(ts,main="Probabilidad de ser al menos subcampeón \nCruz Azul",lwd = 2,xlab = "Tiempo",
        ylab = TeX("$\\P_{t}$"),col = "#2E64FE")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)
lines(ts(cummean(ts),start = c(1996,1),frequency = 2),col = "red"
      ,lty = "dotdash",lwd = 2)
legend("topright",TeX("$\\bar{P_{t}}$"),lty = "dotdash",col = "red"
       ,lwd = 2)


par(mfrow=c(1,1))

##Un vistazo a nuestra serie 

par(mfrow=c(1,3))
plot.ts(ts,main="Probabilidad de ser al menos subcampeón \nCruz Azul",lwd = 2,xlab = "Tiempo",
        ylab = TeX("$\\P_{t}$"),col = "#2E64FE")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)
lines(ts(cummean(ts),start = c(1996,1),frequency = 2),col = "red"
      ,lty = "dotdash",lwd = 2)
legend("topright",TeX("$\\bar{P_{t}}$"),lty = "dotdash",col = "red"
       ,lwd = 2)

acf(ts,lag.max = 32, main = "ACF Prob CAZ",ylab = TeX("$\\rho_{k}$")
    ,xlab = TeX("$k$")) 
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)

acf(ts,type = "partial",main = "PACF Prob CAZ",lag.max =32
    ,ylab = TeX("$\\phi_{k,k}$"),xlab = "k")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)

##Obtenemos las primeras diferencias

dts <- diff(ts,1)

par(mfrow=c(1,3))
plot.ts(dts,main="Diferencias de las series de probabilidades \nCruz Azul",lwd = 2,xlab = "Tiempo",
        ylab = TeX("$\\Delta\\P_{t}$"),col = "#2E64FE")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)
lines(ts(cummean(dts),start = c(1996,1),frequency = 2),col = "red"
      ,lty = "dotdash",lwd = 2)
legend("topright",TeX("$\\bar{P_{t}}$"),lty = "dotdash",col = "red"
       ,lwd = 2)
acf(dts,lag.max = 32, main = "ACF dif. Prob CAZ",ylab = TeX("$\\rho_{k}$")
    ,xlab = TeX("$k$")) 
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)
acf(dts,type = "partial",main = "PACF dif. Prob CAZ",lag.max =32,
    ylab = TeX("$\\phi_{k,k}$"),xlab = "k")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)


# Función para graficar la serie de tiempo, la acf y pacf
plot_ts <- function(ts, d, D, s, serie = ""){
     if(d ==1) ts <- diff(ts,1)
     if (D==1) ts <- diff(ts, s)
     dts <- ts
     par(mfrow=c(1,3))
     plot.ts(dts,main=TeX(paste("Serie de ",serie)), lwd = 2, xlab = "Tiempo",
             ylab = TeX(serie),col = "#3946aa")
     axis(side = 1,lwd = 2)
     axis(side = 2,lwd = 2)
     box(lwd=2)
     acf(dts,lag.max = 32, main = TeX(paste("ACF de ",serie)), ylab = TeX("$\\rho_{k}$")
         ,xlab = TeX("$k$")) 
     axis(side = 1,lwd = 2)
     axis(side = 2,lwd = 2)
     box(lwd=2)
     acf(dts,type = "partial",main = TeX(paste("PACF de ",serie)),lag.max =32,
         ylab = TeX("$\\phi_{k,k}$"),xlab = "k")
     axis(side = 1,lwd = 2)
     axis(side = 2,lwd = 2)
     box(lwd=2)
}


##PROPONEMOS LOS MODELOS

##M1: ARIMA(0,1,1) == MA(1) para las primeras diferencias

plot_ts(ts, 1, 0, 0, "$\\Delta^1\\P_{t}$")

ARIMA_0_1_1 <- arima(ts, order = c(0,1,1))

ARIMA_0_1_1

coeftest(ARIMA_0_1_1)

##M2: ARIMA_1_1_2 

plot_ts(ts, 1, 0, 0, "$\\Delta^1\\P_{t}$")

ARIMA_1_1_2<-arima(ts,order = c(1,1,2),fixed = c(NA,0,NA))

ARIMA_1_1_2

coeftest(ARIMA_1_1_2)

##M3: SARIMA(2,1,0)_5 

plot_ts(ts, 0, 1, 5,"$\\Delta_{5}^1\\P_{t}$")

SARIMA_2_1_0_5 <- arima(ts, order = c(0,0,0), seasonal = list(order = c(2,1,0), period = 5))

SARIMA_2_1_0_5

coeftest(SARIMA_2_1_0_5)

##M2: SARIMA(0, (1,1,0)_8 bien

plot_ts(ts, 0, 1, 8,"$\\Delta_{8}^1\\P_{t}$")

SARIMA_1_1_0_8 <- arima(ts, order = c(0,0,0), seasonal = list(order = c(1,1,0), period = 8))

SARIMA_1_1_0_8

coeftest(SARIMA_1_1_0_8)

#Criterios de Selección
AIC(ARIMA_0_1_1)
BIC(ARIMA_0_1_1)

AIC(ARIMA_1_1_2)
BIC(ARIMA_1_1_2)

AIC(SARIMA_2_1_0_5)
BIC(SARIMA_2_1_0_5)

AIC(SARIMA_1_1_0_8)
BIC(SARIMA_1_1_0_8)

##AIC df
akaike<-data.frame("ARIMA_0_1_1"=AIC(ARIMA_0_1_1), 
                   "ARIMA_1_1_2"=AIC(ARIMA_1_1_2),
                   "SARIMA_2_1_0_5" = AIC(SARIMA_2_1_0_5),
                   "SARIMA_1_1_0_8"=AIC(SARIMA_1_1_0_8))
akaike

##BIC df 
bayes<-data.frame("ARIMA_0_1_1"=BIC(ARIMA_0_1_1), 
                  "ARIMA_1_1_2"=BIC(ARIMA_1_1_2),
                  "SARIMA_2_1_0_5" = BIC(SARIMA_2_1_0_5),
                  "SARIMA_1_1_0_8"=BIC(SARIMA_1_1_0_8)) 
bayes

res1<-ARIMA_0_1_1$residuals
res2<-ARIMA_1_1_2$residuals
res3<-SARIMA_2_1_0_5$residuals
res4<-SARIMA_1_1_0_8$residuals

# Gráfica de los residuales como serie de tiempo
dev.off()
par(mfrow = c(2,2))
plot(res1,xlab = "",main = "Residuos modelo 1",lwd = 2,
     col = "#CD5C5C",ylab = TeX("$\\e_{1}$"))
axis(side = 1,lwd = 2) 
axis(side = 2,lwd = 2)
box(lwd=2)

plot(res2,xlab = "",ylab = TeX("$\\e_{2}$"),col = "#CD5C5C",
     main = "Residuos modelo 2",lwd = 2)
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)

plot(res3,lwd = 2,col = "#CD5C5C",ylab = TeX("$\\e_{3}$"), 
     main = "Residuos modelo 3")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)

plot(res4,lwd = 2,col = "#CD5C5C",ylab = TeX("$\\e_{4}$"), 
     main = "Residuos modelo 4")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)

#histograma de los residuales
dev.off()
par(mfrow = c(2,2))
hist(res1, freq = 0,xlab = TeX("$\\e_{1}$"),col = "#00A4FF",
     ylab = "Densidad",main = "Histograma de los residuos modelo 1")
     axis(side = 1,lwd = 2)
     axis(side = 2,lwd = 2)
hist(res2, freq = 0,xlab = TeX("$\\e_{2}$"),col = "#00A4FF",
     ylab = "Densidad",main = "Histograma de los residuos modelo 2")
     axis(side = 1,lwd = 2)
     axis(side = 2,lwd = 2)
hist(res3, freq = 0,xlab = TeX("$\\e_{3}$"),col = "#00A4FF",
     ylab = "Densidad",main = "Histograma de los residuos modelo 3")
     axis(side = 1,lwd = 2)
     axis(side = 2,lwd = 2)
hist(res4, freq = 0,xlab = TeX("$\\e_{4}$"),col = "#00A4FF",
     ylab = "Densidad",main = "Histograma de los residuos modelo 4")
     axis(side = 1,lwd = 2)
     axis(side = 2,lwd = 2)

# ACF y PACF de los residuales
par(mfrow = c(2,4))
acf(res1,lag.max = 32, main = TeX("ACF $\\e_{1}$"),ylab = TeX("$\\rho_{k}$")
    ,xlab = TeX("$k$")) 
   axis(side = 1,lwd = 2)
   axis(side = 2,lwd = 2)
   box(lwd=2)

acf(res1,type = "partial",main = TeX("PACF $\\e_{1}$"),lag.max =32,
    ylab = TeX("$\\phi_{k,k}$"),xlab = "k")
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

acf(res2,lag.max = 32, main = TeX("ACF $\\e_{2}$"),ylab = TeX("$\\rho_{k}$")
    ,xlab = TeX("$k$")) 
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

acf(res2,type = "partial",main = TeX("PACF $\\e_{2}$"),lag.max =32,
    ylab = TeX("$\\phi_{k,k}$"),xlab = "k")
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

acf(res3,lag.max = 32, main = TeX("ACF $\\e_{3}$"),ylab = TeX("$\\rho_{k}$")
    ,xlab = TeX("$k$")) 
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

acf(res3,type = "partial",main = TeX("PACF $\\e_{3}$"),lag.max =32,
    ylab = TeX("$\\phi_{k,k}$"),xlab = "k")
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

acf(res4,lag.max = 32, main = TeX("ACF $\\e_{4}$"),ylab = TeX("$\\rho_{k}$")
    ,xlab = TeX("$k$")) 
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

acf(res4,type = "partial",main = TeX("PACF $\\e_{4}$"),lag.max =32,
    ylab = TeX("$\\phi_{k,k}$"),xlab = "k")
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

#Prueba de Dickey-Fuller con H_0: la serie no es estacionaria

#Modelo 1
adf.test(res1)
adf_pv_ts<-sapply(1:18,function(i)adf.test(res1,k=i)$p.value)
summary(adf_pv_ts)    
##

#Modelo 2
adf.test(res2)
adf_pv_ts<-sapply(1:18,function(i)adf.test(res2,k=i)$p.value)
summary(adf_pv_ts)    
##

#Modelo 3
adf.test(res3)
adf_pv_ts<-sapply(1:18,function(i)adf.test(res3,k=i)$p.value)
summary(adf_pv_ts)    
##

#Modelo 4
adf.test(res4)
adf_pv_ts<-sapply(1:18,function(i)adf.test(res4,k=i)$p.value)
summary(adf_pv_ts)    
##
    
#Prueba de Ljung-Box con H_0: la serie es estacionaria

#Modelo 1
Box.test(res1,type = "Ljung-Box")
lb_pv_ts<-sapply(1:18,function(i)as.numeric(Box.test(res1,type = "Ljung-Box",lag = i)$p.value))
summary(lb_pv_ts) 
## 

#Modelo 2
Box.test(res2,type = "Ljung-Box")
lb_pv_ts<-sapply(1:18,function(i)as.numeric(Box.test(res2,type = "Ljung-Box",lag = i)$p.value))
summary(lb_pv_ts) 
##

#Modelo 3
Box.test(res3,type = "Ljung-Box")
lb_pv_ts<-sapply(1:18,function(i)as.numeric(Box.test(res3,type = "Ljung-Box",lag = i)$p.value))
summary(lb_pv_ts) 
#

#Modelo 4
Box.test(res4,type = "Ljung-Box")
lb_pv_ts<-sapply(1:18,function(i)as.numeric(Box.test(res4,type = "Ljung-Box",lag = i)$p.value))
summary(lb_pv_ts) 
#

###Predicción 

##1er modelo 
pred1<-predict(ARIMA_0_1_1,n.ahead = 10)
p1<-pred1$pred
sd1<-pred1$se
U<- p1 + sd1 
L<- p1 - sd1 
xx<-c(time(U),rev(time(U)))
yy<-c(L,rev(U))

par(mfrow = c(1,1))
ts.plot(ts,p1,main = "Pronóstico Modelo 1",ylim=c(min(yy),1.0),
        col = "#264fbd",lwd = 2, ylab = TeX("$P_{t}$"),
        xlab = "Temporadas")
polygon(xx,yy,border = 8,col = gray(.6,alpha = .2))
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)


##2do Modelo 
pred2<-predict(ARIMA_1_1_2,n.ahead = 10)
p2<-pred2$pred
sd2<-pred2$se
U<- p2 + sd2 
L<- p2 - sd2 
xx<-c(time(U),rev(time(U)))
yy<-c(L,rev(U))

par(mfrow = c(1,1))
ts.plot(ts,p2,main = "Pronóstico Modelo 2",ylim=c(min(yy),1.0),
        col = "#3946aa",lwd = 2, ylab = TeX("$P_{t}$"),
        xlab = "Temporadas")
polygon(xx,yy,border = 8,col = gray(.6,alpha = .2))
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)

##3er modelo
pred3<-predict(SARIMA_2_1_0_5,n.ahead = 10)
p3<-pred3$pred
sd3<-pred3$se
U<- p3 + sd3 
L<- p3 - sd3 
xx<-c(time(U),rev(time(U)))
yy<-c(L,rev(U))

dev.off() 
par(mfrow = c(1,1))
ts.plot(ts,p3,main = "Pronóstico Modelo 3", ylim=c(min(yy),1.0),
        col = "#2d2d7c",lwd = 2, ylab = TeX("$P_{t}$"),
        xlab = "Temporadas")
polygon(xx,yy,border = 8,col = gray(.6,alpha = .2))
abline(h =  0)


axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)

##4to Modelo 
pred4<-predict(SARIMA_1_1_0_8 ,n.ahead = 10)
p4<-pred4$pred
sd4<-pred4$se
U<- p4 + sd4 
L<- p4 - sd4 
xx<-c(time(U),rev(time(U)))
yy<-c(L,rev(U))

par(mfrow = c(1,1))
ts.plot(ts,p4,main = "Pronóstico Modelo 4",ylim=c(min(yy),1.0),
        col = "#3946aa",lwd = 2, ylab = TeX("$P_{t}$"),
        xlab = "Temporadas")
polygon(xx,yy,border = 8,col = gray(.6,alpha = .2))
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2) 

##DM 

res1<-ARIMA_0_1_1$residuals
res2<-ARIMA_1_1_2$residuals
res3<-SARIMA_2_1_0_5$residuals
res4<-SARIMA_1_1_0_8$residuals

res<-list(res1,res2,res3,res4)

p.ivsj.g<-function(i,j)unname(quantile(sapply(1:5,function(k)dm.test(res[[i]],res[[j]], h = k, alternative = "g",power = 1)$p.value),probs = 0.5))
p.ivsj.l<-function(i,j)unname(quantile(sapply(1:5,function(k)dm.test(res[[i]],res[[j]], h = k, alternative = "l",power = 1)$p.value),probs = 0.5))
p.ivsj<-function(i,j)unname(quantile(sapply(1:5,function(k)dm.test(res[[i]],res[[j]], h = k, alternative = "t",power = 1)$p.value),probs = 0.5))

r.1<-mapply(p.ivsj,rep(1,3),2:4) 
r.2<-c(0,mapply(p.ivsj,rep(2,2),3:4))
r.3<-c(0,0,p.ivsj(3,4))
DM <- rbind(r.1,r.2,r.3)
colnames(DM)<-c("M2","M3","M4")
rownames(DM)<-c("M1","M2","M3") 
DM

r.1<-mapply(p.ivsj.g,rep(1,3),2:4) 
r.2<-c(0,mapply(p.ivsj.g,rep(2,2),3:4))
r.3<-c(0,0,p.ivsj.g(3,4))
DMg <- rbind(r.1,r.2,r.3)
colnames(DMg)<-c("M2","M3","M4")
rownames(DMg)<-c("M1","M2","M3") 
DMg

r.1<-mapply(p.ivsj.l,rep(1,3),2:4) 
r.2<-c(0,mapply(p.ivsj.l,rep(2,2),3:4))
r.3<-c(0,0,p.ivsj.l(3,4))
DMl <- rbind(r.1,r.2,r.3)
colnames(DMl)<-c("M2","M3","M4")
rownames(DMl)<-c("M1","M2","M3") 
DMl


# Comparación de la serie de tiempo original con el pronóstico que se obtiene de cada modelo 
#en las temporadas que ya están dadas

#Modelo 1
par(mfrow = c(2,1))
plot.ts(ts,main="Probabilidad de ser al menos subcampeón \nSerie original",lwd = 2,xlab = "Tiempo",
        ylab = TeX("$\\P_{t}$"),col = "dodgerblue4")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
plot.ts(ts - ARIMA_0_1_1$residuals ,main="Estimación de probabilidad de ser al menos subcampeón \n Modelo 1 ",lwd = 2,xlab = "Tiempo",
        ylab = TeX("$\\hat{P}_{t}$"),col = "firebrick")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)

#Modelo 2
par(mfrow = c(2,1))
plot.ts(ts,main="Probabilidad de ser al menos subcampeón \nSerie original",lwd = 2,xlab = "Tiempo",
        ylab = TeX("$\\P_{t}$"),col = "dodgerblue4")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
plot.ts(ts - ARIMA_1_1_2$residuals ,main="Estimación de probabilidad de ser al menos subcampeón \n Modelo 1 ",lwd = 2,xlab = "Tiempo",
        ylab = TeX("$\\hat{P}_{t}$"),col = "firebrick")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)

#Modelo 3
par(mfrow = c(2,1))
plot.ts(ts,main="Probabilidad de ser al menos subcampeón \nSerie original",lwd = 2,xlab = "Tiempo",
        ylab = TeX("$\\P_{t}$"),col = "dodgerblue4")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
plot.ts(ts - SARIMA_2_1_0_5$residuals ,main="Estimación de probabilidad de ser al menos subcampeón \n Modelo 1 ",lwd = 2,xlab = "Tiempo",
        ylab = TeX("$\\hat{P}_{t}$"),col = "firebrick")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)

#Modelo 4
par(mfrow = c(2,1))
plot.ts(ts,main="Probabilidad de ser al menos subcampeón \nSerie original",lwd = 2,xlab = "Tiempo",
        ylab = TeX("$\\P_{t}$"),col = "dodgerblue4")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
plot.ts(ts - SARIMA_1_1_0_8$residuals ,main="Estimación de probabilidad de ser al menos subcampeón \n Modelo 1 ",lwd = 2,xlab = "Tiempo",
        ylab = TeX("$\\hat{P}_{t}$"),col = "firebrick")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)

## Heteroscedasticidad

res12<-(ARIMA_0_1_1$residuals)^2
res22<-(ARIMA_1_1_2$residuals)^2
res32<-(SARIMA_2_1_0_5$residuals)^2
res42<-(SARIMA_1_1_0_8$residuals)^2

dev.off()
par(mfrow = c(2,2))
plot(res12,xlab = "",main = "Residuos al cuadrado modelo 1",lwd = 2,
     col = "#CD5C5C",ylab = TeX("$\\e_{1}$"))
axis(side = 1,lwd = 2) 
axis(side = 2,lwd = 2)
box(lwd=2)

plot(res22,xlab = "",ylab = TeX("$\\e_{2}$"),col = "#CD5C5C",
     main = "Residuos al cuadrado modelo 2",lwd = 2)
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)

plot(res32,lwd = 2,col = "#CD5C5C",ylab = TeX("$\\e_{3}$"), 
     main = "Residuos al cuadrado modelo 3")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)

plot(res42,lwd = 2,col = "#CD5C5C",ylab = TeX("$\\e_{4}$"), 
     main = "Residuos al cuadrado modelo 4")
axis(side = 1,lwd = 2)
axis(side = 2,lwd = 2)
box(lwd=2)

dev.off()
par(mfrow = c(2,2))
hist(res12, freq = 0,xlab = TeX("$\\e_{1}$"),col = "#00A4FF",
     ylab = "Densidad",main = "Histograma de los residuos cuadrados modelo 1")
     axis(side = 1,lwd = 2)
     axis(side = 2,lwd = 2)
hist(res22, freq = 0,xlab = TeX("$\\e_{2}$"),col = "#00A4FF",
     ylab = "Densidad",main = "Histograma de los residuos cuadrados modelo 2")
     axis(side = 1,lwd = 2)
     axis(side = 2,lwd = 2)
hist(res32, freq = 0,xlab = TeX("$\\e_{3}$"),col = "#00A4FF",
     ylab = "Densidad",main = "Histograma de los residuos cuadrados modelo 3")
     axis(side = 1,lwd = 2)
     axis(side = 2,lwd = 2)
hist(res42, freq = 0,xlab = TeX("$\\e_{4}$"),col = "#00A4FF",
     ylab = "Densidad",main = "Histograma de los residuos cuadrados modelo 4")
     axis(side = 1,lwd = 2)
     axis(side = 2,lwd = 2)


par(mfrow = c(2,4))
acf(res12,lag.max = 32, main = TeX("ACF $\\e_{1}^{2}$"),ylab = TeX("$\\rho_{k}$")
    ,xlab = TeX("$k$")) 
   axis(side = 1,lwd = 2)
   axis(side = 2,lwd = 2)
   box(lwd=2)

acf(res12,type = "partial",main = TeX("PACF $\\e_{1}^{2}$"),lag.max =32,
    ylab = TeX("$\\phi_{k,k}$"),xlab = "k")
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

acf(res22,lag.max = 32, main = TeX("ACF $\\e_{2}^{2}$"),ylab = TeX("$\\rho_{k}$")
    ,xlab = TeX("$k$")) 
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

acf(res22,type = "partial",main = TeX("PACF $\\e_{2}^{2}$"),lag.max =32,
    ylab = TeX("$\\phi_{k,k}$"),xlab = "k")
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

acf(res32,lag.max = 32, main = TeX("ACF $\\e_{3}^{2}$"),ylab = TeX("$\\rho_{k}$")
    ,xlab = TeX("$k$")) 
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

acf(res32,type = "partial",main = TeX("PACF $\\e_{3}^{2}$"),lag.max =32,
    ylab = TeX("$\\phi_{k,k}$"),xlab = "k")
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

acf(res42,lag.max = 32, main = TeX("ACF $\\e_{4}^{2}$"),ylab = TeX("$\\rho_{k}$")
    ,xlab = TeX("$k$")) 
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

acf(res42,type = "partial",main = TeX("PACF $\\e_{4}^{2}$"),lag.max =32,
    ylab = TeX("$\\phi_{k,k}$"),xlab = "k")
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

#Ljung-Box
Box.test(res12,type = "Ljung-Box")
lb_pv_ts<-sapply(1:40,function(i)as.numeric(Box.test(res12,type = "Ljung-Box",lag = i)$p.value))
summary(lb_pv_ts) 
## 

#Ljung-Box
Box.test(res22,type = "Ljung-Box")
lb_pv_ts<-sapply(1:40,function(i)as.numeric(Box.test(res22,type = "Ljung-Box",lag = i)$p.value))
summary(lb_pv_ts) 
##

#Ljung-Box
Box.test(res32,type = "Ljung-Box")
lb_pv_ts<-sapply(1:40,function(i)as.numeric(Box.test(res32,type = "Ljung-Box",lag = i)$p.value))
summary(lb_pv_ts) 
#

#Ljung-Box
Box.test(res42,type = "Ljung-Box")
lb_pv_ts<-sapply(1:40,function(i)as.numeric(Box.test(res42,type = "Ljung-Box",lag = i)$p.value))
summary(lb_pv_ts) 
#

res2<-cbind(res12,res22,res32,res42)
Atest<-apply(res2,2,ArchTest,lags = 10)
Atest

Aptest<-apply(res2,2,function(x)unname(ArchTest(x,lags = 10)$p.value))
Aptest


a1test<-sapply(1:18,function(i)as.numeric(ArchTest(res12,lags = i)$p.value))
summary(a1test)
a2test<-sapply(1:18,function(i)as.numeric(ArchTest(res22,lags = i)$p.value))
summary(a2test)
a3test<-sapply(1:18,function(i)as.numeric(ArchTest(res32,lags = i)$p.value))
summary(a3test)
a4test<-sapply(1:18,function(i)as.numeric(ArchTest(res42,lags = i)$p.value))
summary(a4test)
a4test


###
dev.off()
par(mfrow=c(2,1))
acf(res12,lag.max = 20, main = TeX("ACF $\\e_{1}^{2}$"),ylab = TeX("$\\rho_{k}$")
    ,xlab = TeX("$k$")) 
   axis(side = 1,lwd = 2)
   axis(side = 2,lwd = 2)
   box(lwd=2)

acf(res12,type = "partial",main = TeX("PACF $\\e_{1}^{2}$"),lag.max =20,
    ylab = TeX("$\\phi_{k,k}$"),xlab = "k")
    axis(side = 1,lwd = 2)
    axis(side = 2,lwd = 2)
    box(lwd=2)

    