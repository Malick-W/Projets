# quelques outils (tests ADF/KPSS, auto.arima, ...)
library("aTSA")
library("forecast")
library("tseries")
library("Metrics")
library("ftsa")
library("dLagM")
library("statsr")

##Lecture du fichier à partir de import Dataset
serie = sncf$VK

plot(sncf$VK, type = 'l', main = 'Graphique de serie X(t)')
#A vu d'oeil cette serie n'est pas stationnaire
plot(log(sncf$VK), type = 'l')
#de meme le log n'est pas stationnaire mais elle ? une variance tr?s petite

#Sx = decompose(sncf$VK, type = c('additive', 'multiplica'), filter = NULL)
# on constate qu'il ya une grande correlation car les peaks et dont pas stationnaire
#on constate qu'il ya une periode de 12  

acf(sncf$VK)
freq = 12
y = log(sncf$VK)


#On definie la serie chrono
SY = ts(y, frequency=freq)
# decompose la serie entre la tendance, saisonalite et bruit
sx = decompose(SY)
plot(sx)
frequency(sncf$VK)

#Pour faire la prediction on pourra utiliser la tendance.
#Construire un model sur la tendance qui nous permettra a faire la prediction.
#on extrait le bruit blanc  et on teste la stationnarite des bruit
 
bruit = sx$random
bru =   bruit[!is.na(bruit)]
bruit = bruit[!is.na(bruit)]
kpss.test(bruit)
adf.test(bruit)
# On constate que p= 0.1, KPSS indique la fluc est stationnaire
#on constate aussi p =0.01, adf indique 



###Identification ? priori du model potentiels

#on determine le seuil de p et q en utilisant acf et pacf

acf(bruit) # on prends q {1, 5, 6, 7, 8, 9, 10, 11}
#pacf s'annule a partir d'un certain rang, on peux prendre  p = 10 
#on peut dire que le bruit est un AR(10)
pacf(bruit) #on peut prendre  p {2, 5, 6, 7, 8, 9, 10}

#p= 2 et q= 1 # critere qui minimisent mse
Arm1 = Arima(bruit, order = c(2, 0,2), include.mean = FALSE)
Arm2 = Arima(bruit, order = c(2, 0,4), include.mean = FALSE) 
Arm3 = Arima(bruit, order = c(1, 0,3), include.mean = FALSE)
Arm4 = Arima(bruit, order = c(1, 0,2), include.mean = FALSE)
Arm5 = Arima(bruit, order = c(3, 0,2), include.mean = FALSE)#


### Estimation des modele potentiels
#la commande Arm$coef nous donne les coefs de AR et MA
Arm1$coef
Arm2$coef
Arm3$coef
Arm4$coef
Arm5$coef

### Verification des modeles potentiels:
Arm1$coef/sqrt(diag(Arm1$var.coef))  
Arm2$coef/sqrt(diag(Arm2$var.coef))
Arm3$coef/sqrt(diag(Arm3$var.coef)) 
Arm4$coef/sqrt(diag(Arm4$var.coef))
Arm5$coef/sqrt(diag(Arm5$var.coef))

##Blancheur des residues des different model
res1 = (Arm1$residuals)/(sqrt(Arm1$sigma2))
res2 = (Arm2$residuals)/(sqrt(Arm2$sigma2))
res3 = (Arm3$residuals)/(sqrt(Arm3$sigma2))
res4 = (Arm4$residuals)/(sqrt(Arm4$sigma2))
res5 =  (Arm5$residuals)/(sqrt(Arm5$sigma2))

Box.test(res1, lag = 1, type = c("Box-Pierce", "Ljung-Box")) #p-value > 0.05, on constate l'independence
Box.test(res2, lag = 1, type = c("Box-Pierce", "Ljung-Box")) #p-value > 0.05, on constate l'independence
Box.test(res3, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
Box.test(res4, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
Box.test(res5, lag = 1, type = c("Box-Pierce", "Ljung-Box"))


#acf des residues
pacf(res1)
acf(res1)  
#Normalit? des residuees: on verifie les 4 tests
hist(res2, breaks=sqrt(length(res2)))  #le hist
plot(res2, type="p")
abline(h=c(-1.96,1.96)) # on verifie que les point sont dans l'inter [-1.96, 1.96]
shapiro.test(res2) #p = 0.8801 nous indique 
qqnorm(res2)
plot(res2, type = 'l')


#on constate que res1, res2, res3, res3 sont blancs et gaussiennes.

## Choix definitif du modele.
auto.arima(bruit)  #Nous donne la bonne p = 2 et q= 2

#Graphique du bruit et valeurs estimes 
var(Arm1$fitted)


plot(Arm1$fitted, type= 'l', col = 'green', main = 'Graphiques des mod?les', ylab = 'log Y')
lines(Arm2$fitted,type = 'l',  col= 'red')
lines(Arm5$fitted, type = 'l' , col= 'blue')
lines(bruit, type= 'l')


#On modelise notre serie en utilisant les valeurs estim?e

w = log(sncf$VK)[7:210]
trend    = as.numeric(sx$trend)[7:210]
seasonal = as.numeric(sx$seasonal)[7:210]
bruit    = as.numeric(sx$random)[7:210]

#plot(Arm1$fitted, type= 'l', col = 'yellow')
#lines(log(sncf$VK)[7:204], type = 'l')

acf( Arm1$fitted)

#Modelisation de la serie logarithmic
yest = trend + seasonal + Arm1$fitted
y2 = trend + seasonal + Arm2$fitted
y3 = trend +seasonal  + Arm5$fitted 

#plot((yest), type="l", col="red")
#lines(log(sncf$VK)[7:210], type = 'l')


#Modelisation de notre serie
yest = trend + seasonal + Arm1$fitted
plot(exp(yest), type="l", col="red")
lines(exp(y2), type="l", col="blue")
lines(exp(y3), type="l", col="green")
lines((sncf$VK)[7:210], type = 'l')

##############################################
#Calcul de l'erreur 
mase(exp(log(sncf$VK)[7:210]), yest)
rmse( (sncf$VK)[7:210], exp(yest))
rmse( (sncf$VK)[7:210], exp(y2))
rmse( (sncf$VK)[7:210], exp(y3))

mape( (sncf$VK)[7:210], exp(yest))
mape( (sncf$VK)[7:210], exp(y2))
mape( (sncf$VK)[7:210], exp(y3))

ytrei = log(sncf$VK)[7:210]
a = 0.05
upper <- trend + seasonal + fitted(Arm1) + qnorm(1-a/2)*sqrt(Arm1$sigma2)
lower <- trend + seasonal + fitted(Arm1) - qnorm(1-a/2)*sqrt(Arm1$sigma2)

plot(exp(ytrei), type="l", ylim=c(min(exp(lower)),max(exp(upper))))
polygon(c(time(exp(ytrei)),rev(time(exp(ytrei)))), c(exp(upper),rev(exp(lower))), 
        col=rgb(0,0,0.6,0.2), border=FALSE)
lines(exp(yest), type="l", col="red")
#lines(exp(y2), type="l", col="blue")
#lines(exp(y3), type="l", col="green")
#lines((sncf$VK)[7:210], type = 'l')




##########################################################################################
#Le modele SARIMA
##########################################################################################

#1###Stationarisation de la serie.
 Z_t = y 
plot(y, type = 'l') #on demontre que la serie n'est pas stationnaire(presence d'une tendance)
#On de V_t la serie VK et on travaille avec Z = log(V_t)
acf(Z_t, lag = 36, ylim=c(-1,1)) #on constate que la serie n'est pas un bruit blanc et il y 
                              #plusieurs correlations et elle n'est pas stationnaire.

y_dif1=diff(y,lag=12)#on effectue une differenciation (I-B)
adf.test(y_dif1)
kpss.test(y_dif1)
plot(y_dif1, type = 'l')# visuellement presence d'une saisonalit?
plot(acf(y_dif1,lag =36,ylim=c(-1,1))) #y_diff1 est correle avec une periode de 12

y_diff2 = diff(y_dif1, lag =1)
plot(acf(y_diff2,lag =36,ylim=c(-1,1)))
adf.test(y_diff2)
kpss.test(y_diff2)
plot(y_diff2, type = 'l')

#on estime SARIMA(1,1,1)(1,1,1)_12

mod1=Arima(y,order=c(3,1,0),list(order=c(1,1,0),period=12), include.drift = FALSE)
mod1$coef/sqrt(diag(mod1$var.coef)) 

#on verifie si les residues sont les bruits blancs:
Box.test(mod1$residuals,lag= 3,type="Ljung-Box")#pourquoi lag 12? est ce qu'on aussi verifie pour les lag?
resid1 = (mod1$residuals)/(sqrt(mod1$sigma2))
hist(resid1, breaks=sqrt(length(resid1)))  #le hist
plot(resid1, type="p")
abline(h=c(-1.96,1.96)) 
shapiro.test(resid1) 
qqnorm(resid1)
abline(0, 1)
plot(resid1, type = 'l')



mod2=Arima(y,order=c(0,1,1),list(order=c(0,1,0),period=12),include.mean=FALSE, include.drift = FALSE)
mod2$coef/sqrt(diag(mod2$var.coef))
Box.test(mod2$residuals,lag= 3,type="Ljung-Box")
resid2 = (mod2$residuals)/(sqrt(mod2$sigma2))
hist(resid2, breaks=sqrt(length(resid2)))  #le hist
plot(resid2, type="p")
abline(h=c(-1.96,1.96))
# on verifie que les point sont dans l'inter [-1.96, 1.96]
shapiro.test(resid2) 
qqnorm(resid2)
abline(0, 1)
plot(resid2, type = 'l')



model3 =Arima(y,order=c(2,1,2),list(order=c(1,1,1),period=12),include.mean=FALSE, include.drift = FALSE)
summary(model3) #valeur Aic -634.35
model3$coef/sqrt(diag(model3$var.coef))#on constate que les coef sont tous signicatifs.
Box.test(model3$residuals,lag= 3,type="Ljung-Box") #les residues sont bruit blancs
resid3 = (model3$residuals)/(sqrt(model3$sigma2))
hist(resid3, breaks=sqrt(length(resid3)))  #le hist
plot(resid3, type="p")
abline(h=c(-1.96,1.96)) # on verifie que les point sont dans l'inter [-1.96, 1.96]
shapiro.test(resid3) #p = 0.8801 nous indique 
qqnorm(resid3)
abline(0, 1)




model4=Arima(y,order=c(1,1,1),list(order=c(1,1,1),period=12),include.mean=FALSE, include.drift = FALSE)
model4$coef/sqrt(diag(model4$var.coef))
Box.test(model4$residuals,lag= 3,type="Ljung-Box")
resid4 = (model4$residuals)/(sqrt(model4$sigma2))
hist(resid4, breaks=sqrt(length(resid1)))  #le hist
plot(resid4, type="p")
abline(h=c(-1.96,1.96)) # on verifie que les point sont dans l'inter [-1.96, 1.96]
shapiro.test(resid4) #p = 0.8801 nous indique 
qqnorm(resid4)
abline(0, 1)
plot(resid4, type = 'l')



#graphique de model$fitted
model1=Arima(y,order=c(1,1,1),list(order=c(0,1,1),period=12),include.mean=FALSE, include.drift = FALSE)
model2=Arima(y,order=c(0,1,2),list(order=c(1,1,0),period=12),include.mean=FALSE, include.drift = FALSE)
model3=Arima(y,order=c(3,1,0),list(order=c(0,1,1),period=12),include.mean=FALSE, include.drift = FALSE)

#plot(serie, type = 'l', col = 'black')

plot(exp(model1$fitted), type="l", col="green", xlab = "temps", ylab = "Nbre de voyageurs", main = "Approximations de X_t")
lines(exp(model2$fitted), type="l", col="red")

lines(exp(model3$fitted), type="l", col="blue")
lines((sncf$VK), type = 'l')
text(30, 3900, 'Modele 1 (SARIMA(1, 1, 1)(0, 1, 1)_12)', col = 'green')
text(15, 3800, 'Modele 2 (SARIMA(0, 1, 2)(1, 1, 0)_12)', col = 'red')
text(15, 3700, 'Modele 3 (SARIMA(3, 1, 0)(1, 1, 1)_12)', col = 'blue')
text(10, 3600, 'Serie originale', col = 'black')








#######################################################################################
#Prevision 
####################################################################################### 
 #on choisit la serie qui minimise l'erreur 
model1=Arima(y,order=c(1,1,1),list(order=c(0,1,1),period=12))
model2=Arima(y,order=c(0,1,2),list(order=c(1,1,0),period=12))
model3=Arima(y,order=c(3,1,0),list(order=c(0,1,1),period=12))

serie =  sncf$VK
se_tron   = serie[1:204]
serie_test = serie[205:216]
lse_tron = log(se_tron)[1:204]
lotest  = log(sncf$VK)[205:216]

model1_tron =  Arima(lse_tron,order=c(1,1,1),list(order=c(0,1,1),period=12))
model2_tron  =  Arima(lse_tron,order=c(0,1,2),list(order=c(1,1,0),period=12))
model3_tron =  Arima(lse_tron,order=c(3,1,0),list(order=c(0,1,1),period=12))

pred_model1 = forecast.Arima(model1_tron, h =12, level = c(0.95, 0.80))
pred_model2 = forecast(model2_tron, h =12, level = c(0.95, 0.80))
pred_model3 = forecast(model3_tron, h =12, level = c(0.95, 080))

summary(pred_model3)

pred_mo1  =  exp(pred_model1$mean)*exp((model1_tron$sigma2)/2)
pred_mo2  =  exp(pred_model2$mean)*exp((model2_tron$sigma2)/2)
pred_mo3  =  exp(pred_model3$mean)*exp((model3_tron$sigma2)/2)

predm1 =  exp(pred_model1$mean)
predm2 =  exp(pred_model2$mean)
predm3 =  exp(pred_model3$mean)


err_model1 = mape(serie_test, pred_mo1)
err_model2 = mape(serie_test, pred_mo2)
err_model3 = mape(serie_test, pred_mo3)

err_m1 = mape(serie_test, predm1)
err_m2 = mape(serie_test, predm2)
err_m3 = mape(serie_test, predm3)



#Graphique de la prediction pour la serie logarithmique
plot(pred_model3)
low = pred_model3$lower
upp = pred_model3$upper



#Prediction sans l'intervalle de confiance
plot(se_tron, type = 'l' , xlim = c(0, 220))
lines(205:216, pred, type = 'l', col = 'blue')
lines(205:216, serie_test, type = 'l', col = 'black')
lines(serie, type = 'l', col = 'red')



#########################################################################################
seritronc = serie[1:204]
serit <-ts(seritronc)
pred_model3=forecast(model3_tron,h=12,level=95)
pred = exp(pred_model3$mean)
pred_l=ts(exp(pred_model3$lower),start= c(205,1), frequency=1)

pred_u=ts(exp(pred_model3$upper),start= c(205, 1),frequency=1)

ts.plot(serit,pred,pred_l,pred_u,xlab="t",ylab="Voyageurs",col=c(1,2,3,3),lty=c(1,1,2,2),lwd=c(1,3,2,2))
legend("topleft",legend=c("X","X_prev"),col=c(1,2,3,3),lty=c(1,1),lwd=c(3,3))
legend("topright",legend=c("int95%_inf","int95%_sup"),col=c(3,3),lty=c(2,2),lwd=c(2,2))





#############################################################################################
ori_serie = serie[1:216]
ts_or <-ts(ori_serie)
x_tronc=window(ts_or,end=c(204,1))
y_tronc=log(x_tronc)
x_a_prevoir=window(ts_or,start=c(205,1))
model3tronc =  Arima(y_tronc,order=c(3,1,0),list(order=c(0,1,1),period=12))
summary(model3tronc)


pred_model3tronc=forecast(model3tronc,h=12,level=95)
pred_tronc=exp(pred_model3tronc$mean)
pred_l_tronc=ts(exp(pred_model3tronc$lower),start=c(205,1),frequency=1)
pred_u_tronc=ts(exp(pred_model3tronc$upper),start=c(205,1),frequency=1)
ts.plot(x_a_prevoir,pred_tronc,pred_l_tronc,pred_u_tronc,xlab="t",ylab="Airpass",col=c(1,2,3,3),lty=c(1,1,2,2),lwd=c(3,3,2,2))
legend("topleft",legend=c("X","X_prev"),col=c(1,2,3,3),lty=c(1,1),lwd=c(3,3))
legend("topright",legend=c("int95%_inf","int95%_sup"),col=c(3,3),lty=c(2,2),lwd=c(2,2))

############################################################################################################










