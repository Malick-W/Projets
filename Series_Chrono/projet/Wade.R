#On visualizer les donnees
library(tseries)
library(forecast)
library(Metrics)
library(ftsa)
library(dLagM)
library(statsr)

path_Conso = "/users/mmath/wade/Bureau/Projets/Projets/Series_Chrono/projet/Conso.RData"
path_Temp = "/users/mmath/wade/Bureau/Projets/Projets/Series_Chrono/projet/Temp.RData"

load("Conso.RData")
load("Temp.RData")

Conso = Conso[-c(1:35064)]
Temp = Temp[-c(1:35064)]
n = length(Conso)

# aspect visuel : transformation necessaire
# le log (possible car >0) appaisera les pics, harmonisera la variance, reduira l'echelle des donnees
plot(1:n, Conso, type="l", col="blue", xlab="Temps", ylab="Conso")
LConso = log(Conso)
plot(1:n, LConso, type="l", col="blue", xlab="Temps", ylab="Conso logarithmiques")
plot(Temp, LConso, xlab="temperature", ylab="Conso")

# une representation graphique superposant la serie avec une moyenne lissee (fenetre de 100)
fen = 100
LConsoLiss = rep(0, n-fen)
for (i in 1:(n-fen)){
  LConsoLiss[i] = mean(LConso[i:(i+fen)])
}
plot(1:n, LConso, type="l", col="blue", xlab="Temps", ylab="Conso logarithmiques")
lines((fen/2+1):(n-fen/2), LConsoLiss, col="red", type="p")
# clairement, esperance non constante, confirmation de la non stationnarite

# pour les tests ADF et KPSS, on se contente ici de Type 1

kpss.test(LConso) # KPSS (Type 1) : non stationnaire
adf.test(LConso) # ADF (Type 1) : stationnaire ???


acf(Conso)
freq = 24

#On definie la serie chrono
SY = ts(LConso, frequency=freq)
# decompose la serie entre la tendance, saisonalite et bruit
sx = decompose(SY)
plot(sx)

#Pour faire la prediction on pourra utiliser la tendance.
#Construire un model sur la tendance qui nous permettra a faire la prediction.
#on extrait le bruit blanc  et on teste la stationnarite des bruit

Res = sx$random
Res = Res[!is.na(Res)]

kpss.test(Res) # On constate que p= 0.1, KPSS indique la fluc est stationnaire
adf.test(Res) # on constate aussi p =0.01, adf indique  la fluc est stationnaire

# structure de correlation dans les residus : clairement ce n'est pas un bruit blanc
acf(Res, main="ACF des residus") 
pacf(Res, main="PACF des residus")

# On s'intéressa dans la suite à construire un modèle ARMA d'ordre p et q du bruit.

# Arm1 = Arima(Res, order = c(2, 0,2), include.mean = FALSE)
# Arm2 = Arima(Res, order = c(2, 0,4), include.mean = FALSE) 
# Arm3 = Arima(Res, order = c(1, 0,3), include.mean = FALSE)
# Arm4 = Arima(Res, order = c(1, 0,2), include.mean = FALSE)
# Arm5 = Arima(Res, order = c(3, 0,2), include.mean = FALSE)
# 
# 
# ##Blancheur des residues des different model
# res1 = (Arm1$residuals)/(sqrt(Arm1$sigma2))
# res2 = (Arm2$residuals)/(sqrt(Arm2$sigma2))
# res3 = (Arm3$residuals)/(sqrt(Arm3$sigma2))
# res4 = (Arm4$residuals)/(sqrt(Arm4$sigma2))
# res5 =  (Arm5$residuals)/(sqrt(Arm5$sigma2))
# 
# Box.test(res1, lag = 1, type = c("Box-Pierce", "Ljung-Box")) #p-value > 0.05, on constate l'independence
# Box.test(res2, lag = 1, type = c("Box-Pierce", "Ljung-Box")) #p-value > 0.05, on constate l'independence
# Box.test(res3, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
# Box.test(res4, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
# Box.test(res5, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
# 
# 
# #acf des residues
# pacf(res1)
# acf(res1)  
# #Normalit? des residuees: on verifie les 4 tests
# hist(res2, breaks=sqrt(length(res2)))  #le hist
# plot(res2, type="p")
# abline(h=c(-1.96,1.96)) # on verifie que les point sont dans l'inter [-1.96, 1.96]
# shapiro.test(res2[1:5000]) #p = 0.8801 nous indique 
# qqnorm(res2)
# plot(res2, type = 'l')
# 
# 

## Choix definitif du modele.
auto.arima(Res)  #Nous donne la bonne p = 4 et q= 3

#Graphique du bruit et valeurs estimes 
Arm1 = Arima(Res, order = c(4, 0,3), include.mean = FALSE)
var(Arm1$fitted)


plot(Arm1$fitted, type= 'l', col = 'green', main = 'Graphiques des mod?les', ylab = 'log Y')
lines(Arm2$fitted,type = 'l',  col= 'red')
lines(Arm5$fitted, type = 'l' , col= 'blue')
lines(bruit, type= 'l')


#On modelise notre serie en utilisant les valeurs estim?e

w = log(Conso)[13:8748]
trend    = as.numeric(sx$trend)[13:8748]
seasonal = as.numeric(sx$seasonal)[13:8748]
bruit    = as.numeric(sx$random)[13:8748]

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
plot(exp(yest), xlim=c(1,96), type="l", col="red")
lines(exp(y2), xlim=c(1,96), type="l", col="blue")
lines(exp(y3), xlim=c(1,96), type="l", col="green")
lines(Conso[13:8748], xlim=c(1,96), type = 'l')

##############################################
#Calcul de l'erreur 
mase(exp(log(Conso)[13:8748]), yest)
rmse( (Conso)[13:8748], exp(yest))
rmse( (Conso)[13:8748], exp(y2))
rmse( (Conso)[13:8748], exp(y3))

mape( (Conso)[13:8748], exp(yest))
mape( (Conso)[13:8748], exp(y2))
mape( (Conso)[13:8748], exp(y3))

ytrain = log(Conso)[13:8748]
a = 0.05
upper <- trend + seasonal + fitted(Arm1) + qnorm(1-a/2)*sqrt(Arm1$sigma2)
lower <- trend + seasonal + fitted(Arm1) - qnorm(1-a/2)*sqrt(Arm1$sigma2)

plot(exp(ytrain), type="l", ylim=c(min(exp(lower)),max(exp(upper))))
polygon(c(time(exp(ytrain)),rev(time(exp(ytrain)))), c(exp(upper),rev(exp(lower))), 
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
acf(Z_t, lag = 72, ylim=c(-1,1)) #on constate que la serie n'est pas un bruit blanc et il y 
#plusieurs correlations et elle n'est pas stationnaire.

y_dif1=diff(y,lag=24)#on effectue une differenciation (I-B)
adf.test(y_dif1)
kpss.test(y_dif1)
plot(y_dif1, type = 'l')# visuellement presence d'une saisonalit?
plot(acf(y_dif1,lag =72,ylim=c(-1,1))) #y_diff1 est correle avec une periode de 12

y_diff2 = diff(y_dif1, lag =1)
plot(acf(y_diff2,lag =72,ylim=c(-1,1)))
adf.test(y_diff2)
kpss.test(y_diff2)
plot(y_diff2, type = 'l')

#on estime SARIMA(1,1,1)(1,1,1)_12

mod1=Arima(y,order=c(3,1,0),list(order=c(1,1,0),period=24), include.drift = FALSE)
mod1$coef/sqrt(diag(mod1$var.coef)) 

#on verifie si les residues sont les bruits blancs:
Box.test(mod1$residuals,lag= 3,type="Ljung-Box")
resid1 = (mod1$residuals)/(sqrt(mod1$sigma2))
hist(resid1, breaks=sqrt(length(resid1)))  
plot(resid1, type="p")
abline(h=c(-1.96,1.96)) 
shapiro.test(resid1[1:5000]) 
qqnorm(resid1)
abline(0, 1)
plot(resid1, type = 'l')



mod2=Arima(y,order=c(0,1,1),list(order=c(0,1,0),period=24),include.mean=FALSE, include.drift = FALSE)
mod2$coef/sqrt(diag(mod2$var.coef))
Box.test(mod2$residuals,lag= 3,type="Ljung-Box")
resid2 = (mod2$residuals)/(sqrt(mod2$sigma2))
hist(resid2, breaks=sqrt(length(resid2)))  #le hist
plot(resid2, type="p")
abline(h=c(-1.96,1.96))
# on verifie que les point sont dans l'inter [-1.96, 1.96]
shapiro.test(resid2[1:5000]) 
qqnorm(resid2)
abline(0, 1)
plot(resid2, type = 'l')



model3 =Arima(y,order=c(2,1,2),list(order=c(1,1,1),period=24),include.mean=FALSE, include.drift = FALSE)
summary(model3) #valeur Aic -634.35
model3$coef/sqrt(diag(model3$var.coef))#on constate que les coef sont tous signicatifs.
Box.test(model3$residuals,lag= 3,type="Ljung-Box") #les residues sont bruit blancs
resid3 = (model3$residuals)/(sqrt(model3$sigma2))
hist(resid3, breaks=sqrt(length(resid3)))  #le hist
plot(resid3, type="p")
abline(h=c(-1.96,1.96)) # on verifie que les point sont dans l'inter [-1.96, 1.96]
shapiro.test(resid3[1:5000]) 
qqnorm(resid3)
abline(0, 1)




model4=Arima(y,order=c(1,1,1),list(order=c(1,1,1),period=24),include.mean=FALSE, include.drift = FALSE)
model4$coef/sqrt(diag(model4$var.coef))
Box.test(model4$residuals,lag= 3,type="Ljung-Box")
resid4 = (model4$residuals)/(sqrt(model4$sigma2))
hist(resid4, breaks=sqrt(length(resid1)))  
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
lines((Conso), type = 'l')








#######################################################################################
#Prevision 
####################################################################################### 
y=LConso
#on choisit la serie qui minimise l'erreur 
model1=Arima(y,order=c(1,1,1),list(order=c(0,1,1),period=24))
model2=Arima(y,order=c(0,1,2),list(order=c(1,1,0),period=24))
model3=Arima(y,order=c(3,1,0),list(order=c(0,1,1),period=24))

serie =  Conso
se_tron   = serie[1:8424]
serie_test = serie[8425:8760]
lse_tron = log(se_tron)[1:8424]
lotest  = log(Conso)[8425:8760]

model1_tron =  Arima(lse_tron,order=c(1,1,1),list(order=c(0,1,1),period=24))
model2_tron  =  Arima(lse_tron,order=c(0,1,2),list(order=c(1,1,0),period=24))
model3_tron =  Arima(lse_tron,order=c(3,1,0),list(order=c(0,1,1),period=24))


pred_model1 = forecast(model1_tron, h =336, level = c(0.80, 0.95))
pred_model2 = forecast(model2_tron, h =336, level = c(0.95, 0.80))
pred_model3 = forecast(model3_tron, h =336, level = c(0.95, 080))

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
plot(se_tron, type = 'l' , xlim = c(0, 8760))
lines(8425:8760, pred, type = 'l', col = 'blue')
lines(8425:8760, serie_test, type = 'l', col = 'black')
lines(serie, type = 'l', col = 'red')



#########################################################################################
seritronc = serie[1:8424]
serit <-ts(seritronc)
pred_model3=forecast(model3_tron,h=24,level=95)
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


pred_model3tronc=forecast(model3tronc,h=24,level=95)
pred_tronc=exp(pred_model3tronc$mean)
pred_l_tronc=ts(exp(pred_model3tronc$lower),start=c(205,1),frequency=1)
pred_u_tronc=ts(exp(pred_model3tronc$upper),start=c(205,1),frequency=1)
ts.plot(x_a_prevoir,pred_tronc,pred_l_tronc,pred_u_tronc,xlab="t",ylab="Airpass",col=c(1,2,3,3),lty=c(1,1,2,2),lwd=c(3,3,2,2))
legend("topleft",legend=c("X","X_prev"),col=c(1,2,3,3),lty=c(1,1),lwd=c(3,3))
legend("topright",legend=c("int95%_inf","int95%_sup"),col=c(3,3),lty=c(2,2),lwd=c(2,2))

############################################################################################################
