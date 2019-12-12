
# quelques outils (tests ADF/KPSS, auto.arima, ...)
library("aTSA")
library("forecast")

# ouverture du fichier
Strates = read.csv("strates.txt")$Strates
n = length(Strates)

# aspect visuel : transformation necessaire
# le log (possible car >0) appaisera les pics, harmonisera la variance, reduira l'echelle des donnees
plot(1:n, Strates, type="l", col="blue", xlab="Temps", ylab="Strates")
LStrates = log(Strates)
plot(1:n, LStrates, type="l", col="blue", xlab="Temps", ylab="Strates logarithmiques")

# pour les tests ADF et KPSS, on se contente ici de Type 1
# (Type 2 et Type 3 regardent la stationnarite des residus apres avoir estime une tendance (respectivement constante et lineaire)
# KPSS (Type 1) : non stationnaire
# ADF (Type 1) : non stationnaire pour des lags raisonnables (on regarde 3 ou 4, par exemple)
kpss.test(LStrates)
adf.test(LStrates)

# une representation graphique superposant la serie avec une moyenne lissee (fenetre de 80)
fen = 80
LStratesLiss = rep(0, n-fen)
for (i in 1:(n-fen)){
  LStratesLiss[i] = mean(LStrates[i:(i+fen)])
}
plot(1:n, LStrates, type="l", col="blue", xlab="Temps", ylab="Strates logarithmiques")
lines((fen/2+1):(n-fen/2), LStratesLiss, col="red", type="p")
# clairement, esperance non constante, confirmation de la non stationnarite

# tendance cubique estimee par OLS
Tps = 1:n
Tps2 = (1:n)^2
Tps3 = (1:n)^3
RL = lm(LStrates ~ Tps+Tps2+Tps3)
summary(RL)

# recuperation des coefficients estimes de la tendance et les valeurs reconstruires (Fit = Serie-Res)
Coef = RL$coef
EstTrend = RL$fitted.values
Res = LStrates-EstTrend
MSETendCub = sum(Res^2)

# superposition de la serie avec son estimation
plot(1:n, LStrates, type="l", col="blue", xlab="Temps", ylab="Strates logarithmiques")
lines(1:n, EstTrend, type="l", col="red")
# choix critiquable :
# approxime bien la tendance sur les observations
# mauvais en termes d'extrapolation : tend vers +inf à gauche et -inf à droite, incompatible avec le phenomene reel sous-jacent

# residus stationnaires (pour modelisation ARMA) ?
plot(Res, type="l", col="red", xlab="Temps", ylab="Residus de la modelisation")
kpss.test(Res)
adf.test(Res)
# selon les tests, oui

# graphiquement ? (attention a l'effet trompeur du a l'echelle des ordonnees)
fen = 80
ResLissM = rep(0, n-fen)
ResLissV = rep(0, n-fen)
for (i in 1:(n-fen)){
  ResLissM[i] = mean(Res[i:(i+fen)])
  ResLissV[i] = var(Res[i:(i+fen)])
}
plot(Res, type="l", col="red", xlab="Temps", ylab="Residus de la modelisation")
lines((fen/2+1):(n-fen/2), ResLissM, col="red", type="p")
lines((fen/2+1):(n-fen/2), ResLissV, col="blue", type="p")
# on ne peut pas observer graphiquement les covariances sur ce schema

# tendance sinusoidale estimee par OLS (+ grille)
# on fait varier thera pour trouver celui qui minimise le MSE (= somme des carres des residus)
MSEmin = +Inf
for (theta in 300:500){
  RL = lm(LStrates ~ cos(2*pi*Tps/theta))
  if (sum(RL$residuals^2) < MSEmin){
    MSEmin = sum(RL$residuals^2)
    thetamin = theta
  }
}
# on trouve thetamin = 415 ici
RL = lm(LStrates ~ cos(2*pi*Tps/thetamin))
summary(RL)

# recuperation des coefficients estimes de la tendance et les valeurs reconstruires
Coef = RL$coef
EstTrend = Coef[1] + Coef[2]*cos(2*pi*Tps/thetamin)
Res = LStrates-EstTrend
MSETendSin = MSEmin

# superposition de la serie avec son estimation
plot(1:n, LStrates, type="l", col="blue", xlab="Temps", ylab="Strates logarithmiques")
lines(1:n, EstTrend, type="l", col="red")
# choix critiquable :
# approxime moins bien la tendance sur les observations que la tendance cubique (comparer les deux MSE)
# bien meilleur en termes d'extrapolation : sous-entend que le phenomene reel sous-jacent se comporte de maniere periodique
print(paste("Tendance cubique : MSE = ", MSETendCub))
print(paste("Tendance sinuoidale : MSE = ", MSETendSin))

# residus stationnaires (pour modelisation ARMA) ?
kpss.test(Res)
adf.test(Res)
# selon les tests, oui (assez incertain pour KPSS, mais pas choquant)

# graphiquement ? (attention a l'effet trompeur du a l'echelle des ordonnees)
fen = 80
ResLissM = rep(0, n-fen)
ResLissV = rep(0, n-fen)
for (i in 1:(n-fen)){
  ResLissM[i] = mean(Res[i:(i+fen)])
  ResLissV[i] = var(Res[i:(i+fen)])
}
plot(Res, type="l", col="red", xlab="Temps", ylab="Residus de la modelisation")
lines((fen/2+1):(n-fen/2), ResLissM, col="red", type="p")
lines((fen/2+1):(n-fen/2), ResLissV, col="blue", type="p")
# on ne peut pas observer graphiquement les covariances sur ce schema

# on travaille maintenant avec le modele muni de la tendance sinusoidale
# idee : chercher les rangs ARMA qui minimisent l'AIC... et comparer avec d'autres modeles (significativite, residus "propres", ...)

# structure de correlation dans les residus : clairement ce n'est pas un bruit blanc
acf(Res, lag=20, main="ACF des residus")
pacf(Res, lag=20, main="PACF des residus")
# decroissance rapide en module : on part sur un ARMA
# forme de la PACF : peut-etre un AR pur

# on teste les premiers ARMA (de (1,0) a (5,5) par exemple) mais les MA purs sont inutiles vu l'ACF
# il ne faut pas etre trop gourmand dans le nombre de parametres, car n n'est pas infini
# on ne retient pas d'intercept, la serie est centree (mean(Res) est nul)
AICmin = +Inf
for (p in 1:5){
  for (q in 0:5){
    ARMA = arima(Res, order=c(p,0,q), include.mean=FALSE)
    if (ARMA$aic < AICmin){
      AICmin = ARMA$aic
      pmin = p
      qmin = q
    }
  }
    #plot(Res, type="l", col="black")
    #FitRes = Res-ARMA$residuals
    #lines(FitRes, col="red")
}
# p=4 et q=3 donnent l'AIC minimal
ARMA = arima(Res, order=c(pmin,0,qmin), include.mean=FALSE)
print(ARMA)

# on essaie avec auto.arima
auto.arima(Res, max.p=5, max.q=5, max.d=0, ic="aic")
ARMA12 = arima(Res, order=c(1,0,2), include.mean=FALSE)
ARMA43 = arima(Res, order=c(4,0,3), include.mean=FALSE)
# la solution proposée (p=1, q=2) ne minimise pas l'AIC puisqu'on vient de voir que c'etait (p=4, q=3)
# pourquoi ? la fonction auto.arima rejette les solutions ayant des racines trop proches du cercle unite
# on peut verifier que c'est le cas de la solution donnee pour p=4 et q=3
# auto.arima donne donc un modele legerement degrade sur l'AIC mais plus eloigne des racines unitaires

# on retient en premiere analyse ARMA(1,2) et ARMA(4,3)
Res12 = ARMA12$residuals
Res43 = ARMA43$residuals
Fit12 = Res-Res12
Fit43 = Res-Res43

# significativite :
# - tous les coefs sont significatifs dans l'ARMA(1,2)
# - seul le coef theta1 (le premier de la partie MA) est douteux pour l'ARMA(4,3)

# diagnostics de normalité des residus
hist(Res12, breaks=sqrt(n), main="Histogramme des residus", freq=FALSE, xlab="Residus", ylab="Densite")
hist(Res43, breaks=sqrt(n), main="Histogramme des residus", freq=FALSE, xlab="Residus", ylab="Densite")
qqnorm(Res12, main="QQ-plot des residus", xlab="Quantiles theoriques", ylab="Quantiles empiriques")
qqnorm(Res43, main="QQ-plot des residus", xlab="Quantiles theoriques", ylab="Quantiles empiriques")
plot(Res12/sd(Res12), type="p", main="Nuage de points des residus", xlab="Temps", ylab="Residus standardises")
abline(h=c(-1.96,1.96), col="red")
plot(Res43/sd(Res43), type="p", main="Nuage de points des residus", xlab="Temps", ylab="Residus standardises")
abline(h=c(-1.96,1.96), col="red")
shapiro.test(Res12)
shapiro.test(Res43)
# normalite : OK

# diagnostics de blancheur des residus
acf(Res12, main="ACF des residus")
pacf(Res12, main="PACF des residus")
acf(Res43, main="ACF des residus")
pacf(Res43, main="PACF des residus")
plot(Res12[1:(n-1)], Res12[2:n], type="p", main="", xlab="Residus de l'instant t-1", ylab="Residus de l'instant t")
plot(Res43[1:(n-1)], Res43[2:n], type="p", main="", xlab="Residus de l'instant t-1", ylab="Residus de l'instant t")
Box.test(Res12, lag=3)
Box.test(Res43, lag=3)
# blancheur : OK

# on reconstruit la serie avec la tendance sinusoidale et ces deux modeles pour les residus
FitGen12 = EstTrend+Fit12
FitGen43 = EstTrend+Fit43
plot(1:n, Strates, type="l", col="blue", xlab="Temps", ylab="Strates")
lines(1:n, exp(FitGen12), type="l", col="red")
lines(1:n, exp(FitGen43), type="l", col="magenta")
legend("topright", legend = c("Sin+ARMA(1,2)", "Sin+ARMA(4,3)"), col=c("red", "magenta"), lty=c(1,1))
MSE12 = sum((Strates - exp(FitGen12))^2)
MSE43 = sum((Strates - exp(FitGen43))^2)
print(paste("MSE de la reconstruction avec le modele ARMA(1,2) sur les residus = ", MSE12))
print(paste("MSE de la reconstruction avec le modele ARMA(4,3) sur les residus = ", MSE43))
# au sens de ce critere, on aurait tendance a preferer in fine le modele tendance sinusoidale+ARMA(4,3) sur le residu
# on pourrait evidemment continuer a creuser avec d'autres modelisations sur les residus...

