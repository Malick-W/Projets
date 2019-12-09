path_Conso = "/users/mmath/wade/Bureau/Projets_Malick_fayad/Series_Chrono_Malick_fayad/projet/Conso.RData"
path_Temp = "/users/mmath/wade/Bureau/Projets_Malick_fayad/Series_Chrono_Malick_fayad/projet/Temp.RData"

load(path_Conso)
load(path_Temp)

LConso = log(Conso)

# trace des donnees
plot(Conso, type="l", xlab="temps(Heure)", ylab="Consommation")
plot(LConso, type="l", xlab="temps(Heure)", ylab="Log_Consommation")

# trace des donnees
plot(Temp, type="l",xlim=c(1,96), xlab="temps(Heure)", ylab="Température")


plot(Temp[1:1500], Conso[1:1500], main="The Sine Function", xlab="temp(Heure)", ylab="Conso")


# objectif : modeliser la serie logarithmique par un modele additif et effectuer une prediction sur l'annee suivante

# autocorrelations empiriques de la series
acf(Conso, main="ACF empirique")

# notre choix se porte sur une periode de 12
# (une periode de 6 peut etre retenue mais l'experience montre qu'elle ne permet pas de rendre la serie aperiodique)
tau = 24
m = tau/2
nsup = length(Conso)%%tau
n = length(Conso)-nsup # on se ramene a un nombre entier de periodes (a eviter lorsque les donnees sont rares)
ResConso = Conso[(n+1):length(Conso)]
Conso = Conso[1:n]
ResConso = Conso[(n+1):(n+nsup)]
Conso = Conso[1:n]

# filtrage par moyenne mobile arithmetique modifiee d'ordre 6
MMConso = rep(0, n-2*m)
for (i in 1:(n-2*m)){
  MMConso[i] = (Conso[i] + Conso[i+2*m])/(4*m) + sum(Conso[(i+1):(i+2*m-1)])/(2*m)
}

# representation de la serie filtree : elle est visuellement aperiodique
plot(MMConso, type="l", xlab="t", ylab="Log Ventes apres moyenne mobile")

# il existe manifestement une tendance, pour l'estimer on retient 4 modeles :
# - M1 : une regression lineaire sur {1, t, t^2} pour la forme parabolique, estimee sans memoire du bruit (OLS)
# - M2 : une regression lineaire sur {1, t}, estimee sans memoire du bruit (OLS)
# - M3 : une regression lineaire sur {1, t, t^2}, estimee avec un bruit a courte memoire (GLS-MA(1))
# - M4 : une regression lineaire sur {1, t, t^2}, estimee avec un bruit a longue memoire (GLS-AR(1))

# espace des temps observables (prive des m premieres et dernieres valeurs)
Tps = 1:(n-2*m)
Tps2 = Tps^2

# M1
RegLinM1 = lm(MMConso ~ 1+Tps+Tps2)
summary(RegLinM1)
b0M1 = RegLinM1$coefficients[1]
b1M1 = RegLinM1$coefficients[2]
b2M1 = RegLinM1$coefficients[3]
lines(b0M1 + b1M1*Tps + b2M1*Tps2, type="l", col="red")

# M2
RegLinM2 = lm(MMConso ~ 1+Tps)
summary(RegLinM2)
b0M2 = RegLinM2$coefficients[1]
b1M2 = RegLinM2$coefficients[2]
lines(b0M2 + b1M2*Tps, type="l", col="blue")

# package pour les GLS
library("nlme")

# M3
RegLinGenM3 = gls(MMConso ~ 1+Tps+Tps2, correlation = corARMA(p=0, q=1))
b0M3 = RegLinGenM3$coefficients[1]
b1M3 = RegLinGenM3$coefficients[2]
b2M3 = RegLinGenM3$coefficients[3]
lines(b0M3 + b1M3*Tps + b2M3*Tps2, type="l", col="magenta")

# M4
RegLinGenM4 = gls(MMConso ~ 1+Tps+Tps2, correlation = corARMA(p=1, q=0))
b0M4 = RegLinGenM4$coefficients[1]
b1M4 = RegLinGenM4$coefficients[2]
b2M4 = RegLinGenM4$coefficients[3]
lines(b0M4 + b1M4*Tps + b2M4*Tps2, type="l", col="forestgreen")

# pour chacun des modeles, on recupere l'estimation de la tendance sur tout l'espace des temps
EstM1 = b0M1 + b1M1*(1:n) + b2M1*(1:n)^2
EstM2 = b0M2 + b1M2*(1:n)
EstM3 = b0M3 + b1M3*(1:n) + b2M3*(1:n)^2
EstM4 = b0M4 + b1M4*(1:n) + b2M4*(1:n)^2

# superposition du signal et de la tendance estimee
plot(Conso, type="l", xlab="t", ylab="Log Ventes")
lines(EstM1, type="l", col="red")
lines(EstM2, type="l", col="blue")
lines(EstM3, type="l", col="magenta")
lines(EstM4, type="l", col="forestgreen")

# recuperation du signal prive de sa tendance, pour estimer la saisonnalite
SaisM1 = Conso - EstM1
SaisM2 = Conso - EstM2
SaisM3 = Conso - EstM3
SaisM4 = Conso - EstM4

# representation des signaux prives de leur tendance
plot(SaisM1, type="l", xlab="t", ylab="Log Ventes sans tendance", col="red")
lines(SaisM2, type="l", col="blue")
lines(SaisM3, type="l", col="magenta")
lines(SaisM4, type="l", col="forestgreen")

# le motif periodique est un vecteur de taille 12
MotifM1 = rep(0, tau)
MotifM2 = rep(0, tau)
MotifM3 = rep(0, tau)
MotifM4 = rep(0, tau)

# on moyennise toutes les periodes extraites des signaux
for (k in 1:tau){
  Extr = SaisM1[seq(k, n, by=tau)]
  MotifM1[k] = mean(Extr)
  Extr = SaisM2[seq(k, n, by=tau)]
  MotifM2[k] = mean(Extr)
  Extr = SaisM3[seq(k, n, by=tau)]
  MotifM3[k] = mean(Extr)
  Extr = SaisM4[seq(k, n, by=tau)]
  MotifM4[k] = mean(Extr)
}

# on recentre le motif pour qu'il satisfasse la contrainte d'identifiabilite du modele
MotifM1 = MotifM1 - mean(MotifM1)
MotifM2 = MotifM2 - mean(MotifM2)
MotifM3 = MotifM3 - mean(MotifM3)
MotifM4 = MotifM4 - mean(MotifM4)

# representation des motifs periodiques estimes
plot(MotifM1, type="l", xlab="t", ylab="Motif periodique", col="red")
lines(MotifM2, type="l", col="blue")
lines(MotifM3, type="l", col="magenta")
lines(MotifM4, type="l", col="forestgreen")

# estimation de la saisonnalite par duplication du motif
EstSaisM1 = rep(MotifM1, n/tau)
EstSaisM2 = rep(MotifM2, n/tau)
EstSaisM3 = rep(MotifM3, n/tau)
EstSaisM4 = rep(MotifM4, n/tau)

# superposition du signal et de la somme de la tendance et de la saisonnalite estimees
plot(Conso, type="l", xlab="t", ylab="Log Ventes")
lines(EstM1+EstSaisM1, type="l", col="red")
lines(EstM2+EstSaisM2, type="l", col="blue")
lines(EstM3+EstSaisM3, type="l", col="magenta")
lines(EstM4+EstSaisM4, type="l", col="forestgreen")

# recuperation de la fluctuation residuelle
ResM1 = Conso - EstM1 - EstSaisM1
ResM2 = Conso - EstM2 - EstSaisM2
ResM3 = Conso - EstM3 - EstSaisM3
ResM4 = Conso - EstM4 - EstSaisM4

# calcul de l'erreur MSE commise lorsqu'on estime le signal par la somme de sa tendance et de sa saisonnalite
MSEM1 = mean((ResM1)^2)
MSEM2 = mean((ResM2)^2)
MSEM3 = mean((ResM3)^2)
MSEM4 = mean((ResM4)^2)

# le modele M4 semble le meilleur, au sens de ce critere
# au contraire, le modele M2 semble le moins bon, ce qui etait attendu (la droite est clairement moins adaptee que la parabole pour modeliser la tendance)

# autocorrelations empiriques des residus pour detecter des correlations et/ou une saisonnalite non eliminee
acf(ResM1, main="ACF empirique")
acf(ResM2, main="ACF empirique") # presence de correlations ici, raison supplementaire pour rejeter le modele M2 par rapport aux autres
acf(ResM3, main="ACF empirique")
acf(ResM4, main="ACF empirique")

# on souhaite predire l'annee suivante
NTps = (n+1):(n+tau)
PredM1 = b0M1 + b1M1*NTps + b2M1*NTps^2 + MotifM1
PredM2 = b0M2 + b1M2*NTps + MotifM2
PredM3 = b0M3 + b1M3*NTps + b2M3*NTps^2 + MotifM3
PredM4 = b0M4 + b1M4*NTps + b2M4*NTps^2 + MotifM4

# representation du signal et de nos previsions par les 4 modeles
plot(1:n, Conso, type="l", xlab="t", ylab="Log Ventes", xlim=c(1, n+tau))
lines((n+1):(n+nsup), ResConso, type="l", col="black", lty=2) 
lines(NTps, PredM1, type="l", col="red")
lines(NTps, PredM2, type="l", col="blue")
lines(NTps, PredM3, type="l", col="magenta")
lines(NTps, PredM4, type="l", col="forestgreen")

# representation du signal initial et de nos previsions par les 4 modeles
plot(1:n, Conso, type="l", xlab="t", ylab="Ventes", xlim=c(1, n+tau))
lines((n+1):(n+nsup), ResConso, type="l", col="black", lty=2) 
lines(NTps, exp(PredM1), type="l", col="red")
lines(NTps, exp(PredM2), type="l", col="blue")
lines(NTps, exp(PredM3), type="l", col="magenta")
lines(NTps, exp(PredM4), type="l", col="forestgreen")

# notre etude montre que la prevision par M4 semble la plus pertinente

# remarque : on verra dans la suite du cours qu'il n'est pas necessairement pertinent de predire exp(Conso) par exp(Prediction)

# on illustre pour conclure l'application de la procedure "decompose" qui realise la modelisation additive
ConsoTS = ts(Conso, frequency=tau)
Decomp = decompose(ConsoTS)
plot(Decomp)

# comparaison entre le motif periodique issu de "decompose" et celui de notre meilleur modele
plot(Decomp$figure, xlab="t", ylab="Motif periodique", type="l")
lines(MotifM4, col="forestgreen")

# plus rapide ?
# par contre, R n'estime pas la tendance : donc, pas de prediction !
