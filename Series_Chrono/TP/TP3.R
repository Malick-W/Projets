
# simulation d'un ARMA de taille n, centre en m, de parametres Phi et Theta, a bruit gaussien de variance s2
simulerARMA = function(n, m, Phi, Theta, s2){
  
  # attention : rnorm prend l'ecart-type en parametre et non la variance
  E = rnorm(n, 0, sqrt(s2))
  X = rep(0, n)
  
  # recuperation des rangs p et q
  p = length(Phi)
  q = length(Theta)
  
  # les max(p,q) premieres valeurs doivent etre initialisees
  # on peut les laisser nulles, on peut les mettre egales au bruit, ... choix de l'utilisateur
  X[1:max(p,q)] = E[1:max(p,q)]
  
  for (i in (max(p,q)+1):n){
    # on recupere les valeurs precedentes pour l'autoregression (si p>=1)
    if (p >= 1){
      valPrecX = t(Phi)%*%(X[(i-1):(i-p)]-m)
    } else{
      valPrecX = 0
    }
    
    # on recupere les valeurs precedentes pour la moyenne mobile (si q>=1)
    if (q >= 1){
      valPrecE = t(Theta)%*%E[(i-1):(i-q)]
    } else{
      valPrecE = 0
    }
  
    # recurrence ARMA
    X[i] = m + valPrecX + valPrecE + E[i]
  }
  
  # graphique
  plot(X, type="l", xlab="Temps", ylab="Serie X", col="blue")
  
  return(X)
}


# simulation d'un ARMA de taille n, centre en m, de parametres Phi et Theta, a bruit gaussien de variance s2
tracerCorr = function(X, lag){
  
  # partage en deux de la fenetre graphique
  par(mfrow = c(1,2))
  acf(X, lag.max=lag, ylim=c(-1,1), main="ACF de la serie", xlab="h", ylab="ACF empirique")
  pacf(X, lag.max=lag, ylim=c(-1,1), main="PACF de la serie", xlab="h", ylab="PACF empirique")
}


# teste la significativite des parametres dans le modele ARMA, au risque alpha
significativite = function(ARMA, alpha){
  
  # recuperation des estimations
  Est = ARMA$coef
  
  # recuperation des ecarts-types estimes
  EstSd = sqrt(diag(ARMA$var.coef))
  
  # affichage des conclusions
  print(abs(Est/EstSd) > qnorm(1-alpha/2))
}


# affiche un diagnostic de normalite
outilsNormalite = function(Res){
  
  n = length(Res)
  
  # partage en trois de la fenetre graphique
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  hist(Res, breaks=sqrt(n), xlab="Residus", ylab="Densite empirique", main="", freq=FALSE)
  qqnorm(Res, xlab="Quantiles theoriques", ylab="Quantiles empiriques", main="")
  plot(Res/sd(Res), type="p", xlab="Temps", ylab="Residus", main="")
  abline(h=c(-1.96, 1.96), col="red")
}


# affiche un diagnostic de normalite
outilsBlancheur = function(Res, lag){
  
  n = length(Res)
  
  # partage en trois de la fenetre graphique
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  plot(Res, type="l", col="red", xlab="Temps", ylab="Residus")
  plot(Res[1:(n-1)], Res[2:n], type="p", main="", xlab="residus t-1", ylab="residus t")
  acf(Res, lag.max=lag, ylim=c(-1,1), main="ACF des residus", xlab="h", ylab="ACF empirique")
  pacf(Res, lag.max=lag, ylim=c(-1,1), main="PACF des residus", xlab="h", ylab="PACF empirique")
}

# simulation d'un ARMA
n = 200
Phi = c(0.8)
Theta = c(0.5)
m = -1
s2 = 0.5
X = simulerARMA(n, m, Phi, Theta, s2)

# ACF et PACF
tracerCorr(X, 25)

# estimation ARMA, residus et fitted values
ARMA = arima(X, order=c(1,0,1))
Res = ARMA$residuals
Fit = X-Res

# superposition serie/reconstruction
dev.off()
plot(X, type="l", xlab="Temps", ylab="Serie X", col="blue")
lines(Fit, type="l", xlab="Temps", ylab="Serie X", col="red", lty=2)

# test de significativite des coefficients
alpha = 0.05
significativite(ARMA, alpha)

# diagnostic sur les residus
outilsNormalite(Res)
outilsBlancheur(Res, 20)
Box.test(Res, lag=3, type="Ljung-Box")
shapiro.test(Res)

