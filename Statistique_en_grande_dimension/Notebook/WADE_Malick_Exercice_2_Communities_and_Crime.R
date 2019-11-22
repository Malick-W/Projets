library(glmnet)
library(ggplot2) # pour des grpahes plus jolies

df = read.csv("communities_data_R.csv", sep = ",", header = T)

# On fait du Lasso avec alpha = 1
# Une grille de lambda est automatiquement determinÃ©e, en partant du plus petit lambda, annulant tous les
# coefficients( sauf la constante), jusqu'Ã  une centiÃ©me valeur de lambda (avec l'option par defaut nlambda=100)
# lambdas = 10^seq(3, -2, by = -.1)

df = df[-c(1)]

x = df[,ncol(df)]
y = 

LASSO_ex1=glmnet(x,y, alpha = 1)


## question 3
LASSO_ex1
# Affiche en fonction de lambda le nombre de variables selectionnÃ© et le pourcentage de variance expliquÃ©
# On peut donner manuellement une grille de valeur pour lambda


## question 4
# GÃ©nÃ©ration du "chemin de rÃ©gularisation"
plot(LASSO_ex1) #xvar = c("norm", "lambda", "dev")

which(coef(LASSO_ex1, s=0.32370) !=0) # les coef de teta non nulls


## question 5
v=coef(LASSO_ex1, s=0.32370) 
v[1:100]


## question 6
Ypred = predict(LASSO_ex1,x, s=0.3) # refaire prÃ©diction avec s="lambda.min" de la question 7

ErreurPred = sqrt(sum((Ypred-y)^2)/n)
ErreurPred


## question 7
cvLASSO_ex1=cv.glmnet(x,y, alpha=1, type.measure="mse") # , type.measure="mse", "mae", "auc
plot(cvLASSO_ex1)

#L'erreur MSE  minimum de la cross-validated est:
min(cvLASSO_ex1$cvm)
# avec un lambda:
meilleur_Lambda = cvLASSO_ex1$lambda.min
meilleur_Lambda

# lambda.1se : la plus grande valeur de lambda de sorte que l'erreur se situe Ã  moins d'une erreur-type du minimum.
lambda.1se = cvLASSO_ex1$lambda.1se
lambda.1se

## question 8
# a)
Xtest = matrix(rnorm(n*p),n,p)
Ytest = Simul(n,p,theta,sigma,Xtest)

# b)
Ypred=predict(LASSO_ex1, Xtest, s=meilleur_Lambda)

# c) Erreur quadratique moyenne 
MSE_lasso=(sum((Ypred-Ytest)^2))/n
MSE_lasso
R2_Lasso=1-(sum(Ytest-Ypred)^2)/sum(Ytest^2)
R2_Lasso

# d) prÃ©diction avec le modÃ¨le lm classique avec les variables sÃ©lectionnÃ©es par le LASSO
teta = coef(cvLASSO_ex1,s=meilleur_Lambda)
Var = which(teta!=0)
LM_ex1 = lm (y~x[,Var])
Ypred_LM = predict(LM_ex1, data = Xtest) # Ypred_LM = teta[1]+Xtest[,Var]%*%teta[-1] 

MSE_LM = (sum(Ytest-Ypred_LM)^2)/n
MSE_LM
R2_LM = 1-(sum(Ytest-Ypred_LM)^2)/sum(Ytest^2)
R2_LM

sprintf("R2_Lasso: %s", round(R2_Lasso,3))
sprintf("R2_LM: %s", round(R2_LM,3))

sprintf("MSE_Lasso: %s", round(MSE_lasso,3))
sprintf("MSE_LM: %s", round(MSE_LM,3))

## question 8  Ridge
Ridge_ex1=glmnet(x,y,alpha=0, lambda = 10*seq(0,10,0.01))
plot(Ridge_ex1, xvar="lambda", label=TRUE) #xvar = c("norm", "lambda", "dev")

cvRidge_ex1=cv.glmnet(x,y, alpha=0,lambda = 20*seq(0,5,0.005))
plot(cvRidge_ex1)

meilleur_Lambda = cvRidge_ex1$lambda.min
meilleur_Lambda

Xtest = matrix(rnorm(n*p),n,p)
Ytest = Simul(n,p,theta,sigma,Xtest)

# b)
Ypred=predict(LASSO_ex1, Xtest, s=meilleur_Lambda)

# c) Erreur quadratique moyenne 
MSE_Ridge = (sum((Ypred-Ytest)^2))/n
MSE_Ridge
R2_Ridge = 1-(sum(Ytest-Ypred)^2)/sum(Ytest^2)
R2_Ridge