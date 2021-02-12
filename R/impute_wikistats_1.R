# courtesy from https://www.math.univ-toulouse.fr/~besse/Wikistat/pdf/st-scenar-app-idm.pdf

## Quant dat
dat <- read.table(here::here("data-raw", "Paris.dat"))

# Les données ne sont visiblement pas gaussiennes
boxplot(dat)
hist(dat[,1])
hist(dat[,50])

# Passage au log pour s’en approcher
dat=log(dat+1)

# Vérification visuelle
boxplot(dat)
hist(dat[,1])
hist(dat[,50])

# create missing vals
# initialisation du générateur
set.seed(42)
# Ratio de données manquantes
test.ratio=0.1
# Indices de l’échantillon test
IND=which(!is.na(dat),arr.ind=TRUE)
ntest=ceiling(dim(dat)[1]*test.ratio)
ind.test=IND[sample(1:dim(IND)[1],ntest),]
# Création des données manquantes
dat.test=dat[ind.test]
dat.train=dat
dat.train[ind.test]=NA

# 2.3 Imputation
# 2.3.1 LOCF
# chargement de la bibliothèque
library(zoo)
# Last Observation Carried Forward
dat.locf=na.locf(dat.train,na.rm=FALSE)
dat.locf=na.locf(dat.locf,na.rm=FALSE,
		 fromLast=TRUE)
# calcul de l’erreur
err.locf=abs(dat.test-dat.locf[ind.test])

# 2.3.2 Par la moyenne
# chargement de la bibliothèque
library(Hmisc)
dat.moy=impute(dat.train, fun=mean)
err.moy=abs(dat.test-as.matrix(dat.moy)[ind.test])

# 2.3.3 Par la médiane
med=apply(dat.train,1,median,na.rm=TRUE)
dat.med=dat.train
ind.na=which(is.na(dat.med),arr.ind=TRUE)
dat.med[ind.na]=med[ind.na[,1]]
err.med=abs(dat.test-dat.med[ind.test])

# 2.3.4 k plus proches voisins kNN
# chargement de la bibliothèque
library(VIM)
dat.kNN=kNN(dat.train, k=5, imp_var=FALSE)
err.kNN=abs(dat.test-dat.kNN[ind.test])

# 2.3.5 LOESS
# chargement de la bibliothèque
library(locfit) # Local Regression, Likelihood and Density Estimation
dat.imputed=rbind(colnames(dat.train),dat.train)
indices=1:nrow(dat.train)
dat.loess= apply(dat.imputed, 2, function(j) {
	predict(locfit(j[-1] ~ indices), indices)
})
err.loess=abs(dat.test-dat.loess[ind.test])

# 2.3.6 SVD
# chargement de la bibliothèque
# library(bcv) # Biased Cross-Validation for Bandwidth Selection # CRAN archived the 01 02 21
# dat.SVD=impute.svd(dat.train,k=3,maxiter=1000)$x
# err.svd=abs(dat.test-dat.SVD[ind.test])

# 2.3.7 missForest
# chargement de la bibliothèque
library(missForest)
dat.missForest<-missForest(dat.train,maxiter=10,
			   ntree = 200, variablewise = TRUE)$ximp
err.missForest=abs(dat.test - dat.missForest[ind.test])

# 2.3.8 AmeliaII
# chargement de la bibliothèque
library(Amelia)
dat.amelia=amelia(dat.train,m=1)$imputations$imp1
err.amelia=abs(dat.test-dat.amelia[ind.test])

# 2.4 Comparaison des résultats
# Erreurs de complétion sur l’échantillon test
boxplot(data.frame(err.locf, err.moy,
		   err.med, err.kNN, err.loess, # err.svd,
		   err.missForest,err.amelia), ylim=c(0,4))

library(ggplot2)
library(dplyr)
data.frame(err.locf, err.moy,
	   err.med, err.kNN, err.loess, # err.svd,
	   err.missForest, err.amelia) %>% 
	tidyr::pivot_longer(data = ., cols = everything(), 
			    names_to = "model", values_to = "l1_error") %>%
	group_by(model) %>% 
	mutate(m_error = mean(l1_error)) %>% 
	ggplot(aes(x = forcats::fct_reorder(model, m_error), y = l1_error)) +
	geom_boxplot() +
	geom_point(aes(y = m_error), color = "red") +
	geom_text(aes(y = m_error, label = round(m_error, 2) ),
		  nudge_x = .2, nudge_y = .2, size = 3, color = "red") +
	coord_flip() +
	labs(title = "Boxplot of errors per imputation methods",
	     y = "Error L1", x = "Methods") +
	theme_bw()


