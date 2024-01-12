library(terra)
library(tidyverse)
library(ranger)
library(vroom)
library(party) ## package to run conditional inference random forest
library(caret) ## provides a wrapper for training forests and calculating R2
library(tidyverse)
library("mgcv")
library(permimp)
library(pdp)
library(visreg)
library(itsadug)
library(gridExtra)


seed<-set.seed(2024)
n_values <- 10*10*3


#load and clean water quality data

echantillons <- vroom("AtlasDeLEau/qualite_eau_complet.csv")

qualite_eau <- echantillons %>% 
  mutate(annee = year(date)) %>% 
  mutate(mois = month(date)) %>% 
  filter(mois >= 6, mois <= 9) %>% 
  filter(annee >= 2016) %>% # Pas de données Sentinel 2 avant...
  mutate(ID = paste0(no_station,"_",annee)) %>% 
  mutate(nom_fichier = paste0(ID,".png")) %>% 
  select_if(~sum(!is.na(.)) > 0)%>% 
  select( -c(1,2,4,5,27,34,38:41,50:70,72))

qualite_eau<- qualite_eau[, colSums(is.na(qualite_eau)) <= (0.80 * nrow(qualite_eau))]
qualite_eau <- qualite_eau[rowSums(is.na(qualite_eau)) <= (0.50 * ncol(qualite_eau)), ]

qualite_eau<-qualite_eau%>%
  group_by(no_station)%>% 
  summarise_if(is.numeric,mean, na.rm=T)


#couleur moyenne
donnees_couleurs <- matrix(ncol = 3, nrow = nrow(qualite_eau))

for (i in 1:nrow(qualite_eau)) {
  x <- qualite_eau %>% slice(i)
  r <- rast(paste0("/Users/cindypaquette/Documents/postdoc/RIVE hacking/Projets/QualiteEau/Imagerie/1km/",x$ID,".png"))
  donnees_couleurs[i,1]<-mean(values(r[[1]])) #Red
  donnees_couleurs[i,2]<-mean(values(r[[2]])) #Green
  donnees_couleurs[i,3]<-mean(values(r[[3]])) #Blue
  
}

donnees_couleurs<-as.data.frame(donnees_couleurs)
donnees_couleurs$no_station=qualite_eau$no_station
donnees_couleurs$ID=qualite_eau$ID

colnames(donnees_couleurs) <- c('Red','Green','Blue','no_station',"ID")

donnees_couleurs<-donnees_couleurs%>%
  group_by(no_station)%>% 
  summarise_if(is.numeric,mean, na.rm=T)


#load land use data

connect <- read.csv('/Users/cindypaquette/Documents/postdoc/RIVE hacking/Projets/QualiteEau/Jade.csv', sep=";") 
colnames(connect)[1]<-"no_station" 
connect$no_station<-paste0("0",connect$no_station)

#merge water quality, land use and color data

data<-left_join(donnees_couleurs,connect)
data2<-left_join(data,qualite_eau[c(1,4,5,11,18)])
data2$no_station=NULL
data2<-data2%>%
  select(Red:Blue,Milieu.agricole....:Non.classifié....,cod)%>%
  filter(!is.na(cod))


####Random forest####

#will do the quick method with m.try = sqrt number of variables
seed <- 2021
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid") ## defines control parameters, in this case a 10-fold cross-validation and 3 repeats.
metric <- "RMSE" ## metric choices for regression are RMSE and Rsquared
tunegrid <- expand.grid(.mtry=c(1:8)) ## note: basic heuristic for max mtry is sqrt # of features (i.e. columns)
par(mar=c(4,10,1,1))

# Ideally mtry should be tuned by means of, e.g., cross-validation.
#for quick version, set mtry to
ceiling(sqrt(ncol(data2))) # = 7 #default in cforest remove LCE, coords, 4 connectivity variables, survey an full com
mtry.best=5


## Define training dataset subset
set.seed(seed)
sampled <- sample(nrow(data2 ), round(nrow(data2 )*.75)) ## subsets 75% of the data
data2.train <- data2 [sampled,]
data2.test <- data2 [-sampled,]

## Run optimized model on training dataset
rf.train <- cforest(ph ~ ., data=data2.train, controls = cforest_unbiased(ntree = 1000, mtry=mtry.best))


## Run optimized model on full dataset
rf.test <- predict(rf.train, newdata = data2.test) ## use predict.train() on train() results
## Calculate R2 for full model
r2<-round(postResample(pred = rf.test, obs = data2.test$ph)[2],4) 
r2 <- bquote(italic(r)^2~'(pred vs. obs)'==.(r2))



## Variable importance
set.seed(seed)

vars <- varimp(rf.train, conditional = F)
#vars[vars < 0] <- 0
vars <- (vars/sum(vars))*100
vars <- sort(vars)
barplot(vars,col="royalblue3",border=0,horiz=T,las=1,cex.names=0.8) #also needs finite values
legend('bottomright',bty='n',legend=r2)
title(xlab='relative influence (%)')
