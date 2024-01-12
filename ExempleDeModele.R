#### Chargement des librairies + données ####
library(tidyverse)
library(keras)
cible <- "turb"

library(vroom)
echantillons <- vroom("AtlasDeLEau/qualite_eau_complet.csv")

qualite_eau <- echantillons %>% 
  mutate(annee = year(date)) %>% 
  mutate(mois = month(date)) %>% 
  filter(mois >= 6, mois <= 9) %>% 
  filter(annee >= 2016) %>% # Pas de données Sentinel 2 avant...
  mutate(ID = paste0(no_station,"_",annee)) %>% 
  mutate(nom_fichier = paste0(ID,".png")) %>% 
  drop_na(cible)

qualite_eau[cible] <-  scale(qualite_eau[cible]) # Centrer et réduire pour faciliter les calculs...

#### Préparation des générateurs d'images ####

dossier_images <- "Imagerie/3km/"
colonne_noms_fichiers <- "nom_fichier"
rescale_factor <- 1/255
validation_fraction <- 0.2
image_dimensions <- c(294,201)
random_seed <- runif(1,0,10000) %>% round

training_generator <- flow_images_from_dataframe(
  qualite_eau,
  directory = dossier_images,
  x_col = colonne_noms_fichiers,
  y_col = cible,
  generator =  image_data_generator(rescale = rescale_factor, validation_split = validation_fraction),
  target_size = image_dimensions,
  class_mode = "other",
  subset = "training",
  seed = random_seed
)

validation_generator <- flow_images_from_dataframe(
  qualite_eau,
  directory = dossier_images,
  x_col = colonne_noms_fichiers,
  y_col = cible,
  generator =  image_data_generator(rescale = rescale_factor, validation_split = validation_fraction),
  target_size = image_dimensions,
  class_mode = "other",
  subset = "validation",
  seed = random_seed
)


#### Préparation et ajustement du modèle ####
# https://link.springer.com/article/10.1007/s12517-021-08555-5
model <- keras_model_sequential() %>%
  # ConvNet
  layer_conv_2d(filters = 32,kernel_size = c(3,3),activation = "relu",input_shape = c(image_dimensions,3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 64,kernel_size = c(3,3),activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 128,kernel_size = c(3,3),activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 256,kernel_size = c(3,3),activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 512,kernel_size = c(3,3),activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  # Flatten and predict
  layer_flatten() %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 1)

model

r2 <- custom_metric("r2", function(y_true, y_pred) {
  SS_res =  k_sum(k_square( y_true-y_pred ))
  SS_tot = k_sum(k_square( y_true - k_mean(y_true) ) )
  return ( 1 - SS_res/(SS_tot + k_epsilon()) )
})

model %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae",r2)
)

#### Exploration des résultats ####
history <- model %>% 
  fit(
    x = training_generator,
    validation_data = validation_generator,
    epochs = 5
  )

history