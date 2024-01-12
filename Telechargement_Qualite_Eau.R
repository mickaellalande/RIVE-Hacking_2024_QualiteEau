library(tidyverse)
library(geojsonR)
library(readxl)

# On obtient la liste des stations ici : 
# Source : https://www.donneesquebec.ca/recherche/dataset/suivi-physicochimique-des-rivieres-et-du-fleuve
x <- FROM_GeoJson("donnees/physicochimie_stations_p.json")
# Et pour chaque station, on a l'url d'un fichier Excel avec les mesures détaillées

#### Télécharger tous les fichiers Excel ######
map(x$features, function(feature) {
  file_name <- paste0("grosses_donnees/excel_atlas/",feature$properties$NO_STATION,".xlsx")
  if (!(file.exists(file_name))) {
    
    if (!(is.null(feature$properties$URL_ZGIEBV))) {
      download.file(
        feature$properties$URL_ZGIEBV, 
        file_name, 
        mode = "wb"
      )
      Sys.sleep(rnorm(1,10,3))  
    }
    
    
  }
})

#### Préparer le fichier de données d'entraînement complet, au long ####

# Attention, dans le fichier Excel, il y a au final plusieurs
# numéros de stations pour un même fichier

complet <- map_df(x$features, function(feature){

  # À vérifier plus tard...
  if (feature$properties$NO_STATION %in% c("08070231","08070475","08070476","04E20002","04C20017")) {
    return()
  }
    
  file_name <- paste0("grosses_donnees/excel_atlas/",feature$properties$NO_STATION,".xlsx")

  print(file_name)
  
  if (file.exists(file_name)) {
    dt <- read_excel(
      file_name,
      sheet = "Données transposées"
    )
    
    s <- read_excel(
      file_name,
      sheet = "Station(s)"
    )
    
    dt %>% 
      left_join(s, by = "N° STATION") %>% drop_na(`N° STATION`)  
  }
  
})

complet <- complet %>% 
  rename(
    "no_labo" = "N° LABO",
    "no_projet" = "N° PROJET",
    "no_station" = "N° STATION",
    "date" = "DATE"                     ,
    "heure" = "HEURE"                    ,
    "alc" = "ALC (mg/l)"              ,
    "cat"="CA (mg/l)"                ,
    "cf"="CF (UFC/100 ml)"          ,
    "chl_aa"="CHL-AA (µg/l)"           ,
    "clo"="CLO (UFC/100 ml)"         ,
    "cod"="COD (mg/l)"               ,
    "cond"="COND (µS/cm)"            ,
    "couv"="COU (UCV)"                ,
    "dbo5"="DBO5 (mg/l)"              ,
    "ent"="ENT (UFC/100 ml)"        ,
    "mg"="MG (mg/l)"                ,
    "nh3"="NH3 (mg/l)"               ,
    "nox"="NOX (mg/l)"              ,
    "ntot"="NTOT (mg/l)"              ,
    "od"="OD (mg/l)"                ,
    "opo4"="OPO4 (mg/l)"             ,
    "p_t_per"="P-T-PER (mg/l)"           ,
    "p_t_tra"="P-T-TRA (µg/l)"           ,
    "ph"="PH (pH)"                 ,
    "pheo"="PHEO (µg/l)"              ,
    "ptotd"="PTOTD (mg/l)"             ,
    "ptots"="PTOTS (mg/l)"            ,
    "so4"="SO4 (mg/l)"               ,
    "ss"="SS (mg/l)"                ,
    "temp"="TEMP (°C)"               ,
    "turb"="TURB (UTN)"               ,
    "durete"="DURETÉ CALCULÉE (mg/l)"   ,
    "iqbp6"="IQBP6"                   ,
    "vdec"="VDEC"                     ,
    "si_cf"="siCF"                     ,
    "si_chlaa"="siCHLAA"                 ,
    "si_nh3"="siNH3"                    ,
    "si_nox"="siNOX"                    ,
    "si_ptot"="siPTOT"                  ,
    "si_ss"="siSS"                     ,
    "description"="DESCRIPTION"              ,
    "latitude"="LATITUDE"                ,
    "longitude"="LONGITUDE"                ,
    "superficie_drainee"="SUPERFICIE DRAINÉE (km²)" ,
    "no_carte"="N° CARTE"                ,
    "nb_ech"="NB. ÉCH."                 ,
    "du"="DU"                       ,
    "au"="AU"                      ,
    "cond_t"="COND-T (µS/cm)"           ,
    "ec_mtec"="EC-MTEC (UFC/100 ml)"     ,
    "k"="K (mg/l)"                ,
    "na"="NA (mg/l)"                ,
    "niveau"="NIVEAU (m)"               ,
    "odsat"="ODSAT (%)"               ,
    "p_bio"="P-BIO (mg/l)"             ,
    "ph_t"="PH-T (pH)"                ,
    "prof"="PROF (m)"                ,
    "salinit"="SALINIT (ppt)"            ,
    "ct"="CT (UFC/100 ml)"          ,
    "ec_0157"="EC-O157 (PRÉ-ABS/L)"     ,
    "al"="AL (mg/l)"                ,
    "cl"="CL (mg/l)"                ,
    "fe"="FE (mg/l)" 
  )

nrow(complet)
nrow(distinct(complet))

x <- complet %>% 
  filter(no_station=="0000097A")

# Les mêmes mesures sont présentes dans plusieurs fichiers... d'où le distinct
write_csv(complet %>% distinct(), "grosses_donnees/qualite_eau_complet.csv")
