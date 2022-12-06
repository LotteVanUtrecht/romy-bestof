
# ----Definieer functies------------------------------------------------

#Hai lotte, ik probeer iets te pushen 
factoriseer_eencijferdata <- function(path_data,
                                     path_domeinwaarden="Data/P_DOMEINWAARDEN.csv",
                                     cols_of_interest="ALL"){



  

data_prep <- function(refresh=TRUE){
  # doel:   preparatie van data in een dataframe, dit df wordt gesynthetiseerd 
  # input:  refresh=FALSE haalt een eerder opgeslagen .RDS op. 
  # output: observed_data, de originele data in een datafram 
  
  
  if (refresh==TRUE){
    #data parameters definiëren 
    data_params <<- list(schooljaar = 2010,
                        cols_of_interest = c("gemnr","gtlln","brin","vest","typ", "sexe","apli",
                                           "apnw","apuk", "own"))
    
    path_data  <- paste0("Data/origineel/VO_cohort_", data_params$schooljaar, ".Rds")
    path_data_csv <- paste0("Data/origineel/VO_cohort_", data_params$schooljaar, ".csv")
  
      
    ### Directories aanmaken------------------------------------------------
    
    for (path in c("Data", "Data/Origineel", "Data/Synthetisch")){
      if (!dir.exists(file.path(getwd(), path))){
        dir.create(file.path(getwd(), path))
      }
    }
    
    ### Importeren relevante functies---------------------------------------------
    source("1sup_collect_leerling_data.R") # Verzamelt cohort data:
    source("1sup_collect_brin_data.R") # Verzamelt brin data:
    source("helper_functions/factorisatie_eencijferdata.R")  # functie: Factoriseert 1cijferdata op basis van P-Domeinwaarden
    
    
    ### Importeren data-----------------------------------------------------------
    
    # haalt locatie van data op 
    path_data_vo <- import_leerling_data(data_params$schooljaar, refresh) 
  
    ### Factoriseren data-----------------------------------------------------------
    data_start <-  factoriseer_eencijferdata(path_data = path_data_vo,
                                              cols_of_interest = c(data_params$cols_of_interest)) 
    
    tabel_onderwijsgebied <- import_brin_data()
      
    ### Data postprocessing-----------------------------------------------------------
    
    # je wilt garanderen dat er voor elke gemeente maar 1 mogelijk bevolkingsaantal is
    # dit is essentieel voor om dit detereministisch te kunnen synthetiseren
    # dus: creeeren van datatabel met 1 gtlln per gemeente gebasseerd op meest voorkomende gemnr-gtlln combi
    gtlln_1cijfer <- data_start %>%
      select(gemnr, gtlln) %>% group_by(gemnr, gtlln) %>%
      mutate(count = n()) %>% unique() %>% ungroup %>%
      group_by(gemnr) %>% filter(count == max(count)) %>%
      select(-count)
    
   # Na's expliciet maken, variable types aanpassen 
    data <- data_start %>%
      select(data_params$cols_of_interest, - gtlln) %>%
      left_join(gtlln_1cijfer, by = c('gemnr'))
    
    
    data <- data %>% 
      mutate(apli = as.numeric(apli),
             apnw = as.numeric(apnw),
             apuk = as.numeric(apuk),
             brin = as.factor(brin),
             gtlln = na_if(gtlln, 'NA'))
        
    # brinvest variabele aanmaken
    data <- data %>%
      mutate(vest = str_pad(vest, 2, "left", 0)) %>% 
      mutate(brinvest = as.factor(paste0(brin, vest)))
    
    # onderwijsgebied toevoegen
    data <- data %>%
      left_join(tabel_onderwijsgebied, by = c('brinvest' = 'BRINVEST'))
    
    ### Afronding  -----------------------------------------------------------

    # gewenste volgorde van variabele bepalen
    data <- data %>% 
      select(gemnr, brinvest, gtlln, brin, sexe, apli, apnw, apuk, Onderwijsgebied)
    
    #verwijder andere classes dan df, omdat de post_hoc dan moeilijk doet
    class(data) <- "data.frame"
    
    # data opslaan
    write_csv2(data, path_data_csv)
    saveRDS(data, file = paste0(path_data))
    
  }  

  
  data <- readRDS(file = path_data) 
  
  return(data)
}


#-----Importeer en laad packages------------------------------------------------

# Packages gebruikt in dit script
pkg <- c("tidyverse",
         "synthpop",
         "igraph", #gebruikt voor visualisatie
         "odbc" #gebruikt om DWH connectie te maken
)

# Controleer of ze geinstalleerd zijn en zo niet sla de naam van het package op 
# in new.pkg om ze later wel te installeren
new.pkg <- pkg[!(pkg %in% installed.packages())]

# Installeer de packages in new.pkg 
if (length(new.pkg)) {
  install.packages(new.pkg, repos = "http://cran.rstudio.com")}

# Laad alle packages en vang de output
invisible(capture.output(lapply(pkg, require, character.only = TRUE)))
rm(pkg, new.pkg, GCtorture) #GCtorture alleen bij gebruik synthpop

#-----Laden van functies -------------------------------------------------------

source("1_data_prep.R")
source("2_params.R")

#-----Data ophalen -------------------------------------------------------------

data_observed <- data_prep()

#-----Synthetiseren data -------------------------------------------------------

# Ophalen van parameters
syn_params <- get_syn_params(data_observed)

# Ophalen van benodigdheden voor passieve synthese methodes
gem_data <- get_gem_data(data_observed)
brin_data <- get_brin_data(data_observed)

# Synthetiseren van geobserveerde data 
sds <- syn.strata(data_observed,
                  strata = syn_params[['strata']],
                  method = syn_params[['method']],
                  predictor.matrix = syn_params[['predictor.matrix']],
                  models = TRUE,
                  seed = 27071987,
                  maxfaclevels = Inf,
                  catall.priorn=k/100000)

#-----Post hoc tests -----------------------------------------------------------

# Deze testen controleren utiliteit en privacy
source("helper_functions/test_posthoc.R")

# class moet gedefinieerd worden om post_hoc_utility te runnen

post_hoc_utility <- post_hoc_utility(sds, data_observed)

post_hoc_privacy <- post_hoc_privacy(data = data_observed,
                      strata = syn_params[['strata']],
                      method = syn_params[['method']],
                      predictor.matrix =  syn_params[['predictor.matrix']],
                      seed = 27071987,
                      maxfaclevels = Inf,
                      catall.priorn=0)


#-----Gesynthetiseerde data opslaan -------------------------------------------------------

syn_data_opslaan <- function(sds){
  # doel: opslaan van de synthetische data in RDS en csv
  
  write_csv2(sds[['syn']], paste0("Data/synthetisch/VO_cohort_", data_params$schooljaar,".csv"))
  saveRDS(sds, file = paste0("Data/synthetisch/VO_cohort_", data_params$schooljaar,".Rds"))
}

syn_data_opslaan(sds)



                        pickerInput(
                          inputId = 'filter_graad',
                          label = 'Graad',
                          multiple = TRUE,
                          choices = graad,
                          selected = geimplementeerde_filters$filter_graad,
                          options = pickerOptions(
                            noneSelectedText = 'Geen selectie'
                          )
                        ), 
                        pickerInput(
                          inputId = 'filter_opleidingsvorm',
                          label = 'Opleidingsvorm',
                          multiple = TRUE,
                          choices = c(
                            'alle opleidingsvormen' = 'totaal',
                            'deeltijd',
                            'duaal',
                            'voltijd'
                          ),
                          selected = geimplementeerde_filters$filter_opleidingsvorm,
                          options = pickerOptions(
                            noneSelectedText = 'Geen selectie')
                        ),
                        pickerInput(
                          inputId = 'filter_opleiding',
                          label = 'Opleiding',
                          multiple = TRUE,
                          choices = opleiding,
                          selected = geimplementeerde_filters$filter_opleiding,
                          options = pickerOptions(
                            noneSelectedText = 'Geen selectie',
                            liveSearch = TRUE,
                            liveSearchPlaceholder = "zoeken"
                          )
                        ),
                        pickerInput(
                          inputId = 'filter_vooropleiding',
                          label = 'Vooropleiding',
                          multiple = TRUE,
                          choices = vooropleiding,
                          selected = geimplementeerde_filters$filter_vooropleiding,
                          options = pickerOptions(
                            noneSelectedText = 'Geen selectie'
                          )
                        ),                        
                        pickerInput(
                          inputId = 'filter_geslacht',
                          label = 'Geslacht',
                          multiple = TRUE,
                          choices = c(
                            'alle geslachten' = 'totaal',
                            'man',
                            'vrouw'
                          ),
                          selected = geimplementeerde_filters$filter_geslacht,
                          options = pickerOptions(
                            noneSelectedText = 'Geen selectie'
                          )
                        ),
                        
                        pickerInput(
                          inputId = 'filter_zijinstroom',
                          label = 'Zij-instroom',
                          multiple = TRUE,
                          choices = zijinstroom,
                          selected = geimplementeerde_filters$filter_zijinstroom,
                          options = pickerOptions(
                            noneSelectedText = 'Geen selectie'
                          )
                        ),
                        
                        pickerInput(
                          inputId = 'filter_G5gemeentes',
                          label = 'G5 gemeentes',
                          multiple = TRUE,
                          choices = G5gemeentes,
                          selected = geimplementeerde_filters$filter_G5gemeentes,
                          options = pickerOptions(
                            noneSelectedText = 'Geen selectie'
                          )
                        ),
                        pickerInput(
                          inputId = 'filter_ethniciteit',
                          label = 'Etniciteit',
                          multiple = TRUE,
                          choices = ethniciteit,
                          selected = geimplementeerde_filters$filter_ethniciteit,
                          options = pickerOptions(
                            noneSelectedText = 'Geen selectie'
                          )
                        ),
                        
                        
                    ) # einde div 'sort_slaap'
                    
                    
 if(input$themePicker == "Lerarenopleidingen") {
      message("tab_selection Lerarenopleidingen")
      choices = to_vec(for(x in lerarenopleiding) {x$tab_naam})
    }
    
   
<h2>Instroom definities</h2>

Het aantal instromers is afhankelijk van de definitie die voor een instromer is gekozen. Afhankelijk van de gekozen filters verandert de door DUO aanbevolen
  definitie. Die aanbeveling is gebaseerd op veelvoorkomende vragen, maar alle filters zijn ‘correct’ afhankelijk van de behoefte van de gebruiker. Filters zijn
  niet beschikbaar wanneer dit niet relevant is voor het thema. 

#### Instroom in domein hoger onderwijs

Het aantal personen met een hoofdinschrijving in het HO domein dat voor het eerst ingeschreven staat in het bekostigd hoger onderwijs op 1 oktober. Dat betekent
  dat studenten niet meer als instromers worden meegeteld voor nieuwe opleidingen na een eerdere HO inschrijving, ongeacht soort HO (hbo, wo) of type HO (ad, ba, ma).
* Bijvoorbeeld: een student die een wo-bachelor heeft afgerond en in het huidige studiejaar  
een wo-master begint wordt **niet** meegeteld als instromer. 
* Bijvoorbeeld: een student die eerder een hbo-bachelor volgde en in het huidige studiejaar  
een wo-bachelor begint wordt **niet** meegeteld als instromer.


#### Instroom in soort HO

Het aantal personen met een hoofdinschrijving in het HO domein dat voor het eerst ingeschreven staat in een specifieke soort HO (hbo of wo) op 1 oktober, 
  onafhankelijk van type HO (ad, ba, ma). 
* Bijvoorbeeld: een student die een wo-bachelor heeft afgerond en in het huidige studiejaar een wo-master begint wordt **niet** meegeteld als instromer. 
* Bijvoorbeeld: een student die eerder een hbo-bachelor volgde en na de propedeuse een wo-bachelor begint wordt **wel** meegeteld als instromer.

#### Instroom in type HO

Het aantal personen met een hoofdinschrijving in het HO domein dat voor het eerst ingeschreven staat in type HO (ad, ba, ma) op 1 oktober, onafhankelijk van soort HO. 
* Bijvoorbeeld: een student die een wo-bachelor heeft afgerond en in het huidige studiejaar  
een wo-master begint wordt **wel** meegeteld als instromer. 
* Bijvoorbeeld: een student die eerder een hbo-bachelor volgde en in het huidige studiejaar  
een wo-bachelor begint wordt **niet** meegeteld als instromer.

#### Instroom in combinatie type en soort HO

Het aantal personen met een hoofdinschrijving in het HO domein dat voor het eerst ingeschreven staat in een vaste combinatie
  van een soort HO en een type HO op 1 oktober.
* Bijvoorbeeld: een student die een wo-bachelor heeft afgerond en in het huidige studiejaar  
een wo-master begint wordt **wel** meegeteld als instromer. 
* Bijvoorbeeld: een student die eerder een hbo-bachelor volgde en in het huidige studiejaar  
een wo-bachelor begint wordt **wel** meegeteld als instromer.  

Deze definitie wijkt af van de Monitor Beleidsmaatregelen, waar instromers die na oktober beginnen helemaal niet geteld worden.
#### _Instroom in combinatie opleiding en instelling_

Het aantal personen met een hoofdinschrijving in het HO domein dat voor het eerst ingeschreven staat in een opleiding in een specifieke opleiding, 
  op een specifieke instelling, op 1 oktober.

* Bijvoorbeeld: een student die de hbo-bachelor leraar wiskunde heeft gevolgd aan de Hogeschool van Amsterdam,  
en in het huidige studiejaar deze bachelor voortzet aan de Hogeschool Utrecht wordt **wel** meegeteld als instromer.   

#### _Instroom in opleiding, ongeacht instelling_

Het aantal personen met een hoofdinschrijving in het HO domein dat voor het eerst ingeschreven staat in een specifieke opleiding, 
  ongeacht de instelling, op 1 oktober.

* Bijvoorbeeld: een student die de hbo-bachelor leraar wiskunde heeft gevolgd aan de Hogeschool van Amsterdam,  
en in het huidige studiejaar deze bachelor voortzet aan de Hogeschool Utrecht wordt **niet** meegeteld als instromer.   
* Bijvoorbeeld: een student die de hbo-bachelor leraar wiskunde heeft gevolgd aan de Hogeschool van Amsterdam,  
en in het huidige studiejaar overstapt naar een bachelor leraar natuurkunde wordt **wel** meegeteld als instromer.   

#------------------------------------------------------------------------------
# Auteur:  Romy Schipper
# Datum:   02-05-2022
# Project: Geisoleerde scholen - dataprep  schooljaar 2021/2022 
# Script:  Dataprep van dashboard geisoleerde scholen  voor schooljaar 2021/2022
#
# Toelichting: Gebasseerd op dataprep script van Kimberley 
#
# Input:   "Geisoleerde vestigingen 19-04-22 13.36.59.csv" 
#          [Aangeleverd door Maria Ammerlaan aan IP-frontoffice op 19-04-2022] 
#          "org_D_20220214.sav" [spss file met namen per brin, bron: I:\Team Personeel\Data\Populatie\brinbestanden\system]]
# 
#
#------------------------------------------------------------------------------


# Input data inladen---------------------------------------------------------------

bekostigingsdata <- function(path_data, 
                             data_bekostiging, 
                             data_orgd, 
                             status_data = c('definitief', 'voorlopig', 'test'), 
                             bekostigingsjaar,
                             output_path){
  setwd(path_data) #("I:/Team VO/Projecten/Geisoleerde Scholen/Data")
  
  data_bekostiging <- read.csv(data_bekostiging, sep = ';') #read.csv("data_input/Geisoleerde vestigingen 19-04-22 13.36.59.csv", sep = ';') 
  data_orgd <- read.spss(data_orgd, to.data.frame = TRUE, use.value.labels = FALSE) 
  #read.spss("data_input/org_D_20220214.sav", to.data.frame = TRUE, use.value.labels = FALSE) 
  
  
  # Definieer variabelen ---------------------------------------------------------------
  vaste_voet = 227160.97
  xx_procent_van_het_bedrag_per_leerling = 0.40
  bedrag_per_leerling = 7766.86
  
  afstand_pro = 20000
  afstand_geen_pro = 8000
  afstand_cluster = 5000
  
  speciale_brinvests_uitgesloten_van_bekostiging = c("16PN00", "19EO00", "19ET00", "19HY00", "23GF00")
  
  
  # Data cleaning ---------------------------------------------------------------
  
  data_orgd$OWNAAMK <- str_trim(data_orgd$OWNAAMK)
  

  # data samenvoegen
  data <- merge(data_bekostiging, data_orgd[,c("BRINVEST", "OWNAAMK")], by.x = "VESTIGINGSCODE", by.y = "BRINVEST", all.x = T, all.y = F)
  rm(data_orgd, data_bekostiging)
  
  # alleen voorlopige gegevens, en vestigingen met leerlingen
  data <- data %>% 
    #filter(STATUS_GEGEVENS == "V") %>%
    filter(AANTAL_LEERLINGEN > 0)
  
  data <- data %>% 
    dplyr::rename("BEVOEGD_GEZAG" = "BEVOEGDGEZAGCODE",
                  "BRIN" = "INSTELLINGSCODE", 
                  "brinvest" = "VESTIGINGSCODE",
                  "RD_x" = "X_COORDINAAT_LOCATIE",
                  "RD_y" = "Y_COORDINAAT_LOCATIE")

  
  # values van variabelen aanpassen
  numeric <- function(x, na.rm = FALSE) as.numeric(str_replace_all(x, ",", "."))
  
  # functie toepassen op relevante variabele 
  data <- data %>% mutate_at(c("RD_x", "RD_y", "AANTAL_MAVO", 
                               "AANTAL_HAVO", "AANTAL_VWO", "AANTAL_VBO", "AANTAL_PRO", 
                               "AANTAL_LEERJAAR_1", "AANTAL_LEERJAAR_2", "AANTAL_LEERLINGEN"),
                             numeric)

  
  # Check ---------------------------------------------------------------
  
  if(length(unique(data$brinvest)) < length(data$brinvest)){
    stop("Er zijn dubbele entries van brinvests")
  }
  
  # Variabelen toevoegen--------------------------------------------
  
  data$INDICATIE_SPECIALE_VESTIGING <- ifelse(data$brinvest %in% speciale_brinvests_uitgesloten_van_bekostiging, "J", "N")
  data$VEST <- str_sub(data$brinvest, 5, 6) 
  
  data$VESTIGING_TE_KLEIN = ifelse(data$INDICATIE_UITSLUITEND_PRO == "J" & data$AANTAL_LEERLINGEN < 60, "J", 
                                   ifelse(data$INDICATIE_UITSLUITEND_PRO == "N" & data$AANTAL_LEERLINGEN < 130, "J", "N"))
  
  data$AANTAL_ONDERBOUW = ifelse(data$AANTAL_LEERJAAR_1 >= 1 & data$AANTAL_LEERJAAR_2 >= 1, 
                                 data$AANTAL_LEERJAAR_1 + data$AANTAL_LEERJAAR_2,
                                 0)
  
  # kolom met welke onderwijssoorten een vestiging geisoleerd voor ligt (en welk leerjaar)
  data$AANGEBODEN_ONDERWIJSSOORTEN = rep("", times = nrow(data))
  data$AANGEBODEN_ONDERWIJSSOORTEN_PLUS_LEERJAAR = rep("", times = nrow(data))
  for(i in 1:nrow(data)){
    onderwijssoorten = c()
    onderwijssoorten_plus_leerjaar = c()
    if(data$AANTAL_ONDERBOUW[i] >= 1){
      onderwijssoorten = append(onderwijssoorten, "ONDERBOUW")
      onderwijssoorten_plus_leerjaar = append(onderwijssoorten_plus_leerjaar, "ONDERBOUW")
    }
    if(data$AANTAL_PRO[i] >= 1){
      onderwijssoorten = append(onderwijssoorten, "PRO")
      onderwijssoorten_plus_leerjaar = append(onderwijssoorten_plus_leerjaar, "PRO")
    }
    if(data$AANTAL_VBO[i] >= 1){
      onderwijssoorten = append(onderwijssoorten, "VBO")
      onderwijssoorten_plus_leerjaar = append(onderwijssoorten_plus_leerjaar, "VBO leerjaar 4")
    }
    if(data$AANTAL_MAVO[i] >= 1){
      onderwijssoorten = append(onderwijssoorten, "MAVO")
      onderwijssoorten_plus_leerjaar = append(onderwijssoorten_plus_leerjaar, "VMBO TL leerjaar 4")
    }
    if(data$AANTAL_HAVO[i] >= 1){
      onderwijssoorten = append(onderwijssoorten, "HAVO")
      onderwijssoorten_plus_leerjaar = append(onderwijssoorten_plus_leerjaar, "HAVO leerjaar 5")
    }
    if(data$AANTAL_VWO[i] >= 1){
      onderwijssoorten = append(onderwijssoorten, "VWO")
      onderwijssoorten_plus_leerjaar = append(onderwijssoorten_plus_leerjaar, "VWO leerjaar 6")
    }
    
    data$AANGEBODEN_ONDERWIJSSOORTEN[i] = paste0(onderwijssoorten, collapse = ", ")
    data$AANGEBODEN_ONDERWIJSSOORTEN_PLUS_LEERJAAR[i] = paste0(onderwijssoorten_plus_leerjaar, collapse = ", ")
  }
  
  
  # kolom met toelichting waarom een vestiging niet in aanmerking komt voor bekostiging
  data$waarschuwing_niet_in_aanmerking = rep("", times = nrow(data))
  for(i in 1:nrow(data)){
    x = ""
    if(data$VESTIGING_TE_KLEIN[i] == "J"){
      x = paste0(x, "heeft een te laag leerlingaantal")
    }
    if(data$VESTIGINGSOORT[i] == "TVST" & data$VESTIGING_TE_KLEIN[i] == "J"){
      x = paste0(x, " en ")
    }
    if(data$VESTIGINGSOORT[i] == "TVST"){
      x = paste0(x, "is tijdelijk")
    }
    if(data$VESTIGING_TE_KLEIN[i] == "J" | data$VESTIGINGSOORT[i] == "TVST"){
      data$waarschuwing_niet_in_aanmerking[i] = paste0("LET OP: Deze vestiging ", x, " en komt daardoor niet in aanmerking voor de toeslag. Het aanta
l leerlingen wordt wel gebruikt voor het vaststellen van het aantal leerlingen die op een scholengemeenschap zijn ingeschreven (artikel 5 en 6).") 
    }
    #de speciale vestigingen krijgen een apart bericht omdat ze uitgesloten zijn van de hele regeling 
    #(of ze daarbij tijdelijk of te klein zijn maakt dus niet uit en hoeft er niet bij in de zin)
    if(data$INDICATIE_SPECIALE_VESTIGING[i] == "J"){
      data$waarschuwing_niet_in_aanmerking[i] = "LET OP: Deze vestiging krijgt aanvullende bekostiging op grond van artikel 2 van de regeling aanvullende
bekostiging vo-scholen in uitzonderlijke omstandigheden en komt daardoor niet in aanmerking voor deze regeling"
    }
  }
  
  # longitude en lattitude coordinaten toe aan de data
  data$lng <- 5.387206 + (((5260.52916 * ((data$RD_x-155000)/10^5)) + (105.94684 * ((data$RD_x-155000)/10^5) * ((data$RD_y-463000)/10^5)) + 
                           (2.45656 * ((data$RD_x-155000)/10^5) * (((data$RD_y-463000)/10^5)) ^ 2) + (-0.81885 * (((data$RD_x-155000)/10^5)) ^ 3) + 
                           (0.05594 * ((data$RD_x-155000)/10^5) * (((data$RD_y-463000)/10^5)) ^ 3) + (-0.05607 * (((data$RD_x-155000)/10^5)) ^ 3 * ((data$RD_y-463000)/10^5)) + 
                           (0.01199 * ((data$RD_y-463000)/10^5)) + (-0.00256 * (((data$RD_x-155000)/10^5)) ^ 3 * (((data$RD_y-463000)/10^5)) ^ 2) + 
                           (0.00128 * ((data$RD_x-155000)/10^5) * (((data$RD_y-463000)/10^5)) ^ 4) + (0.00022 * (((data$RD_y-463000)/10^5)) ^ 2) + 
                           (-0.00022 * (((data$RD_x-155000)/10^5)) ^ 2) + (0.00026 * (((data$RD_x-155000)/10^5)) ^ 5)) / 3600)
  data$lat <- 52.15517 + (((3235.65389 * ((data$RD_y-463000)/10^5)) + (-32.58297 * (((data$RD_x-155000)/10^5)) ^ 2) + (-0.2475 * (((data$RD_y-463000)/10^5)) ^ 2) + 
                           (-0.84978 * (((data$RD_x-155000)/10^5)) ^ 2 * ((data$RD_y-463000)/10^5)) + (-0.0655 * (((data$RD_y-463000)/10^5)) ^ 3) + 
                           (-0.01709 * (((data$RD_x-155000)/10^5)) ^ 2 * (((data$RD_y-463000)/10^5)) ^ 2) + (-0.00738 * ((data$RD_x-155000)/10^5)) + 
                           (0.0053 * (((data$RD_x-155000)/10^5)) ^ 4) + (-0.00039 * (((data$RD_x-155000)/10^5)) ^ 2 * (((data$RD_y-463000)/10^5)) ^ 3) +
                           (0.00033 * (((data$RD_x-155000)/10^5)) ^ 4 * ((data$RD_y-463000)/10^5)) + 
                           (-0.00012 * ((data$RD_x-155000)/10^5) * ((data$RD_y-463000)/10^5))) / 3600)
  
  
  # Afstanden berekenen --------------------------------------------
  
  # functie: euclidian afstand berekenen 
  euclidean_distance_calculation = function(row){
    x_coord = row[1]
    y_coord = row[2]
    sqrt((x_coord - data$RD_x)^2 + (y_coord - data$RD_y)^2)
  } 
  
  afstandsmatrix <- apply(data[,c("RD_x", "RD_y")], 1, euclidean_distance_calculation)
  afstandsmatrix <- as.data.frame(afstandsmatrix)
  row.names(afstandsmatrix) <- data$brinvest
  colnames(afstandsmatrix) <- data$brinvest
  
  
  # Bepaling eisoleerd voor welke onderwijssoort ---------------------------------
  
  data$GEISOLEERDE_ONDERWIJSSOORTEN <- rep("", times = nrow(data))
  
  for(owsoort_i in c("ONDERBOUW", "PRO", "VBO", "MAVO", "HAVO", "VWO")){
    data[,paste0("GEISOLEERD_VOOR_", owsoort_i)] = rep("N", times = nrow(data))
    vests = data[data[,paste0("AANTAL_", owsoort_i)] >= 1, ]
    vests = filter(vests, VESTIGING_TE_KLEIN == "N",
                   VESTIGINGSOORT != "TVST",
                   INDICATIE_SPECIALE_VESTIGING == "N")$brinvest 
    
    onderwijssoort_afstandsmatrix = afstandsmatrix[row.names(afstandsmatrix) %in% vests, colnames(afstandsmatrix) %in% vests]
    
    for(vest_i in vests){
      afstandsmatrix_i = onderwijssoort_afstandsmatrix[which(row.names(onderwijssoort_afstandsmatrix) == vest_i),]
      afstandsmatrix_i[,vest_i] = NULL
      kleinste_afstand = min(afstandsmatrix_i)
      if((owsoort_i != "PRO" & kleinste_afstand >= afstand_geen_pro) |
         (owsoort_i == "PRO" & kleinste_afstand >= afstand_pro)){
        data[which(data$brinvest == vest_i), paste0("GEISOLEERD_VOOR_", owsoort_i)] = "J"
        data[which(data$brinvest == vest_i), "GEISOLEERDE_ONDERWIJSSOORTEN"] = paste0(data[which(data$brinvest == vest_i), "GEISOLEERDE_ONDERWIJSSOORTEN"], " ", owsoort_i)
      }
    }
  }
  
  data$GEISOLEERDE_ONDERWIJSSOORTEN <- str_trim(data$GEISOLEERDE_ONDERWIJSSOORTEN, side = "left") #haal de allereerste spatie weg
  data$GEISOLEERDE_ONDERWIJSSOORTEN <- str_replace_all(data$GEISOLEERDE_ONDERWIJSSOORTEN, " ", ", ")
  
  
  # Data cleaning ---------------------------------
  datum_aggregatie <- data %>% select(DATUM_AGGREGATIE) %>% unique() %>% head(1)
  
  data <- data %>% select(-c(DATUM_AGGREGATIE, CODE_RELATIE, CODE_ROL, DATUM_REGISTRATIE, DATUM_LAATSTE_WIJZIGING, STATUS_GEGEVENS,
                             AANTAL_LEERJAAR_1, AANTAL_LEERJAAR_2))
  
  teldatum <- data %>% select(TELDATUM) %>% unique()
  
  # if(definitief == TRUE){
  #   def <- 'D'
  # } else {
  #   def <- 'V'
  # }
  
  if(status_data == 'definitief'){
    status <- 'D'
  } else if(status_data == 'voorlopig') {
    status <- 'V'
  } else if(status_data == 'test') {
    status <- 'T'
  }
  
  data_input <- data
  afstandsmatrix_input <- afstandsmatrix
  
  save(data_input, afstandsmatrix_input, afstand_cluster, afstand_geen_pro, afstand_pro, 
       bedrag_per_leerling, vaste_voet, xx_procent_van_het_bedrag_per_leerling, 
       file = paste0(output_path,"bekostigings_data_",bekostigingsjaar,"_",status,".RData"))
  
}


#------------------------------------------------------------------------------
# Auteur:  Romy Schipper
# Datum:   02-05-2022
# Project: Geisoleerde scholen - dataprep  schooljaar 2021/2022 
# Script:  Dataprep van dashboard geisoleerde scholen  voor schooljaar 2021/2022
#
# Toelichting: Gebasseerd op dataprep script van Kimberley 
#
# Input:   "Geisoleerde vestigingen 19-04-22 13.36.59.csv" 
#          [Aangeleverd door Maria Ammerlaan aan IP-frontoffice op 19-04-2022] 
#          "org_D_20220214.sav" [spss file met namen per brin, bron: I:\Team Personeel\Data\Populatie\brinbestanden\system]]
# 
#
#------------------------------------------------------------------------------


# Input data inladen---------------------------------------------------------------

bekostigingsdata <- function(path_data, 
                             data_bekostiging, 
                             data_orgd, 
                             status_data = c('definitief', 'voorlopig', 'test'), 
                             bekostigingsjaar,
                             output_path){
  setwd(path_data) #("I:/Team VO/Projecten/Geisoleerde Scholen/Data")
  
  data_bekostiging <- read.csv(data_bekostiging, sep = ';') #read.csv("data_input/Geisoleerde vestigingen 19-04-22 13.36.59.csv", sep = ';') 
  data_orgd <- read.spss(data_orgd, to.data.frame = TRUE, use.value.labels = FALSE)  
  #read.spss("data_input/org_D_20220214.sav", to.data.frame = TRUE, use.value.labels = FALSE) 
  
  
  # Definieer variabelen ---------------------------------------------------------------
  vaste_voet = 227160.97
  xx_procent_van_het_bedrag_per_leerling = 0.40
  bedrag_per_leerling = 7766.86
  
  afstand_pro = 20000
  afstand_geen_pro = 8000
  afstand_cluster = 5000
  
  speciale_brinvests_uitgesloten_van_bekostiging = c("16PN00", "19EO00", "19ET00", "19HY00", "23GF00")
  
  
  # Data cleaning ---------------------------------------------------------------
  
  data_orgd$OWNAAMK <- str_trim(data_orgd$OWNAAMK)
  

  # data samenvoegen
  data <- merge(data_bekostiging, data_orgd[,c("BRINVEST", "OWNAAMK")], by.x = "VESTIGINGSCODE", by.y = "BRINVEST", all.x = T, all.y = F)
  rm(data_orgd, data_bekostiging)
  
  # alleen voorlopige gegevens, en vestigingen met leerlingen
  data <- data %>% 
    #filter(STATUS_GEGEVENS == "V") %>%
    filter(AANTAL_LEERLINGEN > 0)
  
  data <- data %>% 
    dplyr::rename("BEVOEGD_GEZAG" = "BEVOEGDGEZAGCODE",
                  "BRIN" = "INSTELLINGSCODE", 
                  "brinvest" = "VESTIGINGSCODE",
                  "RD_x" = "X_COORDINAAT_LOCATIE",
                  "RD_y" = "Y_COORDINAAT_LOCATIE")

  
  # values van variabelen aanpassen
  numeric <- function(x, na.rm = FALSE) as.numeric(str_replace_all(x, ",", "."))
  
  # functie toepassen op relevante variabele 
  data <- data %>% mutate_at(c("RD_x", "RD_y", "AANTAL_MAVO", 
                               "AANTAL_HAVO", "AANTAL_VWO", "AANTAL_VBO", "AANTAL_PRO", 
                               "AANTAL_LEERJAAR_1", "AANTAL_LEERJAAR_2", "AANTAL_LEERLINGEN"),
                             numeric)

  
  # Check ---------------------------------------------------------------
  
  if(length(unique(data$brinvest)) < length(data$brinvest)){
    stop("Er zijn dubbele entries van brinvests")
  }
  
  # Variabelen toevoegen--------------------------------------------
  
  data$INDICATIE_SPECIALE_VESTIGING <- ifelse(data$brinvest %in% speciale_brinvests_uitgesloten_van_bekostiging, "J", "N")
  data$VEST <- str_sub(data$brinvest, 5, 6) 
  
  data$VESTIGING_TE_KLEIN = ifelse(data$INDICATIE_UITSLUITEND_PRO == "J" & data$AANTAL_LEERLINGEN < 60, "J", 
                                   ifelse(data$INDICATIE_UITSLUITEND_PRO == "N" & data$AANTAL_LEERLINGEN < 130, "J", "N"))
  
  data$AANTAL_ONDERBOUW = ifelse(data$AANTAL_LEERJAAR_1 >= 1 & data$AANTAL_LEERJAAR_2 >= 1, 
                                 data$AANTAL_LEERJAAR_1 + data$AANTAL_LEERJAAR_2,
                                 0)
  
  # kolom met welke onderwijssoorten een vestiging geisoleerd voor ligt (en welk leerjaar)
  data$AANGEBODEN_ONDERWIJSSOORTEN = rep("", times = nrow(data))
  data$AANGEBODEN_ONDERWIJSSOORTEN_PLUS_LEERJAAR = rep("", times = nrow(data))
  for(i in 1:nrow(data)){
    onderwijssoorten = c()
    onderwijssoorten_plus_leerjaar = c()
    if(data$AANTAL_ONDERBOUW[i] >= 1){
      onderwijssoorten = append(onderwijssoorten, "ONDERBOUW")
      onderwijssoorten_plus_leerjaar = append(onderwijssoorten_plus_leerjaar, "ONDERBOUW")
    }
    if(data$AANTAL_PRO[i] >= 1){
      onderwijssoorten = append(onderwijssoorten, "PRO")
      onderwijssoorten_plus_leerjaar = append(onderwijssoorten_plus_leerjaar, "PRO")
    }
    if(data$AANTAL_VBO[i] >= 1){
      onderwijssoorten = append(onderwijssoorten, "VBO")
      onderwijssoorten_plus_leerjaar = append(onderwijssoorten_plus_leerjaar, "VBO leerjaar 4")
    }
    if(data$AANTAL_MAVO[i] >= 1){
      onderwijssoorten = append(onderwijssoorten, "MAVO")
      onderwijssoorten_plus_leerjaar = append(onderwijssoorten_plus_leerjaar, "VMBO TL leerjaar 4")
    }
    if(data$AANTAL_HAVO[i] >= 1){
      onderwijssoorten = append(onderwijssoorten, "HAVO")
      onderwijssoorten_plus_leerjaar = append(onderwijssoorten_plus_leerjaar, "HAVO leerjaar 5")
    }
    if(data$AANTAL_VWO[i] >= 1){
      onderwijssoorten = append(onderwijssoorten, "VWO")
      onderwijssoorten_plus_leerjaar = append(onderwijssoorten_plus_leerjaar, "VWO leerjaar 6")
    }
    
    data$AANGEBODEN_ONDERWIJSSOORTEN[i] = paste0(onderwijssoorten, collapse = ", ")
    data$AANGEBODEN_ONDERWIJSSOORTEN_PLUS_LEERJAAR[i] = paste0(onderwijssoorten_plus_leerjaar, collapse = ", ")
  }
  
  
  # kolom met toelichting waarom een vestiging niet in aanmerking komt voor bekostiging
  data$waarschuwing_niet_in_aanmerking = rep("", times = nrow(data))
  for(i in 1:nrow(data)){
    x = ""
    if(data$VESTIGING_TE_KLEIN[i] == "J"){
      x = paste0(x, "heeft een te laag leerlingaantal")
    }
    if(data$VESTIGINGSOORT[i] == "TVST" & data$VESTIGING_TE_KLEIN[i] == "J"){
      x = paste0(x, " en ")
    }
    if(data$VESTIGINGSOORT[i] == "TVST"){
      x = paste0(x, "is tijdelijk")
    }
    if(data$VESTIGING_TE_KLEIN[i] == "J" | data$VESTIGINGSOORT[i] == "TVST"){
      data$waarschuwing_niet_in_aanmerking[i] = paste0("LET OP: Deze vestiging ", x, " en komt daardoor niet in aanmerking voor de toeslag. Het aantal
leerlingen wordt wel gebruikt voor het vaststellen van het aantal leerlingen die op een scholengemeenschap zijn ingeschreven (artikel 5 en 6).") 
    }
    #de speciale vestigingen krijgen een apart bericht omdat ze uitgesloten zijn van de hele regeling (of ze daarbij tijdelijk of te klein zijn maakt dus nie
    t uit en hoeft er niet bij in de zin)
    if(data$INDICATIE_SPECIALE_VESTIGING[i] == "J"){
      data$waarschuwing_niet_in_aanmerking[i] = "LET OP: Deze vestiging krijgt aanvullende bekostiging op grond van artikel 2 van de regeling aanvullende 
bekostiging vo-scholen in uitzonderlijke omstandigheden en komt daardoor niet in aanmerking voor deze regeling"
    }
  }
  
  # longitude en lattitude coordinaten toe aan de data
  data$lng <- 5.387206 + (((5260.52916 * ((data$RD_x-155000)/10^5)) + (105.94684 * ((data$RD_x-155000)/10^5) * ((data$RD_y-463000)/10^5)) + 
                           (2.45656 * ((data$RD_x-155000)/10^5) * (((data$RD_y-463000)/10^5)) ^ 2) 
  data$lat <- 52.15517 + (((3235.65389 * ((data$RD_y-463000)/10^5)) + (-32.58297 * (((data$RD_x-155000)/10^5)) ^ 2) + (-0.2475 * (((data$RD_y-463000)/10^5)) ^ 2) + (
  
  # Afstanden berekenen --------------------------------------------
  
  # functie: euclidian afstand berekenen 
  euclidean_distance_calculation = function(row){
    x_coord = row[1]
    y_coord = row[2]
    sqrt((x_coord - data$RD_x)^2 + (y_coord - data$RD_y)^2)
  } 
  
  afstandsmatrix <- apply(data[,c("RD_x", "RD_y")], 1, euclidean_distance_calculation)
  afstandsmatrix <- as.data.frame(afstandsmatrix)
  row.names(afstandsmatrix) <- data$brinvest
  colnames(afstandsmatrix) <- data$brinvest
  
  
  # Bepaling eisoleerd voor welke onderwijssoort ---------------------------------
  
  data$GEISOLEERDE_ONDERWIJSSOORTEN <- rep("", times = nrow(data))
  
  for(owsoort_i in c("ONDERBOUW", "PRO", "VBO", "MAVO", "HAVO", "VWO")){
    data[,paste0("GEISOLEERD_VOOR_", owsoort_i)] = rep("N", times = nrow(data))
    vests = data[data[,paste0("AANTAL_", owsoort_i)] >= 1, ]
    vests = filter(vests, VESTIGING_TE_KLEIN == "N",
                   VESTIGINGSOORT != "TVST",
                   INDICATIE_SPECIALE_VESTIGING == "N")$brinvest 
    
    onderwijssoort_afstandsmatrix = afstandsmatrix[row.names(afstandsmatrix) %in% vests, colnames(afstandsmatrix) %in% vests]
    
    for(vest_i in vests){
      afstandsmatrix_i = onderwijssoort_afstandsmatrix[which(row.names(onderwijssoort_afstandsmatrix) == vest_i),]
      afstandsmatrix_i[,vest_i] = NULL
      kleinste_afstand = min(afstandsmatrix_i)
      if((owsoort_i != "PRO" & kleinste_afstand >= afstand_geen_pro) |
         (owsoort_i == "PRO" & kleinste_afstand >= afstand_pro)){
        data[which(data$brinvest == vest_i), paste0("GEISOLEERD_VOOR_", owsoort_i)] = "J"
        data[which(data$brinvest == vest_i), "GEISOLEERDE_ONDERWIJSSOORTEN"] = paste0(data[which(data$brinvest == vest_i), "GEISOLEERDE_ONDERWIJSSOORTEN"], " ", owsoort_i)
      }
    }
  }
  
  data$GEISOLEERDE_ONDERWIJSSOORTEN <- str_trim(data$GEISOLEERDE_ONDERWIJSSOORTEN, side = "left") #haal de allereerste spatie weg
  data$GEISOLEERDE_ONDERWIJSSOORTEN <- str_replace_all(data$GEISOLEERDE_ONDERWIJSSOORTEN, " ", ", ")
  
  
  # Data cleaning ---------------------------------
  datum_aggregatie <- data %>% select(DATUM_AGGREGATIE) %>% unique() %>% head(1)
  
  data <- data %>% select(-c(DATUM_AGGREGATIE, CODE_RELATIE, CODE_ROL, DATUM_REGISTRATIE, DATUM_LAATSTE_WIJZIGING, STATUS_GEGEVENS,
                             AANTAL_LEERJAAR_1, AANTAL_LEERJAAR_2))
  
  teldatum <- data %>% select(TELDATUM) %>% unique()
  
  # if(definitief == TRUE){
  #   def <- 'D'
  # } else {
  #   def <- 'V'
  # }
  
  if(status_data == 'definitief'){
    status <- 'D'
  } else if(status_data == 'voorlopig') {
    status <- 'V'
  } else if(status_data == 'test') {
    status <- 'T'
  }
  
  data_input <- data
  afstandsmatrix_input <- afstandsmatrix
  
  save(data_input, afstandsmatrix_input, afstand_cluster, afstand_geen_pro, afstand_pro, 
       bedrag_per_leerling, vaste_voet, xx_procent_van_het_bedrag_per_leerling, 
       file = paste0(output_path,"bekostigings_data_",bekostigingsjaar,"_",status,".RData"))
  
}
