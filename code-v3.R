
# ----Definieer functies------------------------------------------------

#Hai lotte, ik probeer iets te pushen 
factoriseer_eencijferdata <- function(path_data,
                                     path_domeinwaarden="Data/P_DOMEINWAARDEN.csv",
                                     cols_of_interest="ALL"){



  

    
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
 
                    
 if(input$themePicker == "Lerarenopleidingen") {
      message("tab_selection Lerarenopleidingen")
      choices = to_vec(for(x in lerarenopleiding) {x$tab_naam})
    }
    
   
<h2>Instroom definities</h2>

Het aantal instromers is afhankelijk van de definitie die voor een instromer is gekozen. Afhankelijk van de gekozen filters verandert de door DUO aanbevolen
  definitie. Die aanbeveling is gebaseerd op veelvoorkomende vragen, maar alle filters zijn ‘correct’ afhankelijk van de behoefte van de gebruiker. Filters zijn
  niet beschikbaar wanneer dit niet relevant is voor het thema. 

#### Instroom in combinatie type en soort HO

Het aantal personen met een hoofdinschrijving in het HO domein dat voor het eerst ingeschreven staat in een vaste combinatie
  van een soort HO en een type HO op 1 oktober.
* Bijvoorbeeld: een student die een wo-bachelor heeft afgerond en in het huidige studiejaar  
een wo-master begint wordt **wel** meegeteld als instromer. 
* Bijvoorbeeld: een student die eerder een hbo-bachelor volgde en in het huidige studiejaar  
een wo-bachelor begint wordt **wel** meegeteld als instromer.  

Deze definitie wijkt af van de Monitor Beleidsmaatregelen, waar instromers die na oktober beginnen helemaal niet geteld worden.
