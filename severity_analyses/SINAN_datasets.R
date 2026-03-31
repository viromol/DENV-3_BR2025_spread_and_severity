#packages
library(microdatasus)
library(dplyr)

#SIMPLIFIED DATASET (10 YEARS)

#data download
##2016
dengueBR_2016 <- fetch_datasus(year_start = 2016, year_end = 2016, information_system = "SINAN-DENGUE", timeout = 5000,
                               vars = c("SOROTIPO", "SG_UF_NOT","CLASSI_FIN","DT_NOTIFIC", "ID_MUNICIP"))
##2017
dengueBR_2017 <- fetch_datasus(year_start = 2017, year_end = 2017, information_system = "SINAN-DENGUE", timeout = 5000,
                               vars = c("SOROTIPO", "SG_UF_NOT","CLASSI_FIN","DT_NOTIFIC", "ID_MUNICIP"))
##2018
dengueBR_2018 <- fetch_datasus(year_start = 2018, year_end = 2018, information_system = "SINAN-DENGUE", timeout = 5000,
                               vars = c("SOROTIPO", "SG_UF_NOT","CLASSI_FIN","DT_NOTIFIC", "ID_MUNICIP"))
##2019
dengueBR_2019 <- fetch_datasus(year_start = 2019, year_end = 2019, information_system = "SINAN-DENGUE", timeout = 5000,
                               vars = c("SOROTIPO", "SG_UF_NOT","CLASSI_FIN","DT_NOTIFIC", "ID_MUNICIP"))
##2020
dengueBR_2020 <- fetch_datasus(year_start = 2020, year_end = 2020, information_system = "SINAN-DENGUE", timeout = 5000,
                               vars = c("SOROTIPO", "SG_UF_NOT","CLASSI_FIN","DT_NOTIFIC", "ID_MUNICIP"))
##2021
dengueBR_2021 <- fetch_datasus(year_start = 2021, year_end = 2021, information_system = "SINAN-DENGUE", timeout = 5000,
                               vars = c("SOROTIPO", "SG_UF_NOT","CLASSI_FIN","DT_NOTIFIC", "ID_MUNICIP"))
##2022
dengueBR_2022 <- fetch_datasus(year_start = 2022, year_end = 2022, information_system = "SINAN-DENGUE", timeout = 5000,
                               vars = c("SOROTIPO", "SG_UF_NOT","CLASSI_FIN","DT_NOTIFIC", "ID_MUNICIP"))
##2023
dengueBR_2023 <- fetch_datasus(year_start = 2023, year_end = 2023, information_system = "SINAN-DENGUE", timeout = 5000,
                               vars = c("SOROTIPO", "SG_UF_NOT","CLASSI_FIN","DT_NOTIFIC", "ID_MUNICIP"))
##2024
dengueBR_2024 <- fetch_datasus(year_start = 2024, year_end = 2024, information_system = "SINAN-DENGUE", timeout = 5000,
                               vars = c("SOROTIPO", "SG_UF_NOT","CLASSI_FIN","DT_NOTIFIC", "ID_MUNICIP"))
##2025
dengueBR_2025 <- fetch_datasus(year_start = 2025, year_end = 2025, information_system = "SINAN-DENGUE", timeout = 5000,
                               vars = c("SOROTIPO", "SG_UF_NOT","CLASSI_FIN","DT_NOTIFIC", "ID_MUNICIP"))

#data organization
allyears = bind_rows(BR_2007, BR_2009, BR_2010, BR_2011, BR_2012, BR_2013, BR_2014, BR_2015, BR_2016,
                     BR_2017, BR_2018, BR_2019, BR_2020, BR_2021, BR_2022, BR_2023, BR_2024, BR_2025)
rm(BR_2007, BR_2009, BR_2010, BR_2011, BR_2012, BR_2013, BR_2014, BR_2015, BR_2016,
   BR_2017, BR_2018, BR_2019, BR_2020, BR_2021, BR_2022, BR_2023, BR_2024, BR_2025)

#data processing (according to https://github.com/rfsaldanha/microdatasus/blob/master/R/process_sinan_dengue.R data dictionary)
allyears_p <- allyears %>%
  mutate(CLASSI_FIN = recode(CLASSI_FIN,
                             `1` = "Dengue clássico",
                             `2` = "Dengue com complicações",
                             `3` = "Febre hemorrágica do dengue",
                             `4` = "Síndrome do choque do dengue",
                             `5` = "Descartado",
                             `6` = "Ignorado",
                             `8` = "Inconclusivo",
                             `10` = "Dengue",
                             `11` = "Dengue com sinais de alarme",
                             `12` = "Dengue grave"))

allyears_p <- allyears_p %>%
  mutate(SG_UF_NOT = recode(SG_UF_NOT,
                            '0' = 'Ignorado',
                            '99' = 'Ignorado',
                            '11' = 'Rondônia',
                            '12' = 'Acre',
                            '13' = 'Amazonas',
                            '14' = 'Roraima',
                            '15' = 'Pará',
                            '16' = 'Amapá',
                            '17' = 'Tocantins',
                            '21' = 'Maranhão',
                            '22' = 'Piauí',
                            '23' = 'Ceará',
                            '24' = 'Rio Grande do Norte',
                            '25' = 'Paraíba',
                            '26' = 'Pernambuco',
                            '27' = 'Alagoas',
                            '28' = 'Sergipe',
                            '29' = 'Bahia',
                            '31' = 'Minas Gerais',
                            '32' = 'Espírito Santo',
                            '33' = 'Rio de Janeiro',
                            '35' = 'São Paulo',
                            '41' = 'Paraná',
                            '42' = 'Santa Catarina',
                            '43' = 'Rio Grande do Sul',
                            '50' = 'Mato Grosso do Sul',
                            '51' = 'Mato Grosso',
                            '52' = 'Goiás',
                            '53' = 'Distrito Federal'))

#removal of inconclusive, ignored and discarded data 
allyears_p_conf <- subset(allyears_p, allyears_p$CLASSI_FIN!="Descartado")
allyears_p_conf <- subset(allyears_p_conf, allyears_p_conf$CLASSI_FIN!="Inconclusivo")
allyears_p_conf <- subset(allyears_p_conf, allyears_p_conf$CLASSI_FIN!="Ignorado")
table(allyears_p_conf$CLASSI_FIN)

#classifying data in dengue and severe dengue (simplified classification)
allyears_p_conf_ajust <- allyears_p_conf %>%
  mutate(CLASSI_SIMP = case_when((
    CLASSI_FIN %in% c("Dengue", "Dengue clássico") ~ "Dengue"), (CLASSI_FIN %in% c("Dengue com complicações", "Dengue com sinais de alarme", "Dengue grave", 
                                                                                   "Febre hemorrágica do dengue", "Síndrome do choque do dengue") ~ "Dengue grave"),
    TRUE ~ NA_character_
  ))

table(allyears_p_conf_ajust$CLASSI_SIMP)

#_____________________________________________________________________________________________________________________________________

#DETAILED DATASET (3 YEARS) - for dengue severity analysis
##2025##
#data download
dengueBR_2025_classif <- fetch_datasus(year_start = 2025, year_end = 2025, information_system = "SINAN-DENGUE",
                                         vars = c("SOROTIPO", "SG_UF_NOT","ID_MUNICIP","CLASSI_FIN", 
                                                  "DT_NOTIFIC", "DT_SIN_PRI", "ANO_NASC", "NU_IDADE_N", 
                                                  "CS_SEXO", "CS_GESTANT", "CS_RACA", "CS_ESCOL_N",
                                                  "ID_OCUPA_N", "DIABETES",	"HEMATOLOG", "HEPATOPAT",
                                                  "RENAL",	"HIPERTENSA",	"ACIDO_PEPT",	"AUTO_IMUNE",
                                                  "HOSPITALIZ",	"DT_INTERNA", "EVOLUCAO"))


#selection of defined serotype only
dengueBR_2025_classif_sorotipo <- subset(dengueBR_2025_classif, dengueBR_2025_classif$SOROTIPO!="NA")

#data processing
dengueBR_2025_classif_sorotipo$MUNICIPIO = dengueBR_2025_classif_sorotipo$ID_MUNICIP 
table(is.na(dengueBR_2025_classif_sorotipo$MUNICIPIO)) 
dengueBR_2025_classif_sorotipo$MUNICIPIO <- as.character(dengueBR_2025_classif_sorotipo$MUNICIPIO)  
dengueBR_2025_classif_sorotipo_process = process_sinan_dengue(dengueBR_2025_classif_sorotipo, municipality_data = T)
dengueBR_2025_classif_sorotipo_process = select(dengueBR_2025_classif_sorotipo_process, -25, -26, -27, -28, -29, -32, -33, -34, -36, -37, -38, -39, -40)

##2024##
#data download
dengueBR_2024_classif <- fetch_datasus(year_start = 2024, year_end = 2024, information_system = "SINAN-DENGUE",
                                       vars = c("SOROTIPO", "SG_UF_NOT","ID_MUNICIP","CLASSI_FIN", 
                                                "DT_NOTIFIC", "DT_SIN_PRI", "ANO_NASC", "NU_IDADE_N", 
                                                "CS_SEXO", "CS_GESTANT", "CS_RACA", "CS_ESCOL_N",
                                                "ID_OCUPA_N", "DIABETES",	"HEMATOLOG", "HEPATOPAT",
                                                "RENAL",	"HIPERTENSA",	"ACIDO_PEPT",	"AUTO_IMUNE",
                                                "HOSPITALIZ",	"DT_INTERNA", "EVOLUCAO"))


#selection of defined serotype only
dengueBR_2024_classif_sorotipo <- subset(dengueBR_2024_classif, dengueBR_2024_classif$SOROTIPO!="NA")

#data processing
dengueBR_2024_classif_sorotipo$MUNICIPIO = dengueBR_2024_classif_sorotipo$ID_MUNICIP 
table(is.na(dengueBR_2024_classif_sorotipo$MUNICIPIO)) 
dengueBR_2024_classif_sorotipo$MUNICIPIO <- as.character(dengueBR_2024_classif_sorotipo$MUNICIPIO)  
dengueBR_2024_classif_sorotipo_process = process_sinan_dengue(dengueBR_2024_classif_sorotipo, municipality_data = T)
dengueBR_2024_classif_sorotipo_process = select(dengueBR_2024_classif_sorotipo_process, -25, -26, -27, -28, -29, -32, -33, -34, -36, -37, -38, -39, -40)

##2023##
#data download
dengueBR_2023_classif <- fetch_datasus(year_start = 2023, year_end = 2023, information_system = "SINAN-DENGUE",
                                       vars = c("SOROTIPO", "SG_UF_NOT","ID_MUNICIP","CLASSI_FIN", 
                                                "DT_NOTIFIC", "DT_SIN_PRI", "ANO_NASC", "NU_IDADE_N", 
                                                "CS_SEXO", "CS_GESTANT", "CS_RACA", "CS_ESCOL_N",
                                                "ID_OCUPA_N", "DIABETES",	"HEMATOLOG", "HEPATOPAT",
                                                "RENAL",	"HIPERTENSA",	"ACIDO_PEPT",	"AUTO_IMUNE",
                                                "HOSPITALIZ",	"DT_INTERNA", "EVOLUCAO"))


#selection of defined serotype only
dengueBR_2023_classif_sorotipo <- subset(dengueBR_2023_classif, dengueBR_2023_classif$SOROTIPO!="NA")

#data processing
dengueBR_2023_classif_sorotipo$MUNICIPIO = dengueBR_2023_classif_sorotipo$ID_MUNICIP 
table(is.na(dengueBR_2023_classif_sorotipo$MUNICIPIO)) 
dengueBR_2023_classif_sorotipo$MUNICIPIO <- as.character(dengueBR_2023_classif_sorotipo$MUNICIPIO)  
dengueBR_2023_classif_sorotipo_process = process_sinan_dengue(dengueBR_2023_classif_sorotipo, municipality_data = T)
dengueBR_2023_classif_sorotipo_process = select(dengueBR_2023_classif_sorotipo_process, -25, -26, -27, -28, -29, -32, -33, -34, -36, -37, -38, -39, -40)
