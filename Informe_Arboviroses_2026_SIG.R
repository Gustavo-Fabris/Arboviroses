########    SIG    ########

RS22_2025_SINAN_DECODIFICADO$Data_Primeiros_Sintomas <- as.Date(RS22_2025_SINAN_DECODIFICADO$Data_Primeiros_Sintomas)

AUX <- RS22_2025_SINAN_DECODIFICADO

AUX <- RS22_2025_SINAN_DECODIFICADO %>%
  mutate(Abertas = as.numeric(difftime(RS22_2025_SINAN_DECODIFICADO$Data_Primeiros_Sintomas, Sys.Date(), units = "days") * -1))

AUX <- AUX %>%
  filter(Abertas <= 70)

RS22_2025_SINAN_10S <- AUX[, -ncol(AUX)]

sheet_write(RS22_2025_SINAN_10S, 
            ss = "https://docs.google.com/spreadsheets/d/1ul_41n7CWg7YG0jAMKGoh4u_pmVXCmSeOnh6cOsXr9k/edit?pli=1&gid=990855477#gid=990855477", 
            sheet = "SINAN_10S")

write.csv(RS22_2025_SINAN_10S, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAN_10S.csv",
          row.names = FALSE)

## Preparando a tabela de dados regional para fazer o merge no qgis

AUX <- RS22_2025_GERAL[, 2:11]

AUX <- AUX %>%
  mutate(Provaveis = RS22_2025_GERAL$Notificados - RS22_2025_GERAL$Descartados,
         DENV_I = case_when(RS22_2025_GERAL$DENV_I > 0 ~ "I"),
         DENV_II = case_when(RS22_2025_GERAL$DENV_II > 0 ~ "II"),
         DENV_III = case_when(RS22_2025_GERAL$DENV_III > 0 ~ "III"),
         DENV_IV = case_when(RS22_2025_GERAL$DENV_IV > 0 ~ "IV"))

AUX <- AUX %>%
  mutate(Incidencia_Provaveis = format(round((AUX$Provaveis/AUX$Populacao)*100000, 2))
  )

AUX$SOROTIPOS <- paste0(AUX[ , 12], ", ", AUX[ , 13],", ", AUX[ , 14],", ", AUX[ , 15])

sheet_write(AUX, ss = "https://docs.google.com/spreadsheets/d/1ul_41n7CWg7YG0jAMKGoh4u_pmVXCmSeOnh6cOsXr9k/edit?gid=1019073941#gid=1019073941", 
            sheet = "Dengue")

write.csv(AUX, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SIG_DENGUE.csv",
          row.names = FALSE)

## Preparando a tabela Estadual para o merge no qgis

AUX <- PR_DENGUE_2025_GERAL[, 2:17]

AUX <- AUX %>%
  mutate(Provaveis = PR_DENGUE_2025_GERAL$Notificados - PR_DENGUE_2025_GERAL$Descartados,
         DENV_I = case_when(PR_DENGUE_2025_GERAL$DENV_I > 0 ~ "I"),
         DENV_II = case_when(PR_DENGUE_2025_GERAL$DENV_II > 0 ~ "II"),
         DENV_III = case_when(PR_DENGUE_2025_GERAL$DENV_III > 0 ~ "III"),
         DENV_IV = case_when(PR_DENGUE_2025_GERAL$DENV_IV > 0 ~ "IV"))

AUX$SOROTIPOS <- paste0(AUX[ , 17], ", ", AUX[ , 18],", ", AUX[ , 19],", ", AUX[ , 20])

sheet_write(AUX, ss = "https://docs.google.com/spreadsheets/d/1ul_41n7CWg7YG0jAMKGoh4u_pmVXCmSeOnh6cOsXr9k/edit?gid=1019073941#gid=1019073941", 
            sheet = "Dengue_PR")

write.csv(AUX, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/PR_2025_SIG_DENGUE.csv",
          row.names = FALSE)

## Preparando a tabela Estadual Chikungunya para o merge no qgis

AUX <- PR_CHIK_2025_GERAL

sheet_write(AUX, ss = "https://docs.google.com/spreadsheets/d/1ul_41n7CWg7YG0jAMKGoh4u_pmVXCmSeOnh6cOsXr9k/edit?gid=1019073941#gid=1019073941", 
            sheet = "Chikungunya_PR")

write.csv(AUX, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/PR_2025_SIG_CHIK.csv",
          row.names = FALSE)

## Preparando a tabela Chikungunya para o merge no qgis

AUX <- PR_CHIK_2025_GERAL[which(PR_CHIK_2025_GERAL[,1] == "22"),]

sheet_write(AUX, ss = "https://docs.google.com/spreadsheets/d/1ul_41n7CWg7YG0jAMKGoh4u_pmVXCmSeOnh6cOsXr9k/edit?gid=1019073941#gid=1019073941", 
            sheet = "Chikungunya")

write.csv(AUX, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SIG_CHIK.csv",
          row.names = FALSE)

#######  JARDIM ALEGRE 

##  Acertando a planilha de ovitrampas para conter só as últimas 4 leituras

RS22_2025_SIG_JARDIM_OVITRAMPAS <- read_sheet ("https://docs.google.com/spreadsheets/d/13HgONdkmmWhhufINnwVzmZhxBBnOLDmZm75uGR5R7lU/edit", 
                                               sheet ="Rede Ovitrampas")

AUX <- RS22_2025_SIG_JARDIM_OVITRAMPAS[, c(1:8, 
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -15,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -14,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -13,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -12,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -11,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -10,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -9,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -8,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -7,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -6,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -5,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -4,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -3,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -2,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS) -1,
                                           ncol(RS22_2025_SIG_JARDIM_OVITRAMPAS)), ]


RS22_2025_SIG_JARDIM_OVITRAMPAS <- AUX

sheet_write(RS22_2025_SIG_JARDIM_OVITRAMPAS, ss = "https://docs.google.com/spreadsheets/d/13HgONdkmmWhhufINnwVzmZhxBBnOLDmZm75uGR5R7lU/edit?gid=297610919#gid=297610919", 
            sheet = "SIG_OVITRAMPAS")

#### SINAN 10 semanas - JARDIM ALEGRE

RS22_Jardim_Alegre_2025_SINAN_10S <- RS22_2025_SINAN_10S %>% 
  filter(Municipio_Residencia == "JARDIM ALEGRE")

sheet_write(RS22_Jardim_Alegre_2025_SINAN_10S, ss = "https://docs.google.com/spreadsheets/d/13HgONdkmmWhhufINnwVzmZhxBBnOLDmZm75uGR5R7lU/edit?gid=2130934097#gid=2130934097", 
            sheet = "Notificações_10S")

#######  IVAIPORÃ 

##  Acertando a planilha de ovitrampas para conter só as últimas 4 leituras

RS22_2025_SIG_IVAIPORA_OVITRAMPAS <- read_sheet ("https://docs.google.com/spreadsheets/d/1ITIc1Iu3vTl7SwFwsJjevln866P1x0r1sc0JBbMs-IA/edit?gid=1953803859#gid=1953803859", 
                                                 sheet ="Rede Ovitrampas")

AUX <- RS22_2025_SIG_IVAIPORA_OVITRAMPAS[, c(1:8, 
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -15,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -14,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -13,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -12,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -11,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -10,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -9,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -8,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -7,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -6,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -5,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -4,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -3,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -2,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS) -1,
                                             ncol(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)), ]


RS22_2025_SIG_IVAIPORA_OVITRAMPAS <- AUX

sheet_write(RS22_2025_SIG_IVAIPORA_OVITRAMPAS, ss = "https://docs.google.com/spreadsheets/d/1ITIc1Iu3vTl7SwFwsJjevln866P1x0r1sc0JBbMs-IA/edit?gid=1751658261#gid=1751658261", 
            sheet = "SIG_OVITRAMPAS")

####  Rede Ovitrap

RS22_2025_SIG_IVAIPORA_Mosquitrap <- read_sheet ("https://docs.google.com/spreadsheets/d/1ITIc1Iu3vTl7SwFwsJjevln866P1x0r1sc0JBbMs-IA/edit?gid=1953803859#gid=1953803859", 
                                                 sheet ="Rede Mosquitrap")

AUX <- RS22_2025_SIG_IVAIPORA_Mosquitrap[, c(1:7, 
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -15,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -14,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -13,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -12,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -11,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -10,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -9,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -8,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -7,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -6,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -5,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -4,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -3,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -2,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap) -1,
                                             ncol(RS22_2025_SIG_IVAIPORA_Mosquitrap)
), ]


RS22_2025_SIG_IVAIPORA_Mosquitrap <- AUX

sheet_write(RS22_2025_SIG_IVAIPORA_Mosquitrap, ss = "https://docs.google.com/spreadsheets/d/1ITIc1Iu3vTl7SwFwsJjevln866P1x0r1sc0JBbMs-IA/edit?gid=1751658261#gid=1751658261", 
            sheet = "SIG_Mosquitrap")

#### SINAN 10 semanas - Ivaiporã

RS22_Ivaiporã_2025_SINAN_10S <- RS22_2025_SINAN_10S %>% 
  filter(Municipio_Residencia == "IVAIPORÃ")

sheet_write(RS22_Ivaiporã_2025_SINAN_10S, ss = "https://docs.google.com/spreadsheets/d/1ITIc1Iu3vTl7SwFwsJjevln866P1x0r1sc0JBbMs-IA/edit?gid=192070648#gid=192070648", 
            sheet = "Notificações_10S")

#######  Nova Tebas

##  Acertando a planilha de ovitrampas para conter só as últimas 4 leituras

RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS <- read_sheet ("https://docs.google.com/spreadsheets/d/1etqgUoPBa5nPOeJiR9QfenXhPQPzWGubb2WRjGjtJds/edit?gid=242017248#gid=242017248",
                                                   sheet ="Rede Ovitrampas")

AUX <- RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS[, c(1:8,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -15,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -14,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -13,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -12,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -11,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -10,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -9,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -8,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -7,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -6,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -5,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -4,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -3,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -2,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS) -1,
                                               ncol(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)), ]


RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS <- AUX

sheet_write(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS, 
            ss = "https://docs.google.com/spreadsheets/d/1etqgUoPBa5nPOeJiR9QfenXhPQPzWGubb2WRjGjtJds/edit?gid=718202382#gid=718202382",
            sheet = "SIG_OVITRAMPAS")

#### SINAN 10 semanas - Nova Tebas

RS22_NOVA_TEBAS_2025_SINAN_10S <- RS22_2025_SINAN_10S %>%
  filter(Municipio_Residencia == "NOVA TEBAS")

sheet_write(RS22_NOVA_TEBAS_2025_SINAN_10S, ss = "https://docs.google.com/spreadsheets/d/1etqgUoPBa5nPOeJiR9QfenXhPQPzWGubb2WRjGjtJds/edit?gid=1102152649#gid=1102152649",
            sheet = "Notificações_10S")

#######  Lunardelli

##  Acertando a planilha de ovitrampas para conter só as últimas 4 leituras

RS22_2025_SIG_LUNARDELLI_OVITRAMPAS <- read_sheet ("https://docs.google.com/spreadsheets/d/1_TqCW57lEt2B5HiQs06E9ov8K4jVnhA7pOp51wwBbUI/edit?gid=1884457560#gid=1884457560",
                                                   sheet ="Rede Ovitrampas")

AUX <- RS22_2025_SIG_LUNARDELLI_OVITRAMPAS[, c(1:8,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -15,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -14,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -13,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -12,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -11,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -10,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -9,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -8,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -7,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -6,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -5,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -4,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -3,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -2,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS) -1,
                                               ncol(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)), ]


RS22_2025_SIG_LUNARDELLI_OVITRAMPAS <- AUX

sheet_write(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS, 
            ss = "https://docs.google.com/spreadsheets/d/1_TqCW57lEt2B5HiQs06E9ov8K4jVnhA7pOp51wwBbUI/edit?gid=1041871660#gid=1041871660",
            sheet = "SIG_OVITRAMPAS")

#### SINAN 10 semanas - LUNARDELLI

RS22_LUNARDELLI_2025_SINAN_10S <- RS22_2025_SINAN_10S %>%
  filter(Municipio_Residencia == "LUNARDELLI")

sheet_write(RS22_LUNARDELLI_2025_SINAN_10S, 
            ss = "https://docs.google.com/spreadsheets/d/1_TqCW57lEt2B5HiQs06E9ov8K4jVnhA7pOp51wwBbUI/edit?gid=536550572#gid=536550572",
            sheet = "Notificações_10S")


#######  São João do Ivaí

##  Acertando a planilha de ovitrampas para conter só as últimas 4 leituras

RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS <- read_sheet ("https://docs.google.com/spreadsheets/d/1oEr3yDN7klgpSaGBWgpsDhRY7S0-8yMRoTp0Io-E_ks/edit?gid=1873381537#gid=1873381537",
                                                         sheet ="Rede Ovitrampas")

AUX <- RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS[, c(1:8,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -15,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -14,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -13,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -12,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -11,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -10,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -9,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -8,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -7,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -6,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -5,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -4,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -3,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -2,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS) -1,
                                                     ncol(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)), ]


RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS <- AUX

sheet_write(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS, 
            ss = "https://docs.google.com/spreadsheets/d/1oEr3yDN7klgpSaGBWgpsDhRY7S0-8yMRoTp0Io-E_ks/edit?gid=1080166106#gid=1080166106",
            sheet = "SIG_OVITRAMPAS")

#### SINAN 10 semanas - São João do Ivaí

RS22_SAO_JOAO_DO_IVAI_2025_SINAN_10S <- RS22_2025_SINAN_10S %>%
  filter(Municipio_Residencia == "SÃO JOÃO DO IVAÍ")

sheet_write(RS22_SAO_JOAO_DO_IVAI_2025_SINAN_10S, 
            ss = "https://docs.google.com/spreadsheets/d/1oEr3yDN7klgpSaGBWgpsDhRY7S0-8yMRoTp0Io-E_ks/edit?gid=2017988045#gid=2017988045",
            sheet = "Notificações_10S")

#######  Lidianópolis

##  Acertando a planilha de ovitrampas para conter só as últimas 4 leituras

RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS <- read_sheet ("https://docs.google.com/spreadsheets/d/1kf5EuZ_-gOjogpVWkM5edv-yjIaPloSt0KrZK03V21w/edit?gid=1745514602#gid=1745514602",
                                                     sheet ="Rede Ovitrampas")

AUX <- RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS[, c(1:8,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -15,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -14,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -13,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -12,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -11,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -10,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -9,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -8,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -7,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -6,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -5,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -4,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -3,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -2,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS) -1,
                                                 ncol(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS)), ]


RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS <- AUX

sheet_write(RS22_2025_SIG_LIDIANOPOLIS_OVITRAMPAS, 
            ss = "https://docs.google.com/spreadsheets/d/1kf5EuZ_-gOjogpVWkM5edv-yjIaPloSt0KrZK03V21w/edit?gid=886661365#gid=886661365",
            sheet = "SIG_OVITRAMPAS")

#### SINAN 10 semanas - LIDIANOPOLIS

RS22_LIDIANOPOLIS_2025_SINAN_10S <- RS22_2025_SINAN_10S %>%
  filter(Municipio_Residencia == "LIDIANÓPOLIS")

sheet_write(RS22_LIDIANOPOLIS_2025_SINAN_10S, 
            ss = "https://docs.google.com/spreadsheets/d/1kf5EuZ_-gOjogpVWkM5edv-yjIaPloSt0KrZK03V21w/edit?gid=1764843095#gid=1764843095",
            sheet = "Notificações_10S")

##############   Planilha Ovitrampas Regional   ########

colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[9] <- "Instalacao_1"
colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[10] <- "Coleta_1"
colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[11] <- "OBS_1"
colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[12] <- "Resultado_1"

colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[13] <- "Instalacao_2"
colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[14] <- "Coleta_2"
colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[15] <- "OBS_2"
colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[16] <- "Resultado_2"

colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[17] <- "Instalacao_3"
colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[18] <- "Coleta_3"
colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[19] <- "OBS_3"
colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[20] <- "Resultado_3"

colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[21] <- "Instalacao_4"
colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[22] <- "Coleta_4"
colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[23] <- "OBS_4"
colnames(RS22_2025_SIG_JARDIM_OVITRAMPAS)[24] <- "Resultado_4"

colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[9] <- "Instalacao_1"
colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[10] <- "Coleta_1"
colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[11] <- "OBS_1"
colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[12] <- "Resultado_1"

colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[13] <- "Instalacao_2"
colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[14] <- "Coleta_2"
colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[15] <- "OBS_2"
colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[16] <- "Resultado_2"

colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[17] <- "Instalacao_3"
colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[18] <- "Coleta_3"
colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[19] <- "OBS_3"
colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[20] <- "Resultado_3"

colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[21] <- "Instalacao_4"
colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[22] <- "Coleta_4"
colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[23] <- "OBS_4"
colnames(RS22_2025_SIG_IVAIPORA_OVITRAMPAS)[24] <- "Resultado_4"

colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[9] <- "Instalacao_1"
colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[10] <- "Coleta_1"
colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[11] <- "OBS_1"
colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[12] <- "Resultado_1"

colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[13] <- "Instalacao_2"
colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[14] <- "Coleta_2"
colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[15] <- "OBS_2"
colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[16] <- "Resultado_2"

colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[17] <- "Instalacao_3"
colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[18] <- "Coleta_3"
colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[19] <- "OBS_3"
colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[20] <- "Resultado_3"

colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[21] <- "Instalacao_4"
colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[22] <- "Coleta_4"
colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[23] <- "OBS_4"
colnames(RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS)[24] <- "Resultado_4"

colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[9] <- "Instalacao_1"
colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[10] <- "Coleta_1"
colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[11] <- "OBS_1"
colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[12] <- "Resultado_1"

colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[13] <- "Instalacao_2"
colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[14] <- "Coleta_2"
colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[15] <- "OBS_2"
colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[16] <- "Resultado_2"

colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[17] <- "Instalacao_3"
colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[18] <- "Coleta_3"
colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[19] <- "OBS_3"
colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[20] <- "Resultado_3"

colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[21] <- "Instalacao_4"
colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[22] <- "Coleta_4"
colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[23] <- "OBS_4"
colnames(RS22_2025_SIG_LUNARDELLI_OVITRAMPAS)[24] <- "Resultado_4"

colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[9] <- "Instalacao_1"
colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[10] <- "Coleta_1"
colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[11] <- "OBS_1"
colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[12] <- "Resultado_1"

colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[13] <- "Instalacao_2"
colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[14] <- "Coleta_2"
colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[15] <- "OBS_2"
colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[16] <- "Resultado_2"

colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[17] <- "Instalacao_3"
colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[18] <- "Coleta_3"
colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[19] <- "OBS_3"
colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[20] <- "Resultado_3"

colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[21] <- "Instalacao_4"
colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[22] <- "Coleta_4"
colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[23] <- "OBS_4"
colnames(RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)[24] <- "Resultado_4"

teste <- rbind(RS22_2025_SIG_JARDIM_OVITRAMPAS, 
               RS22_2025_SIG_IVAIPORA_OVITRAMPAS,
               RS22_2025_SIG_NOVA_TEBAS_OVITRAMPAS,
               RS22_2025_SIG_LUNARDELLI_OVITRAMPAS,
               RS22_2025_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS)







