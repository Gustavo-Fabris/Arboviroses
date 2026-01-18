# Base de Dados: Bases DBF do SINAN; Planilhas de exames executados pelo LACEN e Planilhas de Monitoramento
#                Municipais (Google Drive)

rm(list = ls())


setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/")

#########################################################################################

Fonte <- "Fonte: SINAN Online. BASE DBF acessada em 13/01/2026"   ##### Fonte dos gráficos relacionados ao SINAN

Fonte_1 <- "Fonte: Lacen. Acesso em 15/01/2026"            ##### Fonte dos gráficos relacionados ao LACEN

Fonte_2 <- "Fonte: Planilhas de Controle Municipais. Acesso em 15/01/2026"     ##### Fonte dos gráficos relacionados às Planilhas Municipais

#########################################################################################

SE <- as.data.frame("10")  ### Colocar a Semana Epidemiológica atual

SE <- as.numeric(SE)

##########################################################################################

RS <- 22   #####  Colocar AQUI a Regional

####  libraries a serem utilizadas  ###

library(patchwork)
library(foreign)
library (dplyr)
library (googlesheets4)
library (ggplot2)
library (httpuv)
library(stringr)
library(lubridate)
library(ggspatial)
library(sf)
library(tidyr)
library(gt)

####  Importando as bases de dados para formulação do Informe Epidemiológico      ####

BASE_IBGE<-read.table(file="Base_de_Dados/Auxiliares/Planilha_Base_IBGE.csv", 
                      header=TRUE, 
                      sep=",")

BASE_IBGE_BRASIL <- read.csv (file = "Base_de_Dados/Auxiliares/Planilha_Base_IBGE_BRASIL.csv",
                              header = TRUE,
                              sep = ",")

######   Criando objeto ID_REG. Será utilizado para selecionar
######   RS no DBF do SINAN ONLINE.

ID_REG <- as.data.frame(BASE_IBGE[which(BASE_IBGE$RS == RS), 6])

ID_REG <- as.numeric(ID_REG[1,1])

####   Estabelecendo o número de municípios em cada RS

nrow <- NROW(BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

####  Base DBF do SINAN. Deve-se baixá-las, renomeá-las e salvá-las no diretório correto  ######

DENGON2026 <- read.dbf(file = "Base_de_Dados/DBF/DENGON2026.dbf", 
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, 
                                                 NU_ANO, SEM_NOT, DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC,
                                                 NU_IDADE_N, CS_SEXO, CS_GESTANT, CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, 
                                                 NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, 
                                                 CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N,
                                                 LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, 
                                                 AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, SOROTIPO, CLASSI_FIN, CRITERIO, 
                                                 TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, DT_INTERNA, DT_OBITO, 
                                                 DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, ALRM_VOM, 
                                                 ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, 
                                                 GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST,
                                                 GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

CHIKON2026 <- read.dbf("Base_de_Dados/DBF/CHIKON2026.dbf",
                       as.is = FALSE) %>% select(ID_REGIONA, NU_NOTIFIC, ID_GEO1,ID_GEO2, ID_AGRAVO, ID_REGIONA, DT_NOTIFIC, NU_ANO, SEM_NOT, 
                                                 DT_SIN_PRI, SEM_PRI,  SG_UF_NOT, ID_MUNICIP, NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, CS_GESTANT, 
                                                 CS_ESCOL_N, NM_MAE_PAC, ID_MN_RESI, SG_UF, ID_RG_RESI, NM_LOGRADO, NU_NUMERO, NM_BAIRRO, NU_CEP, 
                                                 CS_ZONA, DT_DIGITA, DT_INVEST, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, 
                                                 CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, 
                                                 HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, DT_SORO, RESUL_SORO, DT_PCR, RESUL_PCR_, 
                                                 SOROTIPO, CLASSI_FIN, CRITERIO, TPAUTOCTO, COUFINF, COMUNINF, CO_BAINF, EVOLUCAO, HOSPITALIZ, 
                                                 DT_INTERNA, DT_OBITO, DT_ENCERRA, DT_ALRM, ALRM_LETAR, ALRM_HEPAT, ALRM_LIQ, ALRM_HIPOT, ALRM_PLAQ, 
                                                 ALRM_VOM, ALRM_SANG, ALRM_HEMAT, ALRM_ABDOM, DT_GRAV, GRAV_PULSO, GRAV_CONV, GRAV_ENCH, GRAV_INSUF, 
                                                 GRAV_TAQUI, GRAV_EXTRE, GRAV_HIPOT, GRAV_HEMAT, GRAV_MELEN, GRAV_METRO, GRAV_SANG, GRAV_AST, 
                                                 GRAV_MIOC, GRAV_CONSC, GRAV_ORGAO, MANI_HEMOR, EPISTAXE, GENGIVO, METRO, DS_OBS)

NINDINET2026 <- read.dbf("Base_de_Dados/DBF/NINDINET2026.DBF",
                         as.is = FALSE) %>% select(NU_NOTIFIC, ID_AGRAVO, DT_NOTIFIC, SEM_NOT, NU_ANO, ID_MUNICIP,
                                                   ID_REGIONA, DT_SIN_PRI, SEM_PRI, NM_PACIENT, NU_IDADE_N, CS_SEXO,
                                                   ID_MN_RESI, CLASSI_FIN, TPAUTOCTO) %>%
  filter(ID_AGRAVO == "A928")

RS_Serie_Historica_Base <- read.csv(file = paste0("Tabulacoes_R/Arboviroses/RS", RS, "_Serie_Historica_Base.csv"),
                                    header = TRUE,
                                    sep = ",")

RS_CE_Notificados_Base <- read.csv(file = paste0("Tabulacoes_R/Arboviroses/RS", RS, "_CE_Notificados_Base.csv"),
                                   header = TRUE,
                                   sep = ",")

#### Removendo 2024 e o final de 2023 dos CSV

RS_CE_Notificados_Base <- RS_CE_Notificados_Base[-16,]

RS_CE_Notificados_Base[15, 33:54] <- 0

RS_CE_Confirmados_Base <- read.csv(file = paste0("Tabulacoes_R/Arboviroses/RS", RS, "_CE_Confirmados_Base.csv"),
                                   header = TRUE,
                                   sep = ",")

#### Removendo 2024 e o final de 2023 dos CSV

RS_CE_Confirmados_Base <- RS_CE_Confirmados_Base[-16,]

RS_CE_Confirmados_Base[15, 33:54] <- 0

######################################################

RS_CE_Notificados_SEDE_Base <- read.csv(file = paste0("Tabulacoes_R/Arboviroses/RS", RS, "_CE_Notificados_Sede_Base.csv"),
                                        header = TRUE,
                                        sep = ",")

#### Removendo 2024 e o final de 2023 dos CSV

RS_CE_Notificados_SEDE_Base <- RS_CE_Notificados_SEDE_Base[-16,]

RS_CE_Notificados_SEDE_Base[15, 33:54] <- 0

######################################################

RS_CE_Confirmados_SEDE_Base <- read.csv(file = paste0("Tabulacoes_R/Arboviroses/RS", RS, "_CE_Confirmados_Sede_BASE.csv"),
                                        header = TRUE,
                                        sep = ",")

#### Removendo 2024 e o final de 2023 dos CSV

RS_CE_Confirmados_SEDE_Base <- RS_CE_Confirmados_SEDE_Base[-16,]

RS_CE_Confirmados_SEDE_Base[15, 33:54] <- 0

######################################################

RS_CE_Notificados_JARDIM_BASE <- read.csv(file = paste0("Tabulacoes_R/Arboviroses/RS", RS, "_CE_Notificados_JARDIM.csv"),
                                          header = TRUE,
                                          sep = ",")

#### Removendo 2024 e o final de 2023 dos CSV

RS_CE_Notificados_JARDIM_BASE <- RS_CE_Notificados_JARDIM_BASE[-16,]

RS_CE_Notificados_JARDIM_BASE[15, 33:54] <- 0

######################################################

RS_CE_Confirmados_JARDIM_BASE <- read.csv(file = paste0("Tabulacoes_R/Arboviroses/RS", RS, "_CE_Confirmados_JARDIM.csv"),
                                          header = TRUE,
                                          sep = ",")

#### Removendo 2024 e o final de 2023 dos CSV

RS_CE_Confirmados_JARDIM_BASE <- RS_CE_Confirmados_JARDIM_BASE[-16,]

RS_CE_Confirmados_JARDIM_BASE[15, 33:54] <- 0

######################################################

RS_CE_Notificados_SAO_JOAO_BASE <- read.csv(file = paste0("Tabulacoes_R/Arboviroses/RS", RS, "_CE_Notificados_SAO_JOAO_DO_IVAI.csv"),
                                            header = TRUE,
                                            sep = ",")

#### Removendo 2024 e o final de 2023 dos CSV

RS_CE_Notificados_SAO_JOAO_BASE <- RS_CE_Notificados_SAO_JOAO_BASE[-16,]

RS_CE_Notificados_SAO_JOAO_BASE[15, 33:54] <- 0

######################################################

RS_CE_Confirmados_SAO_JOAO_BASE <- read.csv(file = paste0("Tabulacoes_R/Arboviroses/RS", RS, "_CE_Confirmados_SAO_JOAO_DO_IVAI.csv"),
                                            header = TRUE,
                                            sep = ",")

#### Removendo 2024 e o final de 2023 dos CSV

RS_CE_Confirmados_SAO_JOAO_BASE <- RS_CE_Confirmados_SAO_JOAO_BASE[-16,]

RS_CE_Confirmados_SAO_JOAO_BASE[15, 33:54] <- 0


######################################################

RS_CE_Notificados_LUNARDELLI_BASE <- read.csv(file = paste0("Tabulacoes_R/Arboviroses/RS", RS, "_CE_Notificados_LUNARDELLI.csv"),
                                            header = TRUE,
                                            sep = ",")

#### Removendo 2024 e o final de 2023 dos CSV

RS_CE_Notificados_LUNARDELLI_BASE <- RS_CE_Notificados_LUNARDELLI_BASE[-16,]

RS_CE_Notificados_LUNARDELLI_BASE[15, 33:54] <- 0

######################################################

RS_CE_Confirmados_LUNARDELLI_BASE <- read.csv(file = paste0("Tabulacoes_R/Arboviroses/RS", RS, "_CE_Confirmados_LUNARDELLI.csv"),
                                            header = TRUE,
                                            sep = ",")

#### Removendo 2024 e o final de 2023 dos CSV

RS_CE_Confirmados_LUNARDELLI_BASE <- RS_CE_Confirmados_LUNARDELLI_BASE[-16,]

RS_CE_Confirmados_LUNARDELLI_BASE[15, 33:54] <- 0

###################################################################################################
###      Transformando coluna de semana epidemiológica de fator para numérica                   ###
###      (passando por texto, se for direto, ela transforma ###200905 em 06.                    ###
###                                                                                             ###
###                  Será usado para buscar semanas epidemiológicas                             ###                       
###################################################################################################

DENGON2026$SEM_PRI <-as.numeric(as.character(DENGON2026$SEM_PRI))   

CHIKON2026$SEM_PRI <-as.numeric(as.character(CHIKON2026$SEM_PRI))  

################################################################
#####################         2026             #################
################################################################

source("Informe_Arboviroses_2026_SINAN_Tabelas.R")

source("Informe_Arboviroses_2026_SINAN_Decodificacao.R")

RS22_2026_SINAN_DECODIFICADO$SINAN <- as.numeric(as.character(RS22_2026_SINAN_DECODIFICADO$SINAN))

RS22_2026_COORDENADAS <- read_sheet("https://docs.google.com/spreadsheets/d/1teplPGKPT6R7Kfx1bXCcCBLQ0gHW8PFMS_6qBzkQdpk/edit?gid=178016067#gid=178016067", 
                                        sheet = "Consolidado")

AUX <- left_join(RS22_2026_SINAN_DECODIFICADO, 
                   RS22_2026_COORDENADAS, 
                   by = join_by(SINAN == Notificação))

AUX[, c(3, 4)] <- AUX[, c(98, 99)]

colnames(AUX)[c(3, 4)] <- c("Latitude", "Longitude")

sheet_write(RS22_2026_SINAN_DECODIFICADO,
            ss = "https://docs.google.com/spreadsheets/d/1z-cXrCe0ZRMRBG2rqW2BmG09HEa4tMmplXhHMnLbRg4/edit#gid=668044240",
            sheet = "Registros_SINAN")

source("Informe_Arboviroses_2026_Dengue_PR.R")

source("Informe_Arboviroses_2026_Chikungunya.R")

source("Informe_Arboviroses_2026_Canais_Endemicos.R")

source("Informe_Arboviroses_2026_Graficos_Geral.R")

source("Informe_Arboviroses_2026_Mapas_Geral.R")

source("Informe_Arboviroses_2026_Laboratorio.R")

####      Buscando a planilha de índices das ovitrampas no google drive

RS22_2026_INDICES_OVITRAMPAS <- read_sheet("https://docs.google.com/spreadsheets/d/1QWxmCxl7fPiE_6wnyPLZ-B7a0yXm6NxRhFAbhGOtHeU/edit?gid=863361484#gid=863361484", 
                                           sheet = "Consolidado")

source("Informe_Arboviroses_2026_Entomologia.R")

source("Informe_Arboviroses_2026_Inconsistencias.R")

####     Buscando a planilha RS22_2026_SINAN_DECODIFICADO do google sheets com as coordenadas geográficas inseridas pelos municípios   ####

RS22_2026_SINAN_DECODIFICADO <- read_sheet("https://docs.google.com/spreadsheets/d/167vkgU2HLIRk2JKAr_dsxOvhu1luR60e6eMnpw9iIPI/edit?gid=1437725143#gid=1437725143", 
                                           sheet= "SINAN")

RS22_2026_SINAN_PROVAVEIS_DECODIFICADO <- RS22_2026_SINAN_DECODIFICADO[-which(RS22_2026_SINAN_DECODIFICADO$Classificacao_Final == "DESCARTADO"),] 

source("Informe_Arboviroses_2026_SIG.R")

#####################################################################################################################

######     Buscando planilhas com dados dos municípios no google drive

RS22_2026_REDE_OVITRAMPAS <- read_sheet("https://docs.google.com/spreadsheets/d/1EejdDsQHZ_NLdglZPu1itFwiItBrYBpSyvpEoSN_L04/edit?gid=863361484#gid=863361484", 
                                        sheet = "Consolidado")

RS22_2026_RG_MUNICIPIOS <- read_sheet("https://docs.google.com/spreadsheets/d/1LTIvSToBinoLVUa2Wdp4_gdEb3Zj63J5sOBgWXXc-s4/edit?gid=1585473376#gid=1585473376")

RS22_2026_RG_MUNICIPIOS <- RS22_2026_RG_MUNICIPIOS[, -ncol(RS22_2026_RG_MUNICIPIOS)]
RS22_2026_RG_LOCALIDADES <- read_sheet("https://docs.google.com/spreadsheets/d/1jvWfa235GoyUE5-HMLAONTETgcFV91UoHCPTaVE0c0U/edit?gid=877642872#gid=877642872")

RS22_2026_PE <- read_sheet("https://docs.google.com/spreadsheets/d/1pen4eE4ZjEPybtLChkX7qCh8bP7O-vJSLRM2xGfUc4A/edit?gid=863361484#gid=863361484", 
                           sheet = "Consolidado")

RS22_2026_ASSISTENCIA <- read_sheet("https://docs.google.com/spreadsheets/d/1tmDS4llR2zNqxfhlY8FXDpY-OVk7hfeXNl6zsJmvbcM/edit?gid=863361484#gid=863361484",
                                    sheet = "Consolidado")

#####      Salvando planilhas como CSV

write.csv(PR_DENGUE_2026_GERAL, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/PR_2026_DENGUE_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_CHIK_2026_GERAL, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/PR_2026_CHIKUNGUNYA_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(RS22_2026_REDE_OVITRAMPAS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_REDE_OVITRAMPAS.csv",
          row.names = FALSE)

write.csv(RS22_2026_RG_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_RG_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(RS22_2026_RG_LOCALIDADES, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_RG_LOCALIDADES.csv",
          row.names = FALSE)

write.csv(RS22_2026_PE, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_PE.csv",
          row.names = FALSE)

write.csv(RS22_2026_ASSISTENCIA, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_ASSISTENCIA.csv",
          row.names = FALSE)

write.csv(RS22_2026_GERAL, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_GERAL.csv",
          row.names = FALSE)

write.csv(RS22_2026_EXTRA, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_EXTRA.csv",
          row.names = FALSE)

write.csv(RS22_2026_SINAN, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_SINAN.csv",
          row.names = FALSE)

write.csv(RS22_2026_SINAN_DECODIFICADO, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_SINAN_DECODIFICADO.csv",
          row.names = FALSE)

write.csv(RS22_2026_SINAIS_DE_ALARME, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_SINAIS_DE_ALARME.csv",
          row.names = FALSE)

write.csv(RS22_2026_SINAIS_Notificados, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_SINAIS_Notificados.csv",
          row.names = FALSE)

write.csv(RS22_2026_SINAIS_Confirmados, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_SINAIS_Confirmados.csv",
          row.names = FALSE)

write.csv(RS22_2026_SE_Confirmados, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_SE_Confirmados.csv",
          row.names = FALSE)

write.csv(RS22_2026_SE_Notificados, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_SE_Notificados.csv",
          row.names = FALSE)

write.csv(RS22_2026_DENGUE_GRAVE, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_SE_DENGUE_GRAVE.csv",
          row.names = FALSE)

write.csv (assign(paste0("RS", RS, "_2026_SE_Provaveis"), RS22_2026_SE_Provaveis),
           paste0("Tabulacoes_R/Arboviroses/RS", RS, "_2026_SE_Provaveis.csv"),
           row.names = FALSE)

write.csv(RS22_2026_DOENCAS_PRE_EXISTENTES, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_DOENCAS_PRE_EXISTENTES.csv",
          row.names = FALSE)

write.csv (RS22_Serie_Historica, 
           "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_Serie_Historica.csv", 
           row.names = FALSE)

write.csv (assign(paste0("RS", RS, "_2026_SE_Provaveis_CHIK"), RS22_2026_SE_Provaveis_CHIK),
           paste0("Tabulacoes_R/Arboviroses/RS", RS, "_2026_SE_Provaveis_CHIK.csv"),
           row.names = FALSE)

write.csv(RS22_2026_SINAN_DECODIFICADO,  "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_SINAN_DECODIFICADO.csv",
          row.names = FALSE)

write.csv(RS22_2026_SINAN_PROVAVEIS_DECODIFICADO, "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2026_SINAN_PROVAVEIS_DECODIFICADO.csv",
          row.names = FALSE)

#######################################################################################################################
##################################################################################################################
#####     Salvando os Gráficos, tabelas e Mapas

###          QUALIFICA SINAN - Residência Pag 05

gtsave(data = RS22_2026_TAB_Estrategicos,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_05A.png")

gtsave(data = RS22_2026_TAB_Estrategicos_Chik,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_05B.png")

gtsave(data = RS22_2026_TAB_Pop_Vulneravel,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_05C.png")

###          QUALIFICA SINAN - Investigação Pag 06

gtsave(data = RS22_2026_TAB_INCON_INVESTIGACAO,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_06A.png")

gtsave(data = RS22_2026_TAB_INCON_RESID_Municipios,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_06B.png")

###          QUALIFICA SINAN - Laboratório Pag 07

gtsave(data = RS22_2026_TAB_INCON_LABORATORIO_Municipios,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_07.png")

###          QUALIFICA SINAN - Hospitalização Pag 08

gtsave(data = RS22_2026_TAB_INCON_HOSPITALIZACAO,
       filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_08.png")

###          Série Histórica - Pag_09

RS22_2026_INFORME_Pag_05 <- (RS22_GRAF_Serie_Historica_Not_Conf / RS22_GRAF_Serie_Historica_Sorotipo /RS22_GRAF_Serie_Historica_Hospitalizados)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_09.png",
       RS22_2026_INFORME_Pag_05,
       width = 33,
       height = 40,
       units = "cm",)

###          Canal Endêmicos Notificados/Confirmados - Pag 10

RS22_2026_INFORME_Pag_06 <- (RS_2026_GRAF_CE_Notificados / RS_2026_GRAF_CE_Provaveis / RS_2026_GRAF_CE_Confirmados)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_10.png",
       RS22_2026_INFORME_Pag_06,
       width = 33,
       height = 40,
       units = "cm",)

###            Notificados/Confirmados - Pag 11

RS22_2026_INFORME_Pag_07 <- (RS22_GRAF_2026_Not_Conf / RS22_GRAF_2026_Autoctones / RS22_GRAF_2026_Investigacao)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_11.png",
       RS22_2026_INFORME_Pag_07,
       width = 33,
       height = 40,
       units = "cm",)

###           Autóctones/Descartados - Pag 12

RS22_2026_INFORME_Pag_08 <- (RS22_GRAF_2026_Hospitalizados / RS22_GRAF_2026_Descartados / RS22_GRAF_2026_Inconclusivos)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_12.png",
       RS22_2026_INFORME_Pag_08,
       width = 33,
       height = 40,
       units = "cm",)

###            Em Investigação/Incidência - Pag 13

RS22_2026_INFORME_Pag_09 <- (RS22_GRAF_2026_Encerramento / PR_DENGUE_2026_GRAF_SINAIS / RS22_GRAF_2026_SINAIS)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_13.png",
       RS22_2026_INFORME_Pag_09,
       width = 33,
       height = 40,
       units = "cm",)

####           Incidencia - Pag 14

RS22_2026_INFORME_Pag_10 <- (RS22_GRAF_2026_Incidencia / RS22_GRAF_2026_Incidencia_Provaveis)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_14.png",
       RS22_2026_INFORME_Pag_10,
       width = 33,
       height = 40,
       units = "cm",)

####           EXTRA - Pag 15

RS22_2026_INFORME_Pag_11 <- ((PR_2026_GRAF_PIRAMIDE + RS_2026_GRAF_PIRAMIDE) / 
                               (PR_GRAF_Escolaridade + RS22_GRAF_Escolaridade)/
                               (PR_GRAF_Zona + RS22_GRAF_Zona))

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_15.png",
       RS22_2026_INFORME_Pag_11,
       width = 55,
       height = 65,
       units = "cm",)

####          Canais Endêmicos - Pag 16

RS_2026_INFORME_CE_SEDE <- (RS_2026_GRAF_CE_Notificados_SEDE / RS_2026_GRAF_CE_Provaveis_SEDE / RS_2026_GRAF_CE_Confirmados_SEDE)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_16.png",
       RS_2026_INFORME_CE_SEDE,
       width = 33,
       height = 40,
       units = "cm",)

####         Canais Endêmicos JARDIM - Pag 17

RS_2026_CE_JARDIM <- (RS_2026_GRAF_CE_Notificados_JARDIM / RS_2026_GRAF_CE_Provaveis_JARDIM / RS_2026_GRAF_CE_Confirmados_JARDIM)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_17.png",
       RS_2026_CE_JARDIM,
       width = 33,
       height = 40,
       units = "cm",)

####         Canais Endêmicos JARDIM - Pag 18

RS_2026_CE_LUNARDELLI <- (RS_2026_GRAF_CE_Notificados_LUNARDELLI / RS_2026_GRAF_CE_Provaveis_LUNARDELLI / RS_2026_GRAF_CE_Confirmados_LUNARDELLI)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_18.png",
       RS_2026_CE_LUNARDELLI,
       width = 33,
       height = 40,
       units = "cm",)

####        Canais Endêmicos SAO JOAO pag 19

RS_2026_CE_SAO_JOAO <- (RS_2026_GRAF_CE_Notificados_SAO_JOAO_DO_IVAI / RS_2026_GRAF_CE_Provaveis_SAO_JOAO_DO_IVAI / RS_2026_GRAF_CE_Confirmados_SAO_JOAO_DO_IVAI)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_19.png",
       RS_2026_CE_SAO_JOAO,
       width = 33,
       height = 40,
       units = "cm",)

###        Histogramas Notificados - Pag 20

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_20.png",
       RS_2026_GRAF_Histograma_Notificados,
       width = 36,
       height = 46,
       units = "cm",)

###         Histogramas Confirmados - PAG 21

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_21.png",
       RS_2026_GRAF_Histograma_Confirmados,
       width = 36,
       height = 46,
       units = "cm",)

###       Histogramas Prováveis - PAG 22

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_22.png",
       RS_2026_GRAF_Histograma_Provaveis,
       width = 36,
       height = 46,
       units = "cm",)

#####       Mapa Incidência PR

RS22_2026_GRAF_1 <- (PR_2026_GRAF_INCIDENCIA_PR / PR_2026_GRAF_INCIDENCIA_PROV_PR)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_23.png",
       RS22_2026_GRAF_1,
       width = 36,
       height = 46,
       units = "cm",)

####      Mapa Chikungunya

RS22_2026_GRAF_1 <- (PR_2026_GRAF_CHIK_Notificados / PR_2026_GRAF_CHIK_Incidência)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_24.png",
       RS22_2026_GRAF_1,
       width = 36,
       height = 46,
       units = "cm",)

#####     Sinais Chikungunya PR

RS22_2026_GRAF_1 <- PR_2026_GRAF_SINAIS_CHIK 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_25A.png",
       RS22_2026_GRAF_1,
       width = 36,
       height = 23,
       units = "cm",)

####      Chikungunya Regional

RS22_2026_GRAF_1 <- (RS22_2026_GRAF_CHK_Not + RS22_2026_GRAF_CHK_Conf) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_25B.png",
       RS22_2026_GRAF_1,
       width = 36,
       height = 23,
       units = "cm",)

####      10 S Chik

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_26.png",
       RS_2026_GRAF_CHIK_Histograma_Notificados,
       width = 36,
       height = 46,
       units = "cm",)

####    10 S Chik COnfirmados

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_27.png",
       RS_2026_GRAF_CHIK_Histograma_Confirmados,
       width = 36,
       height = 46,
       units = "cm",)

####    10 S Chik Provaveis

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_28.png",
       RS_2026_GRAF_CHIK_Histograma_Provaveis,
       width = 36,
       height = 46,
       units = "cm",)

#### Zika Virus

RS22_2026_GRAF_1 <- (PR_2026_ZIKA_CHIK_Notificados / PR_2026_GRAF_ZIKA_Incidência)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_29.png",
       RS22_2026_GRAF_1,
       width = 36,
       height = 46,
       units = "cm",)

###       Sorotipo - Pag 23

RS22_2026_GRAF_1 <- (PR_2026_GRAF_SOROTIPO_PR / RS22_2026_GRAF_Sorotipo)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_30.png",
       RS22_2026_GRAF_1,
       width = 36,
       height = 46,
       units = "cm",)


RS22_2026_GRAF_1 <- (RS22_GRAF_2026_US_TOTAL / RS22_GRAF_2026_US_DETEC)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_31A.png",
       RS22_2026_GRAF_1,
       width = 46,
       height = 36,
       units = "cm",)

RS22_2026_GRAF_1 <- (RS22_2026_GRAF_SORO_TOTAL / RS22_2026_GRAF_SORO_REAG)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_31B.png",
       RS22_2026_GRAF_1,
       width = 46,
       height = 36,
       units = "cm",)

#####     Exames Municípios  

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_32.png",
       RS22_GRAF_LACEN_MUNIC,
       width = 36,
       height = 23,
       units = "cm",)

#####       Exames Chikungunya

RS22_2026_GRAF_1 <- (RS22_2026_GRAF_SORO_CHIK_TOTAL / RS22_2026_GRAF_SORO_CHIK_REAG)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_33.png",
       RS22_2026_GRAF_1,
       width = 46,
       height = 36,
       units = "cm",)

#####       Exames Chikungunya Municípios

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_34.png",
       RS22_GRAF_LACEN_MUNIC_CHIK,
       width = 36,
       height = 23,
       units = "cm",)

#### Dez semanas taxa de positividade

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_35.png",
       RS_2026_GRAF_Taxa_Positividade_Dengue_Mun,
       width = 36,
       height = 46,
       units = "cm",)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_36.png",
       RS_2026_GRAF_Taxa_Positividade_Chik_Mun,
       width = 36,
       height = 46,
       units = "cm",)

####    Entomologia

RS22_2026_GRAF_1 <- (RS22_2026_GRAF_IPO_ARAPUA / RS22_2026_GRAF_IDO_ARAPUA / 
                       RS22_2026_GRAF_IPO_ARIRANHA / RS22_2026_GRAF_IDO_ARIRANHA  /
                       RS22_2026_GRAF_IPO_CANDIDO / RS22_2026_GRAF_IDO_CANDIDO)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_37.png",
       RS22_2026_GRAF_1,
       width = 46,
       height = 56,
       units = "cm",)

RS22_2026_GRAF_1 <- (RS22_2026_GRAF_IPO_CRUZMALTINA / RS22_2026_GRAF_IDO_CRUZMALTINA / 
                       RS22_2026_GRAF_IPO_GODOY / RS22_2026_GRAF_IDO_GODOY / 
                       RS22_2026_GRAF_IPO_IVAIPORA / RS22_2026_GRAF_IDO_IVAIPORA )

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_38.png",
       RS22_2026_GRAF_1,
       width = 46,
       height = 56,
       units = "cm",)

RS22_2026_GRAF_1 <- (RS22_2026_GRAF_IPO_JARDIM / RS22_2026_GRAF_IDO_JARDIM / 
                       RS22_2026_GRAF_IPO_Lidianopolis / RS22_2026_GRAF_IDO_Lidianopolis / 
                       RS22_2026_GRAF_IPO_LUNARDELLI / RS22_2026_GRAF_IDO_LUNARDELLI )

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_39.png",
       RS22_2026_GRAF_1,
       width = 46,
       height = 56,
       units = "cm",)

RS22_2026_GRAF_1 <- (RS22_2026_GRAF_IPO_MANUEL_RIBAS / RS22_2026_GRAF_IDO_MANUEL_RIBAS / 
                       RS22_2026_GRAF_IPO_MATO_RICO / RS22_2026_GRAF_IDO_MATO_RICO / 
                       RS22_2026_GRAF_IPO_NOVA_TEBAS / RS22_2026_GRAF_IDO_NOVA_TEBAS )

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_40.png",
       RS22_2026_GRAF_1,
       width = 46,
       height = 56,
       units = "cm",)

RS22_2026_GRAF_1 <- (RS22_2026_GRAF_IPO_RIO_BRANCO / RS22_2026_GRAF_IDO_RIO_BRANCO / 
                       RS22_2026_GRAF_IPO_ROSARIO / RS22_2026_GRAF_IDO_ROSARIO / 
                       RS22_2026_GRAF_IPO_SANTA_MARIA / RS22_2026_GRAF_IDO_SANTA_MARIA )

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_41.png",
       RS22_2026_GRAF_1,
       width = 46,
       height = 56,
       units = "cm",)

RS22_2026_GRAF_1 <- (RS22_2026_GRAF_IPO_SAO_JOAO / RS22_2026_GRAF_IDO_SAO_JOAO  )

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2026_INFORME_Pag_42.png",
       RS22_2026_GRAF_1,
       width = 46,
       height = 14,
       units = "cm",)

#####    Removendo Objetos desnecessários

rm(RS22_GRAF_Serie_Historica_Not_Conf,
   RS22_GRAF_Serie_Historica_Sorotipo, 
   RS22_GRAF_Serie_Historica_Hospitalizados,
   RS_2026_GRAF_CE_Notificados,
   RS_2026_GRAF_CE_Provaveis,
   RS_2026_GRAF_CE_Confirmados,
   RS22_GRAF_2026_Incidencia,
   RS22_GRAF_2026_Not_Conf,
   RS22_GRAF_2026_Autoctones,
   RS22_GRAF_2026_Investigacao,
   RS22_GRAF_2026_Hospitalizados,
   RS22_GRAF_2026_Descartados,
   RS22_GRAF_2026_Inconclusivos,
   RS22_GRAF_2026_Incidencia_Provaveis,
   RS22_GRAF_Escolaridade,
   RS22_GRAF_LACEN_MUNIC_CHIK,
   RS22_2026_GRAF_IPO_ARAPUA,
   RS22_2026_GRAF_IPO_ARIRANHA,
   RS22_2026_GRAF_IPO_CANDIDO,
   RS22_2026_GRAF_IPO_CRUZMALTINA,
   RS22_2026_GRAF_IPO_GODOY,
   RS22_2026_GRAF_IPO_IVAIPORA,
   RS22_2026_GRAF_IPO_JARDIM,
   RS22_2026_GRAF_IPO_Lidianopolis,
   RS22_2026_GRAF_IPO_LUNARDELLI,
   RS22_2026_GRAF_IPO_MANUEL_RIBAS,
   RS22_2026_GRAF_IPO_MATO_RICO,
   RS22_2026_GRAF_IPO_NOVA_TEBAS,
   RS22_2026_GRAF_IPO_RIO_BRANCO,
   RS22_2026_GRAF_IPO_ROSARIO,
   RS22_2026_GRAF_IPO_SANTA_MARIA,
   RS22_2026_GRAF_IPO_SAO_JOAO,
   AUX_SEM,
   Quartil_1,
   Quartil_3,
   Theme,
   AUX_GRAF,
   AUX_HIST_CHIK_CONF_LIST,
   AUX_HIST_CHIK_NOT_LIST,
   AUX_HIST_CHIK_PROV_LIST,
   NINDINET2026,
   PR_2026_GRAF_PIRAMIDE,
   PR_GRAF_Escolaridade,
   PR_GRAF_Zona,
   RS_2026_GRAF_CE_Provaveis_SEDE,
   RS_2026_GRAF_CE_Confirmados_SAO_JOAO_DO_IVAI,
   RS_2026_GRAF_CE_Notificados_JARDIM,
   RS_2026_GRAF_CE_Notificados_SAO_JOAO_DO_IVAI,
   SHAPEFILE_ESTADUAL,
   SHAPEFILE_REGIONAL,
   SHAPEFILE_REGIONAL_Dissolvido,
   RS22_GRAF_Zona,
   AUX_2,
   AUX_I,
   AUX_II,
   AUX_III,
   AUX_IV,
   AUX_V,
   AUX_VI,
   AUX01,
   AUX02,
   AUX_HIST_CONF_LIST,
   AUX_HIST_NOT_LIST,
   AUX_HIST_PROV_LIST,
   MAPA_BASE,
   MAPA_BASE_PR,
   MAPA_BASE_RS,
   BASE_IBGE,
   BASE_IBGE_BRASIL,
   CHIKON2026,
   SINAN_DENGUE_RS,
   SINAN_CHIK_RS,
   RS22_GRAF_2026_Encerramento,
   RS22_GRAF_LACEN_MUNIC,
   Fonte,
   Fonte_1,
   Fonte_2,
   i,
   O,
   j,
   N,
   ID_REG,
   nrow,
   RS,
   SE,
   Theme_Hist,
   RS_2026_GRAF_Histograma_Confirmados,
   RS_2026_GRAF_Histograma_Provaveis,
   PR_2026_GRAF_CHIK_Incidência,
   PR_2026_GRAF_CHIK_Notificados,
   PR_2026_GRAF_INCIDENCIA_PR,
   PR_2026_GRAF_INCIDENCIA_PROV_PR,
   PR_2026_GRAF_SINAIS_CHIK,
   PR_2026_GRAF_SOROTIPO_PR,
   PR_2026_GRAF_ZIKA_Incidência,
   PR_2026_CHIK_SINAIS_Confirmados,
   PR_2026_CHIK_SINAIS_NOTIFICADOS,
   PR_2026_DENGUE_SINAIS_Confirmados,
   PR_2026_DENGUE_SINAIS_Notificados,
   PR_2026_ZIKA_CHIK_Notificados,
   PR_DENGUE_2026_GRAF_SINAIS,
   RS22_2026_GRAF_CHK_Conf,
   RS22_2026_GRAF_CHK_Not,
   RS22_GRAF_2026_SINAIS,
   RS22_2026_GRAF_SORO_REAG,
   RS22_2026_GRAF_SORO_TOTAL,
   RS22_2026_GRAF_Sorotipo,
   RS22_GRAF_2026_US_DETEC,
   RS22_GRAF_2026_US_TOTAL,
   RS_CE_Confirmados_JARDIM,
   RS_CE_Confirmados_JARDIM_BASE,
   RS_2026_GRAF_CE_Confirmados_SEDE,
   RS_2026_GRAF_CE_Notificados_SEDE,
   RS_2026_GRAF_Histograma_Notificados,
   RS_2026_GRAF_CE_Confirmados_JARDIM,
   RS22_2026_GRAF_1,
   RS_2026_GRAF_CE_Provaveis_JARDIM,
   RS_2026_GRAF_CE_Provaveis_SAO_JOAO_DO_IVAI,
   RS_2026_GRAF_CHIK_Histograma_Confirmados,
   RS_2026_GRAF_CHIK_Histograma_Notificados,
   RS_2026_GRAF_CHIK_Histograma_Provaveis,
   RS_2026_GRAF_PIRAMIDE,
   RS_CE_Confirmados_SAO_JOAO_BASE,
   RS_CE_Confirmados_SAO_JOAO_DO_IVAI,
   RS_CE_Confirmados_SEDE_Base,
   RS_CE_Notificados_Base,
   RS_CE_Notificados_JARDIM,
   RS_CE_Notificados_JARDIM_BASE,
   RS_CE_Notificados_SAO_JOAO,
   RS_CE_Notificados_SAO_JOAO_BASE,
   RS_CE_Notificados_SEDE_Base,
   RS22_2026_GRAF_IDO_ARAPUA,
   RS22_2026_GRAF_IDO_ARIRANHA,
   RS22_2026_GRAF_IDO_CANDIDO,
   RS22_2026_GRAF_IDO_CRUZMALTINA,
   RS22_2026_GRAF_IDO_GODOY,
   RS22_2026_GRAF_IDO_IVAIPORA,
   RS22_2026_GRAF_IDO_JARDIM,
   RS22_2026_GRAF_IDO_Lidianopolis,
   RS22_2026_GRAF_IDO_LUNARDELLI,
   RS22_2026_GRAF_IDO_MANUEL_RIBAS,
   RS22_2026_GRAF_IDO_MATO_RICO,
   RS22_2026_GRAF_IDO_NOVA_TEBAS,
   RS22_2026_GRAF_IDO_RIO_BRANCO,
   RS22_2026_GRAF_IDO_ROSARIO,
   RS22_2026_GRAF_IDO_SANTA_MARIA,
   RS22_2026_GRAF_IDO_SAO_JOAO,
   PR_CHIK_2026_SINAN,
   RS_2026_SE_Confirmados,
   RS_2026_SE_Notificados,
   RS_CE_Confirmados,
   RS_CE_Confirmados_SEDE,
   RS_CE_Notificados,
   RS_CE_Notificados_SEDE,
   RS_Serie_Historica,
   AUX_VII,
   DENGON2026,
   RS22_2026_GRAF_SORO_CHIK_REAG,
   RS22_2026_GRAF_SORO_CHIK_TOTAL,
   AUX,
   RS_CE_Notificados_LUNARDELLI,
   RS22_2026_Incons_Hospitalizacao,
   RS22_2026_Incons_Investigacao,
   RS22_2026_Incons_Laboratorio,
   RS22_2026_Incons_Residencia,
   RS22_2026_INFORME_Pag_05,
   RS22_2026_INFORME_Pag_06,
   RS22_2026_INFORME_Pag_07,
   RS22_2026_INFORME_Pag_08,
   RS22_2026_INFORME_Pag_09,
   RS22_2026_INFORME_Pag_10,
   RS22_2026_INFORME_Pag_11,
   RS22_2026_SIG_ARAPUA_OVITRAMPAS,
   RS22_2026_SIG_ARIRANHA_DO_IVAI_OVITRAMPAS,
   RS22_2026_SIG_CANDIDO_OVITRAMPAS,
   RS22_2026_SIG_CRUZMALTINA_OVITRAMPAS,
   RS22_2026_SIG_GODOY_OVITRAMPAS,
   RS22_2026_SIG_IVAIPORA_Mosquitrap,
   RS22_2026_SIG_IVAIPORA_OVITRAMPAS,
   RS22_2026_SIG_JARDIM_OVITRAMPAS,
   RS22_2026_SIG_LIDIANOPOLIS_OVITRAMPAS,
   RS22_2026_SIG_LUNARDELLI_OVITRAMPAS,
   RS22_2026_SIG_MANOEL_RIBAS_OVITRAMPAS,
   RS22_2026_SIG_MATO_RICO_OVITRAMPAS,
   RS22_2026_SIG_NOVA_TEBAS_OVITRAMPAS,
   RS22_2026_SIG_RIO_BRANCO_OVITRAMPAS,
   RS22_2026_SIG_ROSARIO_OVITRAMPAS,
   RS22_2026_SIG_SANTA_MARIA_OVITRAMPAS,
   RS22_2026_SIG_SAO_JOAO_DO_IVAI_OVITRAMPAS,
   RS22_2026_SIG_22RS_OVITRAMPAS,
   RS22_2026_SINAN,
   RS22_2026_SINAN_Duplic,
   RS22_ARAPUA_2026_SINAN_10S,
   RS22_ARIRANHA_DO_IVAÍ_2026_SINAN_10S
)

