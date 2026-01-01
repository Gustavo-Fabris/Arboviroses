# Base de Dados: Bases DBF do SINAN; Planilhas de exames executados pelo LACEN e Planilhas de Monitoramento
#                Municipais (Google Drive)

rm(list = ls())

setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/")

#########################################################################################

Fonte <- "Fonte: SINAN. BASE DBF acessada em 13/11/2025"   ##### Fonte dos gráficos relacionados ao SINAN

Fonte_1 <- "Fonte: Lacen. Acesso em 24/08/2025"            ##### Fonte dos gráficos relacionados ao LACEN

Fonte_2 <- "Fonte: Planilhas de Controle Municipais. Acesso em 3-/10/2025"     ##### Fonte dos gráficos relacionados às Planilhas Municipais

#########################################################################################

SE <- as.data.frame("46")  ### Colocar a Semana Epidemiológica atual

SE <- as.numeric(SE)

##########################################################################################

RS <- 22   #####  Colocar AQUI a Regional

###########################################################################################################
###########   Definindo Períodos Epidemícos para serem excluídos do   #####################################
###########   cálculo para os canais endêmicos Regionais              #####################################
###########        COMENTAR AS LINHAS DOS PERÍODOS EPIDÊMICOS         #####################################
###########################################################################################################

# Periodos_Epidêmicos_RS <- c(#  "2009",
#   #  "2010",
#   #  "2011",
#   #  "2012",
#   #  "2013",
#   #  "2014",
#   #  "2015",
#   "2016",
#   "2017",
#   "2018",
#   "2019",
#   "2020",
#   "2021",
#   "2022",
#   "2023",
#   "2024",
#   "2025"
# )
# 
# Periodos_Epidêmicos_SEDE <- c(# "2009",
#   # "2010",
#   # "2011",
#   # "2012",
#   # "2013",
#   # "2014",
#   # "2015",
#   "2016",
#   "2017",
#   "2018",
#   "2019",
#   "2020",
#   "2021",
#   "2022",
#   "2023",
#   "2024",
#   "2025"
# )
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

DENGON2025 <- read.dbf(file = "Base_de_Dados/DBF/DENGON2025.dbf", 
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

CHIKON2025 <- read.dbf("Base_de_Dados/DBF/CHIKON2025.dbf",
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

NINDINET2025 <- read.dbf("Base_de_Dados/DBF/NINDINET2025.DBF",
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

###################################################################################################
###      Transformando coluna de semana epidemiológica de fator para numérica                   ###
###      (passando por texto, se for direto, ela transforma ###200905 em 06.                    ###
###                                                                                             ###
###                  Será usado para buscar semanas epidemiológicas                             ###                       
###################################################################################################

DENGON2025$SEM_PRI <-as.numeric(as.character(DENGON2025$SEM_PRI))   

CHIKON2025$SEM_PRI <-as.numeric(as.character(CHIKON2025$SEM_PRI))  

################################################################
#####################         2026             #################
################################################################

gs4_auth()

source("Informe_Arboviroses_2026_SINAN_Tabelas.R")

source("Informe_Arboviroses_2026_SINAN_Decodificacao.R")

RS22_2025_SINAN_DECODIFICADO$SINAN <- as.numeric(as.character(RS22_2025_SINAN_DECODIFICADO$SINAN))

sheet_write(RS22_2025_SINAN_DECODIFICADO, ss = "https://docs.google.com/spreadsheets/d/1z-cXrCe0ZRMRBG2rqW2BmG09HEa4tMmplXhHMnLbRg4/edit#gid=668044240", 
            sheet = "Registros_SINAN")

source("Informe_Arboviroses_2026_Dengue_PR.R")

source("Informe_Arboviroses_2026_Chikungunya.R")

source("Informe_Arboviroses_2026_Canais_Endemicos.R")

source("Informe_Arboviroses_2026_Graficos_Geral.R")

source("Informe_Arboviroses_2026_Mapas_Geral.R")

source("Informe_Arboviroses_2026_Laboratorio.R")

####      Buscando a planilha de índices das ovitrampas no google drive

RS22_2025_INDICES_OVITRAMPAS <- read_sheet("https://docs.google.com/spreadsheets/d/1QWxmCxl7fPiE_6wnyPLZ-B7a0yXm6NxRhFAbhGOtHeU/edit?gid=863361484#gid=863361484", 
                                           sheet = "Consolidado")

source("Informe_Arboviroses_2026_Entomologia.R")

source("Informe_Arboviroses_2026_Inconsistencias.R")

####     Buscando a planilha RS22_2025_SINAN_DECODIFICADO do google sheets com as coordenadas geográficas inseridas pelos municípios   ####

RS22_2025_SINAN_DECODIFICADO <- read_sheet("https://docs.google.com/spreadsheets/d/167vkgU2HLIRk2JKAr_dsxOvhu1luR60e6eMnpw9iIPI/edit?gid=1437725143#gid=1437725143", 
                                           sheet= "SINAN")

RS22_2025_SINAN_PROVAVEIS_DECODIFICADO <- RS22_2025_SINAN_DECODIFICADO[-which(RS22_2025_SINAN_DECODIFICADO$Classificacao_Final == "DESCARTADO"),] 

source("Informe_Arboviroses_2026_SIG.R")

#####################################################################################################################

######     Buscando planilhas com dados dos municípios no google drive

RS22_2025_REDE_OVITRAMPAS <- read_sheet("https://docs.google.com/spreadsheets/d/1cFq9tkfZYApiOdtJHM1WrQtwIMtUMKt8H2NZ9rDyduU/edit?gid=863361484#gid=863361484", 
                                        sheet = "Consolidado")

RS22_2025_RG_MUNICIPIOS <- read_sheet("https://docs.google.com/spreadsheets/d/1KQKPavrdg8ki2IK0nx_ZhbnrQlNuy3cY0kk_gpLUqDU/edit?gid=1585473376#gid=1585473376")

RS22_2025_RG_LOCALIDADES <- read_sheet("https://docs.google.com/spreadsheets/d/176wM7py-JuHl5gmQXnEzP_tohiNdCAItTiQ5VW1FUDk/edit?gid=877642872#gid=877642872")

RS22_2025_PE <- read_sheet("https://docs.google.com/spreadsheets/d/1JdRMkASZRTpDa9V7lBIXY5aJxlLoSp01PVQ_WmziyjA/edit?gid=863361484#gid=863361484", 
                           sheet = "Consolidado")

RS22_2025_ASSISTENCIA <- read_sheet("https://docs.google.com/spreadsheets/d/1Xx7t478C3knk-OZAkBc3exz0Ki7vDgFA-RPy98kmpiA/edit?gid=863361484#gid=863361484",
                                    sheet = "Consolidado")

#####      Salvando planilhas como CSV

write.csv(PR_DENGUE_2025_GERAL, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/PR_2025_DENGUE_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_CHIK_2025_GERAL, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/PR_2025_CHIKUNGUNYA_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(RS22_2025_REDE_OVITRAMPAS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_REDE_OVITRAMPAS.csv",
          row.names = FALSE)

RS22_2025_RG_MUNICIPIOS <- RS22_2025_RG_MUNICIPIOS[, -15]

write.csv(RS22_2025_RG_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_RG_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(RS22_2025_RG_LOCALIDADES, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_RG_LOCALIDADES.csv",
          row.names = FALSE)

write.csv(RS22_2025_PE, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_PE.csv",
          row.names = FALSE)

write.csv(RS22_2025_ASSISTENCIA, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_ASSISTENCIA.csv",
          row.names = FALSE)

write.csv(RS22_2025_GERAL, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_GERAL.csv",
          row.names = FALSE)

write.csv(RS22_2025_EXTRA, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_EXTRA.csv",
          row.names = FALSE)

write.csv(RS22_2025_SINAN, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAN.csv",
          row.names = FALSE)

write.csv(RS22_2025_SINAN_DECODIFICADO, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAN_DECODIFICADO.csv",
          row.names = FALSE)

write.csv(RS22_2025_SINAIS_DE_ALARME, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAIS_DE_ALARME.csv",
          row.names = FALSE)

write.csv(RS22_2025_SINAIS_Notificados, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAIS_Notificados.csv",
          row.names = FALSE)

write.csv(RS22_2025_SINAIS_Confirmados, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAIS_Confirmados.csv",
          row.names = FALSE)

write.csv(RS22_2025_SE_Confirmados, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SE_Confirmados.csv",
          row.names = FALSE)

write.csv(RS22_2025_SE_Notificados, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SE_Notificados.csv",
          row.names = FALSE)

write.csv(RS22_2025_DENGUE_GRAVE, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SE_DENGUE_GRAVE.csv",
          row.names = FALSE)

write.csv (assign(paste0("RS", RS, "_2025_SE_Provaveis"), AUX), 
           paste0("Tabulacoes_R/Arboviroses/RS", RS, "_2025_SE_Provaveis.csv"), 
           row.names = FALSE)

write.csv(RS22_2025_DOENCAS_PRE_EXISTENTES, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_DOENCAS_PRE_EXISTENTES.csv",
          row.names = FALSE)

write.csv (RS22_Serie_Historica, 
           "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_Serie_Historica.csv", 
           row.names = FALSE)

write.csv (assign(paste0("RS", RS, "_2025_SE_Provaveis_CHIK"), AUX),
           paste0("Tabulacoes_R/Arboviroses/RS", RS, "_2025_SE_Provaveis_CHIK.csv"),
           row.names = FALSE)

write.csv(RS22_2025_SINAN_DECODIFICADO,  "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAN_DECODIFICADO.csv",
          row.names = FALSE)

write.csv(RS22_2025_SINAN_PROVAVEIS_DECODIFICADO, "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAN_PROVAVEIS_DECODIFICADO.csv",
          row.names = FALSE)

#######################################################################################################################
##################################################################################################################
#####     Salvando os Gráficos e Mapas

###          Série Histórica - Pag_05

RS22_2025_INFORME_Pag_05 <- (RS22_GRAF_Serie_Historica_Not_Conf / RS22_GRAF_Serie_Historica_Sorotipo /RS22_GRAF_Serie_Historica_Hospitalizados)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_05.png",
       RS22_2025_INFORME_Pag_05,
       width = 33,
       height = 40,
       units = "cm",)

###          Canal Endêmicos Notificados/Confirmados - Pag 06

RS22_2025_INFORME_Pag_06 <- (RS_2025_GRAF_CE_Notificados / RS_2025_GRAF_CE_Provaveis / RS_2025_GRAF_CE_Confirmados)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_06.png",
       RS22_2025_INFORME_Pag_06,
       width = 33,
       height = 40,
       units = "cm",)

###            Notificados/Confirmados - Pag 07

RS22_2025_INFORME_Pag_07 <- (RS22_GRAF_2025_Not_Conf / RS22_GRAF_2025_Autoctones / RS22_GRAF_2025_Investigacao)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_07.png",
       RS22_2025_INFORME_Pag_07,
       width = 33,
       height = 40,
       units = "cm",)

###           Autóctones/Descartados - Pag 08

RS22_2025_INFORME_Pag_08 <- (RS22_GRAF_2025_Hospitalizados / RS22_GRAF_2025_Descartados / RS22_GRAF_2025_Inconclusivos)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_08.png",
       RS22_2025_INFORME_Pag_08,
       width = 33,
       height = 40,
       units = "cm",)

###            Em Investigação/Incidência - Pag 09

RS22_2025_INFORME_Pag_09 <- (RS22_GRAF_2025_Encerramento / PR_DENGUE_2025_GRAF_SINAIS / RS22_GRAF_2025_SINAIS)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_09.png",
       RS22_2025_INFORME_Pag_09,
       width = 33,
       height = 40,
       units = "cm",)

####           Incidencia - Pag 10

RS22_2025_INFORME_Pag_10 <- (RS22_GRAF_2025_Incidencia / RS22_GRAF_2025_Incidencia_Provaveis)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_10.png",
       RS22_2025_INFORME_Pag_10,
       width = 33,
       height = 40,
       units = "cm",)

####           EXTRA - Pag 11

RS22_2025_INFORME_Pag_11 <- ((PR_2025_GRAF_PIRAMIDE + RS_2025_GRAF_PIRAMIDE) / 
                               (PR_GRAF_Escolaridade + RS22_GRAF_Escolaridade)/
                               (PR_GRAF_Zona + RS22_GRAF_Zona))

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_11.png",
       RS22_2025_INFORME_Pag_11,
       width = 55,
       height = 65,
       units = "cm",)

####          Canais Endêmicos - Pag 12

RS_2025_INFORME_CE_SEDE <- (RS_2025_GRAF_CE_Notificados_SEDE / RS_2025_GRAF_CE_Provaveis_SEDE / RS_2025_GRAF_CE_Confirmados_SEDE)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_12.png",
       RS_2025_INFORME_CE_SEDE,
       width = 33,
       height = 40,
       units = "cm",)

####         Canais Endêmicos JARDIM - Pag 13

RS_2025_CE_JARDIM <- (RS_2025_GRAF_CE_Notificados_JARDIM / RS_2025_GRAF_CE_Provaveis_JARDIM / RS_2025_GRAF_CE_Confirmados_JARDIM)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_13.png",
       RS_2025_CE_JARDIM,
       width = 33,
       height = 40,
       units = "cm",)

####        Canais Endêmicos SAO JOAO

RS_2025_CE_SAO_JOAO <- (RS_2025_GRAF_CE_Notificados_SAO_JOAO_DO_IVAI / RS_2025_GRAF_CE_Provaveis_SAO_JOAO_DO_IVAI / RS_2025_GRAF_CE_Confirmados_SAO_JOAO_DO_IVAI)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_14.png",
       RS_2025_CE_SAO_JOAO,
       width = 33,
       height = 40,
       units = "cm",)

###        Histogramas Notificados - Pag 15

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_15.png",
       RS_2025_GRAF_Histograma_Notificados,
       width = 36,
       height = 46,
       units = "cm",)

###         Histogramas Confirmados - PAG 16

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_16.png",
       RS_2025_GRAF_Histograma_Confirmados,
       width = 36,
       height = 46,
       units = "cm",)

###       Histogramas Prováveis - PAG 17

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_17.png",
       RS_2025_GRAF_Histograma_Provaveis,
       width = 36,
       height = 46,
       units = "cm",)

###       Sorotipo - Pag 18

RS22_2025_GRAF_1 <- (PR_2025_GRAF_SOROTIPO_PR / RS22_2025_GRAF_Sorotipo)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_18.png",
       RS22_2025_GRAF_1,
       width = 36,
       height = 46,
       units = "cm",)

RS22_2025_GRAF_1 <- (RS22_GRAF_2025_US_TOTAL / RS22_GRAF_2025_US_DETEC)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_19.png",
       RS22_2025_GRAF_1,
       width = 36,
       height = 46,
       units = "cm",)

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_SORO_TOTAL / RS22_2025_GRAF_SORO_REAG)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_20.png",
       RS22_2025_GRAF_1,
       width = 36,
       height = 46,
       units = "cm",)

#####     Exames Municípios  

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_21.png",
       RS22_GRAF_LACEN_MUNIC,
       width = 36,
       height = 23,
       units = "cm",)

#####       Exames Chikungunya

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_SORO_CHIK_TOTAL / RS22_2025_GRAF_SORO_CHIK_REAG)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_22.png",
       RS22_2025_GRAF_1,
       width = 36,
       height = 46,
       units = "cm",)

#####       Exames Chikungunya Municípios

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_23.png",
       RS22_GRAF_LACEN_MUNIC_CHIK,
       width = 36,
       height = 23,
       units = "cm",)

#####       Mapa Incidência PR

RS22_2025_GRAF_1 <- (PR_2025_GRAF_INCIDENCIA_PR / PR_2025_GRAF_INCIDENCIA_PROV_PR)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_24.png",
       RS22_2025_GRAF_1,
       width = 36,
       height = 46,
       units = "cm",)

####      Mapa Chikungunya

RS22_2025_GRAF_1 <- (PR_2025_GRAF_CHIK_Notificados / PR_2025_GRAF_CHIK_Incidência)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_25.png",
       RS22_2025_GRAF_1,
       width = 36,
       height = 46,
       units = "cm",)

#####     Sinais Chikungunya PR

RS22_2025_GRAF_1 <- PR_2025_GRAF_SINAIS_CHIK 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_26.png",
       RS22_2025_GRAF_1,
       width = 36,
       height = 23,
       units = "cm",)

####      Chikungunya Regional

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_CHK_Not + RS22_2025_GRAF_CHK_Conf) 

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_27.png",
       RS22_2025_GRAF_1,
       width = 36,
       height = 23,
       units = "cm",)

####      10 S Chik

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_28.png",
       RS_2025_GRAF_CHIK_Histograma_Notificados,
       width = 36,
       height = 46,
       units = "cm",)

####    10 S Chik COnfirmados

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_29.png",
       RRS_2025_GRAF_CHIK_Histograma_Confirmados,
       width = 36,
       height = 46,
       units = "cm",)

####    10 S Chik Provaveis

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_30.png",
       RS_2025_GRAF_CHIK_Histograma_Provaveis,
       width = 36,
       height = 46,
       units = "cm",)

#### Zika Virus

RS22_2025_GRAF_1 <- (PR_2025_ZIKA_CHIK_Notificados / PR_2025_GRAF_ZIKA_Incidência)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_31.png",
       RS22_2025_GRAF_1,
       width = 36,
       height = 46,
       units = "cm",)

####    Entomologia

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_IPO_ARAPUA / RS22_2025_GRAF_IDO_ARAPUA / 
                       RS22_2025_GRAF_IPO_ARIRANHA / RS22_2025_GRAF_IDO_ARIRANHA  /
                       RS22_2025_GRAF_IPO_CANDIDO / RS22_2025_GRAF_IDO_CANDIDO)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_32.png",
       RS22_2025_GRAF_1,
       width = 46,
       height = 56,
       units = "cm",)

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_IPO_CRUZMALTINA / RS22_2025_GRAF_IDO_CRUZMALTINA / 
                       RS22_2025_GRAF_IPO_GODOY / RS22_2025_GRAF_IDO_GODOY / 
                       RS22_2025_GRAF_IPO_IVAIPORA / RS22_2025_GRAF_IDO_IVAIPORA )

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_33.png",
       RS22_2025_GRAF_1,
       width = 46,
       height = 56,
       units = "cm",)

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_IPO_JARDIM / RS22_2025_GRAF_IDO_JARDIM / 
                       RS22_2025_GRAF_IPO_Lidianopolis / RS22_2025_GRAF_IDO_Lidianopolis / 
                       RS22_2025_GRAF_IPO_LUNARDELLI / RS22_2025_GRAF_IDO_LUNARDELLI )

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_34.png",
       RS22_2025_GRAF_1,
       width = 46,
       height = 56,
       units = "cm",)

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_IPO_MANUEL_RIBAS / RS22_2025_GRAF_IDO_MANUEL_RIBAS / 
                       RS22_2025_GRAF_IPO_MATO_RICO / RS22_2025_GRAF_IDO_MATO_RICO / 
                       RS22_2025_GRAF_IPO_NOVA_TEBAS / RS22_2025_GRAF_IDO_NOVA_TEBAS )

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_35.png",
       RS22_2025_GRAF_1,
       width = 46,
       height = 56,
       units = "cm",)

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_IPO_RIO_BRANCO / RS22_2025_GRAF_IDO_RIO_BRANCO / 
                       RS22_2025_GRAF_IPO_ROSARIO / RS22_2025_GRAF_IDO_ROSARIO / 
                       RS22_2025_GRAF_IPO_SANTA_MARIA / RS22_2025_GRAF_IDO_SANTA_MARIA )

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_36.png",
       RS22_2025_GRAF_1,
       width = 46,
       height = 56,
       units = "cm",)

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_IPO_SAO_JOAO / RS22_2025_GRAF_IDO_SAO_JOAO  )

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_32.png",
       RS22_2025_GRAF_1,
       width = 46,
       height = 14,
       units = "cm",)

#####    Removendo Objetos desnecessários

rm(RS22_GRAF_Serie_Historica_Not_Conf,
   RS22_GRAF_Serie_Historica_Sorotipo, 
   RS22_GRAF_Serie_Historica_Hospitalizados,
   RS_2025_GRAF_CE_Notificados,
   RS_2025_GRAF_CE_Provaveis,
   RS_2025_GRAF_CE_Confirmados,
   RS22_GRAF_2025_Incidencia,
   RS22_GRAF_2025_Not_Conf,
   RS22_GRAF_2025_Autoctones,
   RS22_GRAF_2025_Investigacao,
   RS22_GRAF_2025_Hospitalizados,
   RS22_GRAF_2025_Descartados,
   RS22_GRAF_2025_Inconclusivos,
   AUX,
   AUX_GRAF,
   AUX_HIST_CHIK_CONF_LIST,
   AUX_HIST_CHIK_NOT_LIST,
   AUX_HIST_CHIK_PROV_LIST,
   NINDINET2025,
   PR_2025_GRAF_PIRAMIDE,
   PR_GRAF_Escolaridade,
   PR_GRAF_Zona,
   RS_2025_GRAF_CE_Provaveis_SEDE,
   RS_2025_GRAF_CE_Confirmados_SAO_JOAO_DO_IVAI,
   RS_2025_GRAF_CE_Notificados_JARDIM,
   RS_2025_GRAF_CE_Notificados_SAO_JOAO_DO_IVAI,
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
   CHIKON2025,
   SINAN_DENGUE_RS,
   SINAN_CHIK_RS,
   RS22_GRAF_2025_Encerramento,
   RS22_GRAF_LACEN_MUNIC,
   RS22_2025_INFORME_Pag_05,
   RS22_2025_INFORME_Pag_06,
   RS22_2025_INFORME_Pag_07,
   RS22_2025_INFORME_Pag_08,
   Fonte,
   Fonte_1,
   Fonte_2,
   i,
   O,
   j,
   N
   ID_REG,
   nrow,
   Periodos_Epidêmicos_RS,
   Periodos_Epidêmicos_SEDE,
   RS,
   SE,
   Theme_Hist,
   RS_2025_GRAF_Histograma_Confirmados_01,
   RS_2025_GRAF_Histograma_Confirmados_02,
   RS_2025_GRAF_Histograma_Notificados_01,
   RS_2025_GRAF_Histograma_Notificados_02,
   RS_2025_GRAF_Histograma_Provaveis_01,
   RS_2025_GRAF_Histograma_Provaveis_02,
   RS_2025_GRAF_CE_Confirmados_SEDE,
   RS_2025_GRAF_CE_Notificados_SEDE,
   RS_2025_CE_SEDE,
   PR_2025_GRAF_CHIK_Incidência,
   PR_2025_GRAF_CHIK_Notificados,
   PR_2025_GRAF_INCIDENCIA_PR,
   PR_2025_GRAF_INCIDENCIA_PROV_PR,
   PR_2025_GRAF_SINAIS_CHIK,
   PR_2025_GRAF_SOROTIPO_PR,
   PR_2025_GRAF_ZIKA_Incidência,
   PR_2025_CHIK_SINAIS_Confirmados,
   PR_2025_CHIK_SINAIS_NOTIFICADOS,
   PR_2025_DENGUE_SINAIS_Confirmados,
   PR_2025_DENGUE_SINAIS_Notificados,
   PR_2025_ZIKA_CHIK_Notificados,
   PR_DENGUE_2025_GRAF_SINAIS,
   RS22_2025_GRAF_1,
   RS22_2025_GRAF_CHK_Conf,
   RS22_2025_GRAF_CHK_Not,
   RS22_GRAF_2025_SINAIS,
   RS22_2025_GRAF_SORO_REAG,
   RS22_2025_GRAF_SORO_TOTAL,
   RS22_2025_GRAF_Sorotipo,
   RS22_GRAF_2025_US_DETEC,
   RS22_GRAF_2025_US_TOTAL,
   RS_2025_GRAF_CE_Provaveis_SEDE,
   RS_2025_GRAF_CE_Confirmados_SEDE,
   RS_2025_GRAF_CE_Notificados_SEDE,
   RS_2025_GRAF_Histograma_Provaveis_02,
   RS_2025_GRAF_Histograma_Provaveis_01,
   RS_2025_GRAF_Histograma_Confirmados_02,
   RS_2025_GRAF_Histograma_Confirmados_01,
   RS_2025_GRAF_Histograma_Notificados_02,
   RS_2025_GRAF_Histograma_Notificados_01,
   RS_2025_GRAF_CE_Confirmados_JARDIM,
   RS_2025_GRAF_CE_Provaveis_JARDIM,
   RS_2025_GRAF_CE_Provaveis_SAO_JOAO_DO_IVAI,
   RS_2025_GRAF_CHIK_Histograma_Confirmados_01,
   RS_2025_GRAF_CHIK_Histograma_Confirmados_02,
   RS_2025_GRAF_CHIK_Histograma_Notificados_01,
   RS_2025_GRAF_CHIK_Histograma_Notificados_02,
   RS_2025_GRAF_CHIK_Histograma_Provaveis_01,
   RS_2025_GRAF_CHIK_Histograma_Provaveis_02,
   RS_2025_GRAF_PIRAMIDE,
   PR_CHIK_2025_SINAN,
   RS_2025_SE_Confirmados,
   RS_2025_SE_Notificados,
   RS_CE_Confirmados,
   RS_CE_Confirmados_SEDE,
   RS_CE_Notificados,
   RS_CE_Notificados_SEDE,
   RS_Serie_Historica,
   AUX_VII,
   DENGON2025
)

