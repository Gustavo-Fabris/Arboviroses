rm(list =ls())
###############################################################################################################
###############################################################################################################
##                                                                                                           ##
##    Script elaborado em ambiente LINUX. Para replicação deve ser ajustado os caminhos dos diretórios       ##
##    ################################################################################################       ##
##                                                                                                           ##
##    Até o ponto em que é necessario autenticação de acesso à internet todos os passos abaixo são           ##
##    reprodutíveis em qualquer computador desde que os itens abaixo sejam executados corretamente.          ##
##    #############################################################################################          ## 
##                                                                                                           ##
##    /home/gustavo/Área de trabalho/Análise_de_Dados/Arboviroses_Geral deve ser previamente rodado          ##
##    para que o script abaixo funcione.                                                                     ##
##    ############################################################################################           ##
##                                                                                                           ##
###############################################################################################################
###############################################################################################################

#####      Definindo diretório de trabalho, caso tenha que trabalhar em Windows, acertar o diretório       ####

setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/")

###############################################################################################################
###############################################################################################################
##                                                                                                           ##
##               OS PASSOS ABAIXO (ATÉ LINHA 80) DEVEM SER SEGUIDOS PARA FUNCIONAMENTO!!!                    ##
##    ################################################################################################       ##
##                                                                                                           ##
##            Lembrar que OBRIGATORIAMENTE deve ser baixados as bases DBF de 2009 até 2025                   ##
##            As bases DBF devem ser salvas no formato DENGON2009, DENGON2012... até DENGON2025              ##
##            A base DENGON2025 deve ser baixada diariamente e salva no local correto para que               ## 
##            o sistema esteja sempre atualizado!!!                                                          ##
##            Os dados do LACEN devem ser baixados da GAL                                                    ##
##            Estes arquivos devem ser alocados no diretório abaixo:                                         ##
##            /home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/DBF ou /Base_de_Dados/LACEN      ##
##    ############################################################################################           ##
##                                                                                                           ##
###############################################################################################################
###############################################################################################################

Fonte <- "Fonte: SINAN. BASE DBF acessada em 13/11/2025"   ##### Fonte dos gráficos relacionados ao SINAN

Fonte_1 <- "Fonte: Lacen. Acesso em 24/08/2025"            ##### Fonte dos gráficos relacionados ao LACEN

Fonte_2 <- "Fonte: Planilhas de Controle Municipais. Acesso em 3-/10/2025"     ##### Fonte dos gráficos relacionados às Planilhas Municipais

SE <- as.data.frame("46")  ### Colocar a Semana Epidemiológica atual

SE <- as.numeric(SE)

##########################################################################################

RS <- 22   #####  Deve-se colocar AQUI a Regional

###########################################################################################################
###########   Definindo Períodos Epidemícos para serem excluídos do   #####################################
###########   cálculo para os canais endêmicos Regionais              #####################################
###########        COMENTAR AS LINHAS DOS PERÍODOS EPIDÊMICOS         #####################################
###########################################################################################################

Periodos_Epidêmicos_RS <- c(#  "2009",
  #  "2010",
  #  "2011",
  #  "2012",
  #  "2013",
  #  "2014",
  #  "2015",
  "2016",
  "2017",
  "2018",
  "2019",
  "2020",
  "2021",
  "2022",
  "2023",
  "2024",
  "2025"
)

Periodos_Epidêmicos_SEDE <- c(# "2009",
  # "2010",
  # "2011",
  # "2012",
  # "2013",
  # "2014",
  # "2015",
  "2016",
  "2017",
  "2018",
  "2019",
  "2020",
  "2021",
  "2022",
  "2023",
  "2024",
  "2025"
)
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
library(ggplot2)
library(tidyr)
#library(gt)

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

######################################################


###################################################################################################
###      Transformando coluna de semana epidemiológica de fator para numérica                   ###
###      (passando por texto, se for direto, ela transforma ###200905 em 06.                    ###
###                                                                                             ###
###                  Será usado para buscar semanas epidemiológicas                             ###                       
###################################################################################################

###
DENGON2025$SEM_PRI <-as.numeric(as.character(DENGON2025$SEM_PRI))   ###############################
###

###
CHIKON2025$SEM_PRI <-as.numeric(as.character(CHIKON2025$SEM_PRI))   ###############################

################################################################
#####################         2026             #################
################################################################

source("/home/gustavo/Área de trabalho/Análise_de_Dados/Informe_Arboviroses_2026_SINAN_Tabelas.R")

source("/home/gustavo/Área de trabalho/Análise_de_Dados/Informe_Arboviroses_2026_SINAN_Decodificacao.R")

#########################################################################################################################
##################  Trabalhando os arquivos de série histórica   ########################################################
#########################################################################################################################

####      Adicionando os dados do período atual na tabela Série Histórica     ####

RS_Serie_Historica_Base[1, 18] <- sum(RS_2025_GERAL$Notificados)
RS_Serie_Historica_Base[2, 18] <- sum(RS_2025_GERAL$Dengue)
RS_Serie_Historica_Base[3, 18] <- sum(RS_2025_GERAL$D_S_A)
RS_Serie_Historica_Base[4, 18] <- sum(RS_2025_GERAL$Dengue_Grave)
RS_Serie_Historica_Base[5, 18] <- sum(RS_2025_GERAL$Hospitalizacao)
RS_Serie_Historica_Base[6, 18] <- sum(RS_2025_GERAL$Autoctones)
RS_Serie_Historica_Base[7, 18] <- sum(RS_2025_GERAL$DENV_I)
RS_Serie_Historica_Base[8, 18] <- sum(RS_2025_GERAL$DENV_II)
RS_Serie_Historica_Base[9, 18] <- sum(RS_2025_GERAL$DENV_III)
RS_Serie_Historica_Base[10, 18] <- sum(RS_2025_GERAL$DENV_IV)
RS_Serie_Historica_Base[11, 18] <- sum(RS_2025_GERAL$Obitos)

AUX <- as.data.frame(t(RS_Serie_Historica_Base))

colnames(AUX) <- AUX[1,]

AUX <- AUX[-1,]

AUX[,12] <- c("2009", "2010", "2011", "2012", "2013", 
              "2014", "2015", "2016", "2017", "2018", "2019", 
              "2020", "2021", "2022", "2023", "2024", "2025")

colnames(AUX)[12] <- "Periodo"

AUX <- AUX[,c(12, 1:11)]

rownames(AUX) <- c(1:17)

RS_Serie_Historica <- AUX

RS_Serie_Historica[,2] <- as.numeric(RS_Serie_Historica[,2])
RS_Serie_Historica[,3] <- as.numeric(RS_Serie_Historica[,3])
RS_Serie_Historica[,4] <- as.numeric(RS_Serie_Historica[,4])
RS_Serie_Historica[,5] <- as.numeric(RS_Serie_Historica[,5])
RS_Serie_Historica[,6] <- as.numeric(RS_Serie_Historica[,6])
RS_Serie_Historica[,7] <- as.numeric(RS_Serie_Historica[,7])
RS_Serie_Historica[,8] <- as.numeric(RS_Serie_Historica[,8])
RS_Serie_Historica[,9] <- as.numeric(RS_Serie_Historica[,9])
RS_Serie_Historica[,10] <- as.numeric(RS_Serie_Historica[,10])
RS_Serie_Historica[,11] <- as.numeric(RS_Serie_Historica[,11])
RS_Serie_Historica[,12] <- as.numeric(RS_Serie_Historica[,12])

assign(paste0("RS", RS, "_Serie_Historica"), RS_Serie_Historica) 

rm(AUX, RS_Serie_Historica_Base, RS_2025_GERAL)


#######################################################
######     Histogramas Municipais   ###################
#######################################################

###       NOTIFICADOS     ########

RS_2025_SE_Notificados[nrow(RS_2025_SE_Notificados) +1, 2:ncol(RS_2025_SE_Notificados)] <- c("1",  "2", "3", 
                                                                                             "4",  "5",  "6",  
                                                                                             "7",  "8",  "9",  
                                                                                             "10",  "11",  "12",  
                                                                                             "13",  "14",  "15",  
                                                                                             "16",  "17",  "18",  
                                                                                             "19",  "20",  "21",  
                                                                                             "22",  "23",  
                                                                                             "24",  "25",  "26",  
                                                                                             "27",  "28",  "29",  
                                                                                             "30",  "31",  "32", 
                                                                                             "33",  "34",  "35",  
                                                                                             "36",  "37",  "38",  
                                                                                             "39",  "40",  "41",  
                                                                                             "42",  "43",  "244",  
                                                                                             "45",  "46",  "47",  
                                                                                             "48",  "49",  "50",  
                                                                                             "51",  "52")

AUX_GRAF <- as.data.frame(RS_2025_SE_Notificados$Município)

AUX_GRAF[, 2] <- as.data.frame(RS_2025_SE_Notificados[, which(colnames(RS_2025_SE_Notificados) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(RS_2025_SE_Notificados[, which(colnames(RS_2025_SE_Notificados) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(RS_2025_SE_Notificados[, which(colnames(RS_2025_SE_Notificados) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(RS_2025_SE_Notificados[, which(colnames(RS_2025_SE_Notificados) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(RS_2025_SE_Notificados[, which(colnames(RS_2025_SE_Notificados) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(RS_2025_SE_Notificados[, which(colnames(RS_2025_SE_Notificados) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(RS_2025_SE_Notificados[, which(colnames(RS_2025_SE_Notificados) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(RS_2025_SE_Notificados[, which(colnames(RS_2025_SE_Notificados) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(RS_2025_SE_Notificados[, which(colnames(RS_2025_SE_Notificados) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(RS_2025_SE_Notificados[, which(colnames(RS_2025_SE_Notificados) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(RS_2025_SE_Notificados)[which(colnames(RS_2025_SE_Notificados) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(RS_2025_SE_Notificados)[which(colnames(RS_2025_SE_Notificados) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(RS_2025_SE_Notificados)[which(colnames(RS_2025_SE_Notificados) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(RS_2025_SE_Notificados)[which(colnames(RS_2025_SE_Notificados) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(RS_2025_SE_Notificados)[which(colnames(RS_2025_SE_Notificados) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(RS_2025_SE_Notificados)[which(colnames(RS_2025_SE_Notificados) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(RS_2025_SE_Notificados)[which(colnames(RS_2025_SE_Notificados) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(RS_2025_SE_Notificados)[which(colnames(RS_2025_SE_Notificados) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(RS_2025_SE_Notificados)[which(colnames(RS_2025_SE_Notificados) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(RS_2025_SE_Notificados)[which(colnames(RS_2025_SE_Notificados) == SE)]

AUX_GRAF[nrow(AUX_GRAF), 1] <- "Municipios"

AUX_GRAF <- AUX_GRAF[-(nrow(AUX_GRAF)-1),]

AUX_GRAF <- AUX_GRAF[c(nrow(AUX_GRAF), 1:(nrow(AUX_GRAF) -1)), ]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF[, 2: ncol(AUX_GRAF)] <- apply(AUX_GRAF[, 2: ncol(AUX_GRAF)], 2, as.numeric)

rownames(AUX_GRAF) <- 1: nrow(AUX_GRAF)

colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")

###############  Criando uma função para o tema do gráfico   ##################

Theme_Hist <- function(){ 
  theme_minimal(base_size = 10) %+replace%  
    theme(
      axis.text.x = element_text(face = "bold"),
      panel.grid.major = element_line(color = "#C0C0C0"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#F5F5F5"),
      plot.title = element_text(face = "bold", 
                                size = 15, 
                                colour = "#556B2F")
    )
}

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

######   Criando o conjunto de gráficos dos histogramas via Lapply.  ######

AUX_HIST_NOT_LIST <- AUX_GRAF %>%
  pivot_longer(-SE, names_to = "Municipios") %>% 
  mutate(
    SE = SE,
    Municipios = gsub("_", " ", Municipios)
  ) %>%
  group_split(Municipios) %>% 
  lapply(\(dados) {
    titulo <- dados$Municipios %>% 
      unique() %>% 
      paste0(" - Notificados")
    ggplot(dados, aes(x = SE, 
                      y = value)
    ) + 
      geom_col(color = "black", 
               fill = "#8FBC8F") + 
      geom_label(aes(label = value), 
                 alpha = 0.5, 
                 vjust = 0.1) +
      labs(
        caption = Fonte, 
        x = "Semana Epidemiológica",
        y = "Número de Casos",
        title = titulo
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      Theme_Hist()
  })

RS_2025_GRAF_Histograma_Notificados_01 <- (AUX_HIST_NOT_LIST[[1]] + AUX_HIST_NOT_LIST[[2]]) / 
  (AUX_HIST_NOT_LIST[[3]] + AUX_HIST_NOT_LIST[[4]]) / 
  (AUX_HIST_NOT_LIST[[5]] + AUX_HIST_NOT_LIST[[6]]) / 
  (AUX_HIST_NOT_LIST[[7]] + AUX_HIST_NOT_LIST[[8]]) 

RS_2025_GRAF_Histograma_Notificados_02 <- (AUX_HIST_NOT_LIST[[9]] + AUX_HIST_NOT_LIST[[10]]) / 
  (AUX_HIST_NOT_LIST[[11]] + AUX_HIST_NOT_LIST[[12]]) / 
  (AUX_HIST_NOT_LIST[[13]] + AUX_HIST_NOT_LIST[[14]]) / 
  (AUX_HIST_NOT_LIST[[15]] + AUX_HIST_NOT_LIST[[16]]) 

###     Confirmados    #####

RS_2025_SE_Confirmados[nrow(RS_2025_SE_Confirmados) +1, 2:ncol(RS_2025_SE_Confirmados)] <- c("1",  "2", "3", 
                                                                                             "4",  "5",  "6",  
                                                                                             "7",  "8",  "9",  
                                                                                             "10",  "11",  "12",  
                                                                                             "13",  "14",  "15",  
                                                                                             "16",  "17",  "18",  
                                                                                             "19",  "20",  "21",  
                                                                                             "22",  "23",  
                                                                                             "24",  "25",  "26",  
                                                                                             "27",  "28",  "29",  
                                                                                             "30",  "31",  "32", 
                                                                                             "33",  "34",  "35",  
                                                                                             "36",  "37",  "38",  
                                                                                             "39",  "40",  "41",  
                                                                                             "42",  "43",  "244",  
                                                                                             "45",  "46",  "47",  
                                                                                             "48",  "49",  "50",  
                                                                                             "51",  "52")

AUX_GRAF <- as.data.frame(RS_2025_SE_Confirmados$Município)

AUX_GRAF[, 2] <- as.data.frame(RS_2025_SE_Confirmados[, which(colnames(RS_2025_SE_Confirmados) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(RS_2025_SE_Confirmados[, which(colnames(RS_2025_SE_Confirmados) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(RS_2025_SE_Confirmados[, which(colnames(RS_2025_SE_Confirmados) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(RS_2025_SE_Confirmados[, which(colnames(RS_2025_SE_Confirmados) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(RS_2025_SE_Confirmados[, which(colnames(RS_2025_SE_Confirmados) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(RS_2025_SE_Confirmados[, which(colnames(RS_2025_SE_Confirmados) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(RS_2025_SE_Confirmados[, which(colnames(RS_2025_SE_Confirmados) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(RS_2025_SE_Confirmados[, which(colnames(RS_2025_SE_Confirmados) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(RS_2025_SE_Confirmados[, which(colnames(RS_2025_SE_Confirmados) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(RS_2025_SE_Confirmados[, which(colnames(RS_2025_SE_Confirmados) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(RS_2025_SE_Confirmados)[which(colnames(RS_2025_SE_Confirmados) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(RS_2025_SE_Confirmados)[which(colnames(RS_2025_SE_Confirmados) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(RS_2025_SE_Confirmados)[which(colnames(RS_2025_SE_Confirmados) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(RS_2025_SE_Confirmados)[which(colnames(RS_2025_SE_Confirmados) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(RS_2025_SE_Confirmados)[which(colnames(RS_2025_SE_Confirmados) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(RS_2025_SE_Confirmados)[which(colnames(RS_2025_SE_Confirmados) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(RS_2025_SE_Confirmados)[which(colnames(RS_2025_SE_Confirmados) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(RS_2025_SE_Confirmados)[which(colnames(RS_2025_SE_Confirmados) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(RS_2025_SE_Confirmados)[which(colnames(RS_2025_SE_Confirmados) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(RS_2025_SE_Confirmados)[which(colnames(RS_2025_SE_Confirmados) == SE)]

AUX_GRAF[nrow(AUX_GRAF), 1] <- "Municipios"

AUX_GRAF <- AUX_GRAF[-(nrow(AUX_GRAF)-1),]

AUX_GRAF <- AUX_GRAF[c(nrow(AUX_GRAF), 1:(nrow(AUX_GRAF) -1)), ]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF[, 2: ncol(AUX_GRAF)] <- apply(AUX_GRAF[, 2: ncol(AUX_GRAF)], 2, as.numeric)

rownames(AUX_GRAF) <- 1: nrow(AUX_GRAF)

colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

######   Criando o conjunto de gráficos dos histogramas via Lapply.  ######

AUX_HIST_CONF_LIST <- AUX_GRAF %>%
  pivot_longer(-SE, names_to = "Municipios") %>% 
  mutate(
    SE = SE,
    Municipios = gsub("_", " ", Municipios)
  ) %>%
  group_split(Municipios) %>% 
  lapply(\(dados) {
    titulo <- dados$Municipios %>% 
      unique() %>% 
      paste0(" - Confirmados")
    ggplot(dados, aes(x = SE, 
                      y = value)
    ) + 
      geom_col(color = "black", 
               fill = "#DB7093") + 
      geom_label(aes(label = value), 
                 alpha = 0.5, 
                 vjust = 0.1) +
      labs(
        caption = Fonte, 
        x = "Semana Epidemiológica",
        y = "Número de Casos",
        title = titulo
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      Theme_Hist()
  })

RS_2025_GRAF_Histograma_Confirmados_01 <- (AUX_HIST_CONF_LIST[[1]] + AUX_HIST_CONF_LIST[[2]]) / 
  (AUX_HIST_CONF_LIST[[3]] + AUX_HIST_CONF_LIST[[4]]) / 
  (AUX_HIST_CONF_LIST[[5]] + AUX_HIST_CONF_LIST[[6]]) / 
  (AUX_HIST_CONF_LIST[[7]] + AUX_HIST_CONF_LIST[[8]]) 

RS_2025_GRAF_Histograma_Confirmados_02 <- (AUX_HIST_CONF_LIST[[9]] + AUX_HIST_CONF_LIST[[10]]) / 
  (AUX_HIST_CONF_LIST[[11]] + AUX_HIST_CONF_LIST[[12]]) / 
  (AUX_HIST_CONF_LIST[[13]] + AUX_HIST_CONF_LIST[[14]]) / 
  (AUX_HIST_CONF_LIST[[15]] + AUX_HIST_CONF_LIST[[16]]) 

######Histogramas

###Provaveis

RS_2025_SE_Provaveis[nrow(RS_2025_SE_Provaveis) +1, 2:ncol(RS_2025_SE_Provaveis)] <- c("1",  "2", "3", 
                                                                                       "4",  "5",  "6",  
                                                                                       "7",  "8",  "9",  
                                                                                       "10",  "11",  "12",  
                                                                                       "13",  "14",  "15",  
                                                                                       "16",  "17",  "18",  
                                                                                       "19",  "20",  "21",  
                                                                                       "22",  "23",  
                                                                                       "24",  "25",  "26",  
                                                                                       "27",  "28",  "29",  
                                                                                       "30",  "31",  "32", 
                                                                                       "33",  "34",  "35",  
                                                                                       "36",  "37",  "38",  
                                                                                       "39",  "40",  "41",  
                                                                                       "42",  "43",  "244",  
                                                                                       "45",  "46",  "47",  
                                                                                       "48",  "49",  "50",  
                                                                                       "51",  "52")


AUX_GRAF <- as.data.frame(RS_2025_SE_Provaveis$Município)

AUX_GRAF[, 2] <- as.data.frame(RS_2025_SE_Provaveis[, which(colnames(RS_2025_SE_Provaveis) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(RS_2025_SE_Provaveis[, which(colnames(RS_2025_SE_Provaveis) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(RS_2025_SE_Provaveis[, which(colnames(RS_2025_SE_Provaveis) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(RS_2025_SE_Provaveis[, which(colnames(RS_2025_SE_Provaveis) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(RS_2025_SE_Provaveis[, which(colnames(RS_2025_SE_Provaveis) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(RS_2025_SE_Provaveis[, which(colnames(RS_2025_SE_Provaveis) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(RS_2025_SE_Provaveis[, which(colnames(RS_2025_SE_Provaveis) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(RS_2025_SE_Provaveis[, which(colnames(RS_2025_SE_Provaveis) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(RS_2025_SE_Provaveis[, which(colnames(RS_2025_SE_Provaveis) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(RS_2025_SE_Provaveis[, which(colnames(RS_2025_SE_Provaveis) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(RS_2025_SE_Provaveis)[which(colnames(RS_2025_SE_Provaveis) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(RS_2025_SE_Provaveis)[which(colnames(RS_2025_SE_Provaveis) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(RS_2025_SE_Provaveis)[which(colnames(RS_2025_SE_Provaveis) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(RS_2025_SE_Provaveis)[which(colnames(RS_2025_SE_Provaveis) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(RS_2025_SE_Provaveis)[which(colnames(RS_2025_SE_Provaveis) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(RS_2025_SE_Provaveis)[which(colnames(RS_2025_SE_Provaveis) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(RS_2025_SE_Provaveis)[which(colnames(RS_2025_SE_Provaveis) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(RS_2025_SE_Provaveis)[which(colnames(RS_2025_SE_Provaveis) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(RS_2025_SE_Provaveis)[which(colnames(RS_2025_SE_Provaveis) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(RS_2025_SE_Provaveis)[which(colnames(RS_2025_SE_Provaveis) == SE)]


AUX_GRAF[nrow(AUX_GRAF), 1] <- "Municipios"

AUX_GRAF <- AUX_GRAF[-(nrow(AUX_GRAF)-1),]

AUX_GRAF <- AUX_GRAF[c(nrow(AUX_GRAF), 1:(nrow(AUX_GRAF) -1)), ]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF[, 2: ncol(AUX_GRAF)] <- apply(AUX_GRAF[, 2: ncol(AUX_GRAF)], 2, as.numeric)

rownames(AUX_GRAF) <- 1: nrow(AUX_GRAF)

colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

AUX_HIST_PROV_LIST <- AUX_GRAF %>%
  pivot_longer(-SE, names_to = "Municipios") %>% 
  mutate(
    SE = SE,
    Municipios = gsub("_", " ", Municipios)
  ) %>%
  group_split(Municipios) %>% 
  lapply(\(dados) {
    titulo <- dados$Municipios %>% 
      unique() %>% 
      paste0(" - Prováveis")
    ggplot(dados, aes(x = SE, 
                      y = value)
    ) + 
      geom_col(color = "black", 
               fill = "#F0E68C") + 
      geom_label(aes(label = value), 
                 alpha = 0.5, 
                 vjust = 0.1) +
      labs(
        caption = Fonte, 
        x = "Semana Epidemiológica",
        y = "Número de Casos",
        title = titulo
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      Theme_Hist()
  })

RS_2025_GRAF_Histograma_Provaveis_01 <- (AUX_HIST_PROV_LIST[[1]] + AUX_HIST_PROV_LIST[[2]]) / 
  (AUX_HIST_PROV_LIST[[3]] + AUX_HIST_PROV_LIST[[4]]) / 
  (AUX_HIST_PROV_LIST[[5]] + AUX_HIST_PROV_LIST[[6]]) / 
  (AUX_HIST_PROV_LIST[[7]] + AUX_HIST_PROV_LIST[[8]]) 

RS_2025_GRAF_Histograma_Provaveis_02 <- (AUX_HIST_PROV_LIST[[9]] + AUX_HIST_PROV_LIST[[10]]) / 
  (AUX_HIST_PROV_LIST[[11]] + AUX_HIST_PROV_LIST[[12]]) / 
  (AUX_HIST_PROV_LIST[[13]] + AUX_HIST_PROV_LIST[[14]]) / 
  (AUX_HIST_PROV_LIST[[15]] + AUX_HIST_PROV_LIST[[16]]) 

################################################################
################################################################
######      Gráficos para serem utilizados no Informe       ####
################################################################
################################################################

#########   Criando uma função para theme()

Theme <- function(){
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14),
        panel.grid.major = element_line(color = "#C0C0C0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.title = element_text(face = "bold",
                                  size = 24,
                                  colour = "#556B2F"),
        legend.position = "bottom")
}

####################################    Pirâmide Etária RS  ###########################################################

AUX <- c("< 01", "< 01", "01 - 04", "01 - 04", "05 - 09", "05 - 09", "10 - 14", "10 - 14", "15 - 19", "15 - 19", "20 - 24", "20 - 24", 
         "25 - 29", "25 - 29", "30 - 34", "30 - 34", "35 - 39", "35 - 39", "40 - 44", "40 - 44", "45 - 49", "45 - 49", "50 - 54", "50 - 54",
         "55 - 59", "55 - 59", "60 - 64", "60 - 64", "65 - 69", "65 - 69", "70 - 74",  "70 - 74", "75 - 79", "75 - 79", "80 - 84", 
         "80 - 84", "> 84", "> 84")

AUX <- as.data.frame(AUX)

AUX[, 2] <- c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", 
              "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F")

AUX[1, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N <= 4000 & CS_SEXO == "M") %>%
  count()

AUX[2, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N <= 4000 & CS_SEXO == "F") %>%
  count()

AUX[3, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4000 & NU_IDADE_N <= 4004 & CS_SEXO == "M") %>%
  count()

AUX[4, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4000 & NU_IDADE_N <= 4004 & CS_SEXO == "F") %>%
  count()

AUX[5, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4004 & NU_IDADE_N <= 4009 & CS_SEXO == "M") %>%
  count()

AUX[6, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4004 & NU_IDADE_N <= 4009 & CS_SEXO == "F") %>%
  count()

AUX[7, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4009 & NU_IDADE_N <= 4014 & CS_SEXO == "M") %>%
  count()

AUX[8, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4009 & NU_IDADE_N <= 4014 & CS_SEXO == "F") %>%
  count()

AUX[9, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4014 & NU_IDADE_N <= 4019 & CS_SEXO == "M") %>%
  count()

AUX[10, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4014 & NU_IDADE_N <= 4019 & CS_SEXO == "F") %>%
  count()

AUX[11, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4019 & NU_IDADE_N <= 4024 & CS_SEXO == "M") %>%
  count()

AUX[12, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4019 & NU_IDADE_N <= 4024 & CS_SEXO == "F") %>%
  count()

AUX[13, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4024 & NU_IDADE_N <= 4029 & CS_SEXO == "M") %>%
  count()

AUX[14, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4024 & NU_IDADE_N <= 4029 & CS_SEXO == "F") %>%
  count()

AUX[15, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4029 & NU_IDADE_N <= 4034 & CS_SEXO == "M") %>%
  count()

AUX[16, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4029 & NU_IDADE_N <= 4034 & CS_SEXO == "F") %>%
  count()

AUX[17, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4034 & NU_IDADE_N <= 4039 & CS_SEXO == "M") %>%
  count()

AUX[18, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4034 & NU_IDADE_N <= 4039 & CS_SEXO == "F") %>%
  count()

AUX[19, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4039 & NU_IDADE_N <= 4044 & CS_SEXO == "M") %>%
  count()

AUX[20, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4039 & NU_IDADE_N <= 4044 & CS_SEXO == "F") %>%
  count()

AUX[21, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4044 & NU_IDADE_N <= 4049 & CS_SEXO == "M") %>%
  count()

AUX[22, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4044 & NU_IDADE_N <= 4049 & CS_SEXO == "F") %>%
  count()

AUX[23, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4049 & NU_IDADE_N <= 4054 & CS_SEXO == "M") %>%
  count()

AUX[24, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4049 & NU_IDADE_N <= 4054 & CS_SEXO == "F") %>%
  count()

AUX[25, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4054 & NU_IDADE_N <= 4059 & CS_SEXO == "M") %>%
  count()

AUX[26, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4054 & NU_IDADE_N <= 4059 & CS_SEXO == "F") %>%
  count()

AUX[27, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4059 & NU_IDADE_N <= 4064 & CS_SEXO == "M") %>%
  count()

AUX[28, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4059 & NU_IDADE_N <= 4064 & CS_SEXO == "F") %>%
  count()

AUX[29, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4064 & NU_IDADE_N <= 4069 & CS_SEXO == "M") %>%
  count()

AUX[30, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4064 & NU_IDADE_N <= 4069 & CS_SEXO == "F") %>%
  count()

AUX[31, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4069 & NU_IDADE_N <= 4074 & CS_SEXO == "M") %>%
  count()

AUX[32, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4069 & NU_IDADE_N <= 4074 & CS_SEXO == "F") %>%
  count()

AUX[33, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4074 & NU_IDADE_N <= 4079 & CS_SEXO == "M") %>%
  count()

AUX[34, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4074 & NU_IDADE_N <= 4079 & CS_SEXO == "F") %>%
  count()

AUX[35, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4079 & NU_IDADE_N <= 4084 & CS_SEXO == "M") %>%
  count()

AUX[36, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4079 & NU_IDADE_N <= 4084 & CS_SEXO == "F") %>%
  count()

AUX[37, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4084 & CS_SEXO == "M") %>%
  count()

AUX[38, 3] <- RS22_2025_SINAN[, 15:16] %>%
  filter(NU_IDADE_N > 4084 & CS_SEXO == "F") %>%
  count()

colnames(AUX) <- c("Grupo_Idade", "Sexo", "Populacao")

AUX <- AUX %>%
  mutate(Pop = case_when(Sexo == "M" ~ Populacao * -1,
                         Sexo == "F" ~ Populacao ))

AUX <- AUX %>% 
  mutate(Sexo_Legenda = case_when(Sexo == "M" ~ "Masculino",
                                  Sexo == "F" ~ "Feminino"))

AUX <- AUX %>%
  mutate(grupo_Idade_FACT = factor(Grupo_Idade, 
                                   levels = c(
                                     "< 01",
                                     "01 - 04",
                                     "05 - 09",
                                     "10 - 14",
                                     "15 - 19",
                                     "20 - 24",
                                     "25 - 29",
                                     "30 - 34",
                                     "35 - 39",
                                     "40 - 44",
                                     "45 - 49",
                                     "50 - 54",
                                     "55 - 59",
                                     "60 - 64",
                                     "65 - 69",
                                     "70 - 74",
                                     "75 - 79",
                                     "80 - 84",
                                     "> 84")
  )
  )

RS_2025_GRAF_PIRAMIDE <- ggplot(AUX, 
                                aes(x = Pop,
                                    y = grupo_Idade_FACT, 
                                    fill = Sexo_Legenda)) +
  geom_col(color = "black",
           linewidth = 0.8) +
  labs(title = "Pirâmide Etária - 22ª RS (2025)",
       y = "Faixa Etária",
       x = "Nº de Notificações",
       fill = "Sexo",
       caption = Fonte) +
  scale_x_continuous(labels = abs) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

####################################    Pirâmide Etária ESTADO  ###########################################################

AUX <- c("< 01", "< 01", "01 - 04", "01 - 04", "05 - 09", "05 - 09", "10 - 14", "10 - 14", "15 - 19", "15 - 19", "20 - 24", "20 - 24", 
         "25 - 29", "25 - 29", "30 - 34", "30 - 34", "35 - 39", "35 - 39", "40 - 44", "40 - 44", "45 - 49", "45 - 49", "50 - 54", "50 - 54",
         "55 - 59", "55 - 59", "60 - 64", "60 - 64", "65 - 69", "65 - 69", "70 - 74",  "70 - 74", "75 - 79", "75 - 79", "80 - 84", 
         "80 - 84", "> 84", "> 84")

AUX <- as.data.frame(AUX)

AUX[, 2] <- c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", 
              "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "F")

AUX[1, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N <= 4000 & CS_SEXO == "M") %>%
  count()

AUX[2, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N <= 4000 & CS_SEXO == "F") %>%
  count()

AUX[3, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4000 & NU_IDADE_N <= 4004 & CS_SEXO == "M") %>%
  count()

AUX[4, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4000 & NU_IDADE_N <= 4004 & CS_SEXO == "F") %>%
  count()

AUX[5, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4004 & NU_IDADE_N <= 4009 & CS_SEXO == "M") %>%
  count()

AUX[6, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4004 & NU_IDADE_N <= 4009 & CS_SEXO == "F") %>%
  count()

AUX[7, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4009 & NU_IDADE_N <= 4014 & CS_SEXO == "M") %>%
  count()

AUX[8, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4009 & NU_IDADE_N <= 4014 & CS_SEXO == "F") %>%
  count()

AUX[9, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4014 & NU_IDADE_N <= 4019 & CS_SEXO == "M") %>%
  count()

AUX[10, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4014 & NU_IDADE_N <= 4019 & CS_SEXO == "F") %>%
  count()

AUX[11, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4019 & NU_IDADE_N <= 4024 & CS_SEXO == "M") %>%
  count()

AUX[12, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4019 & NU_IDADE_N <= 4024 & CS_SEXO == "F") %>%
  count()

AUX[13, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4024 & NU_IDADE_N <= 4029 & CS_SEXO == "M") %>%
  count()

AUX[14, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4024 & NU_IDADE_N <= 4029 & CS_SEXO == "F") %>%
  count()

AUX[15, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4029 & NU_IDADE_N <= 4034 & CS_SEXO == "M") %>%
  count()

AUX[16, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4029 & NU_IDADE_N <= 4034 & CS_SEXO == "F") %>%
  count()

AUX[17, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4034 & NU_IDADE_N <= 4039 & CS_SEXO == "M") %>%
  count()

AUX[18, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4034 & NU_IDADE_N <= 4039 & CS_SEXO == "F") %>%
  count()

AUX[19, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4039 & NU_IDADE_N <= 4044 & CS_SEXO == "M") %>%
  count()

AUX[20, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4039 & NU_IDADE_N <= 4044 & CS_SEXO == "F") %>%
  count()

AUX[21, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4044 & NU_IDADE_N <= 4049 & CS_SEXO == "M") %>%
  count()

AUX[22, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4044 & NU_IDADE_N <= 4049 & CS_SEXO == "F") %>%
  count()

AUX[23, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4049 & NU_IDADE_N <= 4054 & CS_SEXO == "M") %>%
  count()

AUX[24, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4049 & NU_IDADE_N <= 4054 & CS_SEXO == "F") %>%
  count()

AUX[25, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4054 & NU_IDADE_N <= 4059 & CS_SEXO == "M") %>%
  count()

AUX[26, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4054 & NU_IDADE_N <= 4059 & CS_SEXO == "F") %>%
  count()

AUX[27, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4059 & NU_IDADE_N <= 4064 & CS_SEXO == "M") %>%
  count()

AUX[28, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4059 & NU_IDADE_N <= 4064 & CS_SEXO == "F") %>%
  count()

AUX[29, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4064 & NU_IDADE_N <= 4069 & CS_SEXO == "M") %>%
  count()

AUX[30, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4064 & NU_IDADE_N <= 4069 & CS_SEXO == "F") %>%
  count()

AUX[31, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4069 & NU_IDADE_N <= 4074 & CS_SEXO == "M") %>%
  count()

AUX[32, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4069 & NU_IDADE_N <= 4074 & CS_SEXO == "F") %>%
  count()

AUX[33, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4074 & NU_IDADE_N <= 4079 & CS_SEXO == "M") %>%
  count()

AUX[34, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4074 & NU_IDADE_N <= 4079 & CS_SEXO == "F") %>%
  count()

AUX[35, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4079 & NU_IDADE_N <= 4084 & CS_SEXO == "M") %>%
  count()

AUX[36, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4079 & NU_IDADE_N <= 4084 & CS_SEXO == "F") %>%
  count()

AUX[37, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4084 & CS_SEXO == "M") %>%
  count()

AUX[38, 3] <- DENGON2025[, 15:16] %>%
  filter(NU_IDADE_N > 4084 & CS_SEXO == "F") %>%
  count()

colnames(AUX) <- c("Grupo_Idade", "Sexo", "Populacao")

AUX <- AUX %>%
  mutate(Pop = case_when(Sexo == "M" ~ Populacao * -1,
                         Sexo == "F" ~ Populacao ))

AUX <- AUX %>% 
  mutate(Sexo_Legenda = case_when(Sexo == "M" ~ "Masculino",
                                  Sexo == "F" ~ "Feminino"))

AUX <- AUX %>%
  mutate(grupo_Idade_FACT = factor(Grupo_Idade, 
                                   levels = c(
                                     "< 01",
                                     "01 - 04",
                                     "05 - 09",
                                     "10 - 14",
                                     "15 - 19",
                                     "20 - 24",
                                     "25 - 29",
                                     "30 - 34",
                                     "35 - 39",
                                     "40 - 44",
                                     "45 - 49",
                                     "50 - 54",
                                     "55 - 59",
                                     "60 - 64",
                                     "65 - 69",
                                     "70 - 74",
                                     "75 - 79",
                                     "80 - 84",
                                     "> 84")
  )
  )

PR_2025_GRAF_PIRAMIDE <- ggplot(AUX, 
                                aes(x = Pop,
                                    y = grupo_Idade_FACT, 
                                    fill = Sexo_Legenda)) +
  geom_col(color = "black",
           linewidth = 0.8) +
  labs(title = "Pirâmide Etária - Paraná (2025)",
       y = "Faixa Etária",
       x = "Nº de Notificações",
       fill = "Sexo",
       caption = Fonte) +
  scale_x_continuous(labels = abs) +
  Theme() +
  theme(legend.position = "bottom",  
        legend.title = element_text(face = "bold",
                                    size = 14), 
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 20)    
  )

##########   Escolaridade   #####

AUX_GRAF <- tibble(Rotulos = colnames(RS22_2025_EXTRA[14:21]),
                   Ordem = as.factor(1:(21-13)),
                   Notificados = apply(RS22_2025_EXTRA[, 14:21], 2, sum),
                   Confirmados = apply(RS22_2025_EXTRA_Confirmados[, 14:21], 2, sum) )

AUX_GRAF[, 5] <- AUX_GRAF[, 3]/ apply(AUX_GRAF[, 3], 2, sum) *100

AUX_GRAF[, 6] <- AUX_GRAF[, 4]/ apply(AUX_GRAF[, 4], 2, sum) *100

colnames(AUX_GRAF)[5:6] <- c("Not", "Conf")

AUX_GRAF$Not <- as.numeric(format(round(AUX_GRAF$Not , 2))
)

AUX_GRAF$Conf <- as.numeric(format(round(AUX_GRAF$Conf , 2))
)

AUX_GRAF <- AUX_GRAF[, c(1, 2, 5:6)]

AUX_GRAF[,1] <- c("Analfabeto", "Fundamental 
Incompleto", "Fundamental", "Médio 
Incompleto", 
                  "Médio", "Superior 
Incompleto", "Superior", "Ignorado")

colnames(AUX_GRAF)[3:4] <- c("Notificados", "Confirmados")

RS22_GRAF_Escolaridade <- ggplot (AUX_GRAF, 
                                  aes(x = Ordem)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Percentagem de Casos",
       title = "Distribuição das Notificações por Escolaridade 22ª RS 
(2025)") +
  geom_bar(
    aes( y = Notificados, 
         fill = "Notificados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.1) + 
  scale_fill_manual(name = "", 
                    values = c("Notificados" = "#C4BF7A", 
                               "Confirmados" = "#C4A37A")) +
  geom_bar(
    aes( y = Confirmados, 
         fill = "Confirmados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = Confirmados,
                 label = Confirmados),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:(21-13)),
                   labels = AUX_GRAF$Rotulos) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 55),
        plot.title = element_text(face = "bold",
                                  size = 16,
                                  colour = "#556B2F"))

#########   ZONA de ocorrência  #####

AUX_GRAF <- tibble(Rotulos = colnames(RS22_2025_EXTRA[10:11]),
                   Ordem = as.factor(1:2),
                   Notificados = apply(RS22_2025_EXTRA[, 10:11], 2, sum),
                   Confirmados = apply(RS22_2025_EXTRA_Confirmados[, 10:11], 2, sum) )

AUX_GRAF[,1] <- c("Urbana", "Rural")

AUX_GRAF[, 5] <- AUX_GRAF[, 3]/ apply(AUX_GRAF[, 3], 2, sum) *100

AUX_GRAF[, 6] <- AUX_GRAF[, 4]/ apply(AUX_GRAF[, 4], 2, sum) *100

colnames(AUX_GRAF)[5:6] <- c("Not", "Conf")

AUX_GRAF$Not <- as.numeric(format(round(AUX_GRAF$Not , 2))
)

AUX_GRAF$Conf <- as.numeric(format(round(AUX_GRAF$Conf , 2))
)

AUX_GRAF <- AUX_GRAF[, c(1, 2, 5:6)]

AUX_GRAF[,1] <- c("Urbana", "Rural")

colnames(AUX_GRAF)[3:4] <- c("Notificados", "Confirmados")

RS22_GRAF_Zona <- ggplot (AUX_GRAF, 
                          aes(x = Ordem)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Percentual de Casos",
       title = paste0("Distribuição das Notificações por Zona de Ocorrência ", RS, "ªRS 
(2025)")) +
  geom_bar(
    aes( y = Notificados, fill = "Notificados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.1) + 
  scale_fill_manual(name = "", values = c("Notificados" = "#614352", "Confirmados" = "#436155")) +
  geom_bar(
    aes( y = Confirmados, fill = "Confirmados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = Confirmados,
                 label = Confirmados),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:2),
                   labels = AUX_GRAF$Rotulos) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 0),
        plot.title = element_text(face = "bold",
                                  size = 16,
                                  colour = "#556B2F"))

#####   Séries Históricas   ####

AUX_GRAF <- as.data.frame(RS_Serie_Historica$Periodo)

AUX_GRAF <- AUX_GRAF %>% mutate(Notificados = RS_Serie_Historica$Notificados)

AUX_GRAF <- AUX_GRAF %>% mutate(Confirmados = RS_Serie_Historica$Dengue 
                                + 
                                  RS_Serie_Historica$D.S.A. 
                                + 
                                  RS_Serie_Historica$Dengue_Grave)

colnames(AUX_GRAF) <- c("Periodo", "Notificados", "Confirmados")

#####################################################################################################################
####        Criando gráfico de barras lado a lado Série Histórica Notificados e Confirmados.    #####################
#####################################################################################################################

RS22_GRAF_Serie_Historica_Not_Conf <- ggplot (AUX_GRAF, 
                                              aes(x = Periodo)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Casos",
       title = paste0("CASOS NOTIFICADOS/CONFIRMADOS ", RS, "ªRS (2009 - 2025)")) +
  geom_bar(
    aes( y = Notificados, fill = "Notificados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.1) + 
  scale_fill_manual(name = "", values = c("Notificados" = "#556B2F", "Confirmados" = "#FF6347")) +
  geom_bar(
    aes( y = Confirmados, fill = "Confirmados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = Confirmados,
                 label = Confirmados),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Theme()

###############################################################
#####     Série Histórica de casos Hospitalizados       #######
###############################################################

RS22_GRAF_Serie_Historica_Hospitalizados <- ggplot (RS_Serie_Historica, 
                                                    aes(x = Periodo, 
                                                        y = Hospitalizados)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Casos",
       title = paste0("CASOS HOSPITALIZADOS ", RS, "ªRS (2009 - 2025)") )+
  geom_bar(stat = "identity", 
           fill = "#0F815D",
           color = "black") + 
  geom_label(aes(label = Hospitalizados),
             alpha = 0.5,
             size =3,
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Theme()

############################################################
#####        Série Histórica Sorotipo Circulante      ######
############################################################

RS22_GRAF_Serie_Historica_Sorotipo <- ggplot (RS_Serie_Historica, 
                                              aes(x = Periodo)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Casos",
       title = paste0("SOROTIPO CIRCULANTE ", RS, "ªRS (2009 - 2025)"),
       subtitle = "Sorotipo viral identificado via Pesquisa de Arbovírus pelo LACEN/PR")+
  geom_bar(
    aes( y = DENV_I, fill = "DENV I"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = DENV_I,
                 label = DENV_I),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.1) + 
  scale_fill_manual(name = "", 
                    values = c("DENV I" = "#3CB371", 
                               "DENV II" = "#2F657E")) +
  geom_bar(
    aes( y = DENV_II, 
         fill = "DENV II"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = DENV_II,
                 label = DENV_II),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Theme()

#########################################
#####   Critério de Encerramento   ######
#########################################

RS22_GRAF_2025_Encerramento <- ggplot (RS22_2025_GERAL, 
                                       aes(x = Município)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Casos",
       title = "CRITÉRIO DE ENCERRAMENTO/MUNICÍPIO",
       subtitle = "Casos confirmados e casos descartados") +
  geom_bar(
    aes( y = Criterio_Encerramento_Lab, 
         fill = "Laboratorial"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Criterio_Encerramento_Lab,
                 label = Criterio_Encerramento_Lab),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.1) + 
  scale_fill_manual(name = "", 
                    values = c("Laboratorial" = "#3CB371", 
                               "Clínico-epidemiológico" = "#2F657E")
  ) +
  geom_bar(
    aes( y = Criterio_Encerramento_Clin_Epid, 
         fill = "Clínico-epidemiológico"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = Criterio_Encerramento_Clin_Epid,
                 label = Criterio_Encerramento_Clin_Epid),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 75))

####################################################
###        Sintomas Confirmados e Notificados    ###
####################################################

AUX_GRAF <- data.frame(Sintomas = colnames(RS22_2025_SINAIS_Notificados)[-c(1, 2, 3)],
                       Notificados = NA,
                       Confirmados = NA)

AUX_GRAF[1,2] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Notificados[, 4])/(sum(RS22_2025_GERAL[, 5]))*100)))
AUX_GRAF[2,2] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Notificados[, 5])/(sum(RS22_2025_GERAL[, 5]))*100)))
AUX_GRAF[3,2] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Notificados[, 6])/(sum(RS22_2025_GERAL[, 5]))*100)))
AUX_GRAF[4,2] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Notificados[, 7])/(sum(RS22_2025_GERAL[, 5]))*100)))
AUX_GRAF[5,2] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Notificados[, 8])/(sum(RS22_2025_GERAL[, 5]))*100)))
AUX_GRAF[6,2] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Notificados[, 9])/(sum(RS22_2025_GERAL[, 5]))*100)))
AUX_GRAF[7,2] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Notificados[, 10])/(sum(RS22_2025_GERAL[, 5]))*100)))
AUX_GRAF[8,2] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Notificados[, 11])/(sum(RS22_2025_GERAL[, 5]))*100)))
AUX_GRAF[9,2] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Notificados[, 12])/(sum(RS22_2025_GERAL[, 5]))*100)))
AUX_GRAF[10,2] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Notificados[, 13])/(sum(RS22_2025_GERAL[, 5]))*100)))
AUX_GRAF[11,2] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Notificados[, 14])/(sum(RS22_2025_GERAL[, 5]))*100)))
AUX_GRAF[12,2] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Notificados[, 15])/(sum(RS22_2025_GERAL[, 5]))*100)))
AUX_GRAF[13,2] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Notificados[, 16])/(sum(RS22_2025_GERAL[, 5]))*100)))
AUX_GRAF[14,2] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Notificados[, 17])/(sum(RS22_2025_GERAL[, 5]))*100)))

AUX_GRAF[1,3] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Confirmados[, 4])/((sum(RS22_2025_GERAL[, 6]) + 
                                                                                    sum(RS22_2025_GERAL[, 7]) +
                                                                                    sum(RS22_2025_GERAL[, 8])))*100)))
AUX_GRAF[2,3] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Confirmados[, 5])/((sum(RS22_2025_GERAL[, 6]) + 
                                                                                    sum(RS22_2025_GERAL[, 7]) +
                                                                                    sum(RS22_2025_GERAL[, 8])))*100)))

AUX_GRAF[3,3] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Confirmados[, 6])/((sum(RS22_2025_GERAL[, 6]) + 
                                                                                    sum(RS22_2025_GERAL[, 7]) +
                                                                                    sum(RS22_2025_GERAL[, 8])))*100)))

AUX_GRAF[4,3] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Confirmados[, 7])/((sum(RS22_2025_GERAL[, 6]) + 
                                                                                    sum(RS22_2025_GERAL[, 7]) +
                                                                                    sum(RS22_2025_GERAL[, 8])))*100)))

AUX_GRAF[5,3] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Confirmados[, 8])/((sum(RS22_2025_GERAL[, 6]) + 
                                                                                    sum(RS22_2025_GERAL[, 7]) +
                                                                                    sum(RS22_2025_GERAL[, 8])))*100)))

AUX_GRAF[6,3] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Confirmados[, 9])/((sum(RS22_2025_GERAL[, 6]) + 
                                                                                    sum(RS22_2025_GERAL[, 7]) +
                                                                                    sum(RS22_2025_GERAL[, 8])))*100)))

AUX_GRAF[7,3] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Confirmados[, 10])/((sum(RS22_2025_GERAL[, 6]) + 
                                                                                     sum(RS22_2025_GERAL[, 7]) +
                                                                                     sum(RS22_2025_GERAL[, 8])))*100)))

AUX_GRAF[8,3] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Confirmados[, 11])/((sum(RS22_2025_GERAL[, 6]) + 
                                                                                     sum(RS22_2025_GERAL[, 7]) +
                                                                                     sum(RS22_2025_GERAL[, 8])))*100)))

AUX_GRAF[9,3] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Confirmados[, 12])/((sum(RS22_2025_GERAL[, 6]) + 
                                                                                     sum(RS22_2025_GERAL[, 7]) +
                                                                                     sum(RS22_2025_GERAL[, 8])))*100)))

AUX_GRAF[10,3] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Confirmados[, 13])/((sum(RS22_2025_GERAL[, 6]) + 
                                                                                      sum(RS22_2025_GERAL[, 7]) +
                                                                                      sum(RS22_2025_GERAL[, 8])))*100)))

AUX_GRAF[11,3] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Confirmados[, 14])/((sum(RS22_2025_GERAL[, 6]) + 
                                                                                      sum(RS22_2025_GERAL[, 7]) +
                                                                                      sum(RS22_2025_GERAL[, 8])))*100)))

AUX_GRAF[12,3] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Confirmados[, 15])/((sum(RS22_2025_GERAL[, 6]) + 
                                                                                      sum(RS22_2025_GERAL[, 7]) +
                                                                                      sum(RS22_2025_GERAL[, 8])))*100)))

AUX_GRAF[13,3] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Confirmados[, 16])/((sum(RS22_2025_GERAL[, 6]) + 
                                                                                      sum(RS22_2025_GERAL[, 7]) +
                                                                                      sum(RS22_2025_GERAL[, 8])))*100)))

AUX_GRAF[14,3] <- as.numeric(format(round(sum(RS22_2025_SINAIS_Confirmados[, 17])/((sum(RS22_2025_GERAL[, 6]) + 
                                                                                      sum(RS22_2025_GERAL[, 7]) +
                                                                                      sum(RS22_2025_GERAL[, 8])))*100)))


AUX_GRAF[,1] <- c("Febre", "Mialgia", "Cefaleia", "Exantema", "Vômito", "Náusea", "Dor nas 
Costas", "Conjuntivite", "Artrite", 
                  "Artralgia 
Intensa", "Petéquias", "Leucopenia", "Prova do 
Laço Positiva", "Dor 
Retroorbital")

RS22_GRAF_2025_SINAIS <- ggplot (AUX_GRAF, 
                                 aes(x = Sintomas)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Percentual de Casos",
       title = paste0("Distribuição das Notificações por Sinais/Sintomas ", RS, "ªRS - 2025"),
       subtitle = "Sinais Clínicos/Sintomas em Notificações de DENGUE Assinalados no Campo 33 da Ficha do SINAN") +
  geom_bar(
    aes( y = Notificados, 
         fill = "Notificados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.1) + 
  scale_fill_manual(name = "", 
                    values = c("Notificados" = "#9F9F5F", 
                               "Confirmados" = "#4E2F2F")) +
  geom_bar(
    aes( y = Confirmados, 
         fill = "Confirmados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = Confirmados,
                 label = Confirmados),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 65))

###########################################################################
#####      Casos Notificados/Confirmados por município - Período sazonal atual     ####
###########################################################################

AUX_GRAF <- data.frame (Municípios = RS22_2025_GERAL[, 2],
                        Notificados = RS22_2025_GERAL[, 5],
                        Confirmados = (RS22_2025_GERAL[, 6] + 
                                         RS22_2025_GERAL[, 7] + 
                                         RS22_2025_GERAL[, 8]
                        )
)

RS22_GRAF_2025_Not_Conf <- ggplot (AUX_GRAF, 
                                   aes(x = Municípios)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Casos",
       title = "CASOS NOTIFICADOS E CONFIRMADOS/MUNICÍPIO - 2025") +
  geom_bar(aes(y = Notificados,
               fill = "Notificados"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1,
             nudge_x = -.20)  +
  geom_bar(aes(y = Confirmados,
               fill = "Confirmados"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = .20)) + 
  geom_label(aes(y = Confirmados,
                 label = Confirmados), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1,
             nudge_x = .20) +
  scale_fill_manual(name = "", 
                    values = c("Notificados" = "#046236", 
                               "Confirmados" = "#8E1C21")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 75))

###########################################################################
#####      Casos autóctones por município - Período sazonal atual      ####
###########################################################################

RS22_GRAF_2025_Autoctones <- ggplot (RS22_2025_GERAL, 
                                     aes(x = Município, 
                                         y = Autoctones)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Casos",
       title = "CASOS AUTÓCTONES/MUNICÍPIO - 2025") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#BDB76B") + 
  geom_label(aes(label = Autoctones), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))  +
  Theme() +
  theme(axis.text.x = element_text(angle = 75))

###########################################################################
#####  Casos em investigação por município - Período sazonal atual     ####
###########################################################################

RS22_GRAF_2025_Investigacao <- ggplot (RS22_2025_GERAL, 
                                       aes(x = Município, 
                                           y = Em_Investigacao)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Casos",
       title = "CASOS EM INVESTIGAÇÃO/MUNICÍPIO - 2025") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#EEE8AA") + 
  geom_label(aes(label = Em_Investigacao), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))  +
  Theme() +
  theme(axis.text.x = element_text(angle = 75))

###########################################################################
#####      Incidência por município - Período sazonal atual            ####
###########################################################################

RS22_GRAF_2025_Incidencia <- ggplot (RS22_2025_GERAL, 
                                     aes(x = Município, 
                                         y = Incidencia)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Casos",
       title = "INCIDÊNCIA/MUNICÍPIO (CASOS AUTÓCTONES) - 2025",
       subtitle = "Casos/100.000 habitantes") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Incidencia), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 75))


###########################################################################
#####      Incidência Prováveis por município - Período sazonal atual            ####
###########################################################################

RS22_GRAF_2025_Incidencia_Provaveis <- ggplot (RS22_2025_GERAL, 
                                               aes(x = Município, 
                                                   y = Incidencia_Provaveis)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Casos",
       title = "Incidência de Casos Prováveis/Município - 2025",
       subtitle = "Casos Prováveis/100.000 habitantes") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#565900") + 
  geom_label(aes(label = Incidencia_Provaveis), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 75))

###########################################################################
#####      Casos descartados por município - Período sazonal atual     ####
###########################################################################

RS22_GRAF_2025_Descartados <- ggplot (RS22_2025_GERAL, 
                                      aes(x = Município, 
                                          y = Descartados)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Casos",
       title = "CASOS DESCARTADOS/MUNICÍPIO - 2025") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#8FBC8F") + 
  geom_label(aes(label = Descartados),
             alpha = 0.5,
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 75))

###########################################################################
#####   Casos hospitalizados por município - Período sazonal atual     ####
###########################################################################

RS22_GRAF_2025_Hospitalizados <- ggplot (RS22_2025_GERAL, 
                                         aes(x = Município, 
                                             y = Hospitalizacao)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Casos",
       title = "CASOS HOSPITALIZADOS/MUNICÍPIO - 2025") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#D2B48C") + 
  geom_label(aes(label = Hospitalizacao),
             alpha = 0.5,
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 75))

###########################################################################
#####      Casos Inconclusivos por município - Período sazonal atual   ####
###########################################################################

AUX_GRAF <- data.frame (Municípios = RS22_2025_GERAL[, 2],
                        Inconclusivos = (RS22_2025_GERAL[, 20]
                        )
)

RS22_GRAF_2025_Inconclusivos <- ggplot (AUX_GRAF, 
                                        aes(x = Municípios, 
                                            y = Inconclusivos)) + 
  labs(caption = Fonte, 
       y = NULL,
       title = "CASOS INCONCLUSIVOS/MUNICÍPIO - 2025",
       subtitle = "Casos notificados e encerrados automaticamente pelo sistema após 60 dias") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#856363") + 
  geom_label(aes(label = Inconclusivos), 
             size = 3, 
             alpha = 0.5,
             vjust = 0.1)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 75))


#######################################################
######     Histogramas Municipais   ###################
#######################################################

###       NOTIFICADOS     ########
RS_2025_SE_Notificados_CHIK <- RS_2025_SE_Notificados_CHIK[, -54]

# RS_2025_SE_Notificados_CHIK[nrow(RS_2025_SE_Notificados_CHIK) +1, ] <- colnames(RS_2025_SE_Notificados_CHIK)

RS_2025_SE_Notificados_CHIK[nrow(RS_2025_SE_Notificados_CHIK) +1, 2:ncol(RS_2025_SE_Notificados_CHIK)] <- c("1",  "2", "3", 
                                                                                                            "4",  "5",  "6",  
                                                                                                            "7",  "8",  "9",  
                                                                                                            "10",  "11",  "12",  
                                                                                                            "13",  "14",  "15",  
                                                                                                            "16",  "17",  "18",  
                                                                                                            "19",  "20",  "21",  
                                                                                                            "22",  "23",  
                                                                                                            "24",  "25",  "26",  
                                                                                                            "27",  "28",  "29",  
                                                                                                            "30",  "31",  "32", 
                                                                                                            "33",  "34",  "35",  
                                                                                                            "36",  "37",  "38",  
                                                                                                            "39",  "40",  "41",  
                                                                                                            "42",  "43",  "244",  
                                                                                                            "45",  "46",  "47",  
                                                                                                            "48",  "49",  "50",  
                                                                                                            "51",  "52")

AUX_GRAF <- as.data.frame(RS_2025_SE_Notificados_CHIK$Município)

AUX_GRAF[, 2] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(RS_2025_SE_Notificados_CHIK[, which(colnames(RS_2025_SE_Notificados_CHIK) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(RS_2025_SE_Notificados_CHIK)[which(colnames(RS_2025_SE_Notificados_CHIK) == SE)]

AUX_GRAF[nrow(AUX_GRAF), 1] <- "Municipios"

AUX_GRAF <- AUX_GRAF[-(nrow(AUX_GRAF)-1),]

AUX_GRAF <- AUX_GRAF[c(nrow(AUX_GRAF), 1:(nrow(AUX_GRAF) -1)), ]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF[, 2: ncol(AUX_GRAF)] <- apply(AUX_GRAF[, 2: ncol(AUX_GRAF)], 2, as.numeric)

rownames(AUX_GRAF) <- 1: nrow(AUX_GRAF)

colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")

###############  Criando uma função para o tema do gráfico   ##################

Theme_Hist <- function(){ 
  theme_minimal(base_size = 10) %+replace%  
    theme(
      axis.text.x = element_text(face = "bold"),
      panel.grid.major = element_line(color = "#C0C0C0"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#F5F5F5"),
      plot.title = element_text(face = "bold", 
                                size = 15, 
                                colour = "#556B2F")
    )
}

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

######   Criando o conjunto de gráficos dos histogramas via Lapply.  ######

AUX_HIST_CHIK_NOT_LIST <- AUX_GRAF %>%
  pivot_longer(-SE, names_to = "Municipios") %>% 
  mutate(
    SE = SE,
    Municipios = gsub("_", " ", Municipios)
  ) %>%
  group_split(Municipios) %>% 
  lapply(\(dados) {
    titulo <- dados$Municipios %>% 
      unique() %>% 
      paste0(" - Notificados")
    ggplot(dados, aes(x = SE, 
                      y = value)
    ) + 
      geom_col(color = "black", 
               fill = "#00FF00") + 
      geom_label(aes(label = value), 
                 alpha = 0.5, 
                 vjust = 0.1) +
      labs(
        caption = Fonte, 
        x = "Semana Epidemiológica",
        y = "Número de Casos",
        title = titulo
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      Theme_Hist()
  })

RS_2025_GRAF_CHIK_Histograma_Notificados_01 <- (AUX_HIST_CHIK_NOT_LIST[[1]] + AUX_HIST_CHIK_NOT_LIST[[2]]) / 
  (AUX_HIST_CHIK_NOT_LIST[[3]] + AUX_HIST_CHIK_NOT_LIST[[4]]) / 
  (AUX_HIST_CHIK_NOT_LIST[[5]] + AUX_HIST_CHIK_NOT_LIST[[6]]) / 
  (AUX_HIST_CHIK_NOT_LIST[[7]] + AUX_HIST_CHIK_NOT_LIST[[8]]) 

RS_2025_GRAF_CHIK_Histograma_Notificados_02 <- (AUX_HIST_CHIK_NOT_LIST[[9]] + AUX_HIST_CHIK_NOT_LIST[[10]]) / 
  (AUX_HIST_CHIK_NOT_LIST[[11]] + AUX_HIST_CHIK_NOT_LIST[[12]]) / 
  (AUX_HIST_CHIK_NOT_LIST[[13]] + AUX_HIST_CHIK_NOT_LIST[[14]]) / 
  (AUX_HIST_CHIK_NOT_LIST[[15]] + AUX_HIST_CHIK_NOT_LIST[[16]]) 

###     Confirmados    #####

RS_2025_SE_Confirmados_CHIK <- RS_2025_SE_Confirmados_CHIK[, -54]

RS_2025_SE_Confirmados_CHIK[nrow(RS_2025_SE_Confirmados_CHIK) +1, 2:ncol(RS_2025_SE_Confirmados_CHIK)] <- c("1",  "2", "3", 
                                                                                                            "4",  "5",  "6",  
                                                                                                            "7",  "8",  "9",  
                                                                                                            "10",  "11",  "12",  
                                                                                                            "13",  "14",  "15",  
                                                                                                            "16",  "17",  "18",  
                                                                                                            "19",  "20",  "21",  
                                                                                                            "22",  "23",  
                                                                                                            "24",  "25",  "26",  
                                                                                                            "27",  "28",  "29",  
                                                                                                            "30",  "31",  "32", 
                                                                                                            "33",  "34",  "35",  
                                                                                                            "36",  "37",  "38",  
                                                                                                            "39",  "40",  "41",  
                                                                                                            "42",  "43",  "244",  
                                                                                                            "45",  "46",  "47",  
                                                                                                            "48",  "49",  "50",  
                                                                                                            "51",  "52")

AUX_GRAF <- as.data.frame(RS_2025_SE_Confirmados_CHIK$Município)

AUX_GRAF[, 2] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(RS_2025_SE_Confirmados_CHIK[, which(colnames(RS_2025_SE_Confirmados_CHIK) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(RS_2025_SE_Confirmados_CHIK)[which(colnames(RS_2025_SE_Confirmados_CHIK) == SE)]

AUX_GRAF[nrow(AUX_GRAF), 1] <- "Municipios"

AUX_GRAF <- AUX_GRAF[-(nrow(AUX_GRAF)-1),]

AUX_GRAF <- AUX_GRAF[c(nrow(AUX_GRAF), 1:(nrow(AUX_GRAF) -1)), ]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF[, 2: ncol(AUX_GRAF)] <- apply(AUX_GRAF[, 2: ncol(AUX_GRAF)], 2, as.numeric)

rownames(AUX_GRAF) <- 1: nrow(AUX_GRAF)

colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

######   Criando o conjunto de gráficos dos histogramas via Lapply.  ######

AUX_HIST_CHIK_CONF_LIST <- AUX_GRAF %>%
  pivot_longer(-SE, names_to = "Municipios") %>% 
  mutate(
    SE = SE,
    Municipios = gsub("_", " ", Municipios)
  ) %>%
  group_split(Municipios) %>% 
  lapply(\(dados) {
    titulo <- dados$Municipios %>% 
      unique() %>% 
      paste0(" - Confirmados")
    ggplot(dados, aes(x = SE, 
                      y = value)
    ) + 
      geom_col(color = "black", 
               fill = "#DB7093") + 
      geom_label(aes(label = value), 
                 alpha = 0.5, 
                 vjust = 0.1) +
      labs(
        caption = Fonte, 
        x = "Semana Epidemiológica",
        y = "Número de Casos",
        title = titulo
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      Theme_Hist()
  })

RS_2025_GRAF_CHIK_Histograma_Confirmados_01 <- (AUX_HIST_CHIK_CONF_LIST[[1]] + AUX_HIST_CHIK_CONF_LIST[[2]]) / 
  (AUX_HIST_CHIK_CONF_LIST[[3]] + AUX_HIST_CHIK_CONF_LIST[[4]]) / 
  (AUX_HIST_CHIK_CONF_LIST[[5]] + AUX_HIST_CHIK_CONF_LIST[[6]]) / 
  (AUX_HIST_CHIK_CONF_LIST[[7]] + AUX_HIST_CHIK_CONF_LIST[[8]]) 

RS_2025_GRAF_CHIK_Histograma_Confirmados_02 <- (AUX_HIST_CHIK_CONF_LIST[[9]] + AUX_HIST_CHIK_CONF_LIST[[10]]) / 
  (AUX_HIST_CHIK_CONF_LIST[[11]] + AUX_HIST_CHIK_CONF_LIST[[12]]) / 
  (AUX_HIST_CHIK_CONF_LIST[[13]] + AUX_HIST_CHIK_CONF_LIST[[14]]) / 
  (AUX_HIST_CHIK_CONF_LIST[[15]] + AUX_HIST_CHIK_CONF_LIST[[16]]) 

######Histogramas

###Provaveis

RS_2025_SE_Provaveis_CHIK <- RS_2025_SE_Provaveis_CHIK[, -54]
RS_2025_SE_Provaveis_CHIK[nrow(RS_2025_SE_Provaveis_CHIK) +1, 2:ncol(RS_2025_SE_Provaveis_CHIK)] <- c("1",  "2", "3", 
                                                                                                      "4",  "5",  "6",  
                                                                                                      "7",  "8",  "9",  
                                                                                                      "10",  "11",  "12",  
                                                                                                      "13",  "14",  "15",  
                                                                                                      "16",  "17",  "18",  
                                                                                                      "19",  "20",  "21",  
                                                                                                      "22",  "23",  
                                                                                                      "24",  "25",  "26",  
                                                                                                      "27",  "28",  "29",  
                                                                                                      "30",  "31",  "32", 
                                                                                                      "33",  "34",  "35",  
                                                                                                      "36",  "37",  "38",  
                                                                                                      "39",  "40",  "41",  
                                                                                                      "42",  "43",  "244",  
                                                                                                      "45",  "46",  "47",  
                                                                                                      "48",  "49",  "50",  
                                                                                                      "51",  "52")


AUX_GRAF <- as.data.frame(RS_2025_SE_Provaveis_CHIK$Município)

AUX_GRAF[, 2] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(RS_2025_SE_Provaveis_CHIK[, which(colnames(RS_2025_SE_Provaveis_CHIK) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(RS_2025_SE_Provaveis_CHIK)[which(colnames(RS_2025_SE_Provaveis_CHIK) == SE)]


AUX_GRAF[nrow(AUX_GRAF), 1] <- "Municipios"

AUX_GRAF <- AUX_GRAF[-(nrow(AUX_GRAF)-1),]

AUX_GRAF <- AUX_GRAF[c(nrow(AUX_GRAF), 1:(nrow(AUX_GRAF) -1)), ]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF[, 2: ncol(AUX_GRAF)] <- apply(AUX_GRAF[, 2: ncol(AUX_GRAF)], 2, as.numeric)

rownames(AUX_GRAF) <- 1: nrow(AUX_GRAF)

colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")
colnames(AUX_GRAF) <- str_replace(colnames(AUX_GRAF), " ", "_")

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

AUX_HIST_CHIK_PROV_LIST <- AUX_GRAF %>%
  pivot_longer(-SE, names_to = "Municipios") %>% 
  mutate(
    SE = SE,
    Municipios = gsub("_", " ", Municipios)
  ) %>%
  group_split(Municipios) %>% 
  lapply(\(dados) {
    titulo <- dados$Municipios %>% 
      unique() %>% 
      paste0(" - Prováveis")
    ggplot(dados, aes(x = SE, 
                      y = value)
    ) + 
      geom_col(color = "black", 
               fill = "#DAA520") + 
      geom_label(aes(label = value), 
                 alpha = 0.5, 
                 vjust = 0.1) +
      labs(
        caption = Fonte, 
        x = "Semana Epidemiológica",
        y = "Número de Casos",
        title = titulo
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      Theme_Hist()
  })

RS_2025_GRAF_CHIK_Histograma_Provaveis_01 <- (AUX_HIST_CHIK_PROV_LIST[[1]] + AUX_HIST_CHIK_PROV_LIST[[2]]) / 
  (AUX_HIST_CHIK_PROV_LIST[[3]] + AUX_HIST_CHIK_PROV_LIST[[4]]) / 
  (AUX_HIST_CHIK_PROV_LIST[[5]] + AUX_HIST_CHIK_PROV_LIST[[6]]) / 
  (AUX_HIST_CHIK_PROV_LIST[[7]] + AUX_HIST_CHIK_PROV_LIST[[8]]) 

RS_2025_GRAF_CHIK_Histograma_Provaveis_02 <- (AUX_HIST_CHIK_PROV_LIST[[9]] + AUX_HIST_CHIK_PROV_LIST[[10]]) / 
  (AUX_HIST_CHIK_PROV_LIST[[11]] + AUX_HIST_CHIK_PROV_LIST[[12]]) / 
  (AUX_HIST_CHIK_PROV_LIST[[13]] + AUX_HIST_CHIK_PROV_LIST[[14]]) / 
  (AUX_HIST_CHIK_PROV_LIST[[15]] + AUX_HIST_CHIK_PROV_LIST[[16]]) 


#################################

PR_2025_CHIK_SINAIS_NOTIFICADOS <- tibble(Febre = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>% 
                                                               filter(Febre == "SIM" ) %>%
                                                               count()),
                                          Mialgia = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                 filter(Mialgia == "SIM" ) %>%
                                                                 count()),
                                          Cefaleia = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                  filter(Cefaleia == "SIM") %>%
                                                                  count()),
                                          Exantema = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                  filter(Exantema == "SIM") %>%
                                                                  count()),
                                          Vomito = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                filter(Vomito == "SIM") %>%
                                                                count()),
                                          Nausea = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                filter(Nausea == "SIM") %>%
                                                                count()),
                                          Dor_nas_Costas = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                        filter(Dor_nas_Costas == "SIM") %>%
                                                                        count()),
                                          Conjuntivite = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                      filter(Conjuntivite == "SIM") %>%
                                                                      count()),
                                          Artrite = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                 filter(Artrite == "SIM") %>%
                                                                 count()),
                                          Artralgia_Intensa = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                           filter(Artralgia_Intensa == "SIM") %>%
                                                                           count()),
                                          Petequias = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                   filter(Petequias == "SIM") %>%
                                                                   count()),
                                          Leucopenia = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                    filter(Leucopenia == "SIM") %>%
                                                                    count()),
                                          Prova_do_Laco_Positiva = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                                filter(Prova_do_Laco_Positiva == "SIM") %>%
                                                                                count()),
                                          Dor_retroorbital = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                          filter(Dor_retroorbital == "SIM") %>%
                                                                          count())
)
colnames(PR_2025_CHIK_SINAIS_NOTIFICADOS) <- c("Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", "Dor_Retroorbital")

PR_2025_CHIK_SINAIS_Confirmados <- tibble(Febre = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>% 
                                                               filter(Febre == "SIM",
                                                                      Classificacao_Final == "CHIKUNGUNYA") %>%
                                                               count()),
                                          Mialgia = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                 filter(Mialgia == "SIM",
                                                                        Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                 count()),
                                          Cefaleia = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                  filter(Cefaleia == "SIM",
                                                                         Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                  count()),
                                          Exantema = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                  filter(Exantema == "SIM",
                                                                         Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                  count()),
                                          Vomito = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                filter(Vomito == "SIM",
                                                                       Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                count()),
                                          Nausea = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                filter(Nausea == "SIM",
                                                                       Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                count()),
                                          Dor_nas_Costas = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                        filter(Dor_nas_Costas == "SIM",
                                                                               Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                        count()),
                                          Conjuntivite = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                      filter(Conjuntivite == "SIM",
                                                                             Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                      count()),
                                          Artrite = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                 filter(Artrite == "SIM",
                                                                        Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                 count()),
                                          Artralgia_Intensa = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                           filter(Artralgia_Intensa == "SIM",
                                                                                  Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                           count()),
                                          Petequias = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                   filter(Petequias == "SIM",
                                                                          Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                   count()),
                                          Leucopenia = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                    filter(Leucopenia == "SIM",
                                                                           Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                    count()),
                                          Prova_do_Laco_Positiva = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                                filter(Prova_do_Laco_Positiva == "SIM",
                                                                                       Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                                count()),
                                          Dor_retroorbital = as.integer(PR_2025_SINAN_DECODIFICADO_CHIK %>%
                                                                          filter(Dor_retroorbital == "SIM",
                                                                                 Classificacao_Final == "CHIKUNGUNYA") %>%
                                                                          count())
)
#colnames(PR_2025_CHIK_SINAIS_NOTIFICADOS) <- c("Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", "Dor_Retroorbital")


AUX_GRAF <- data.frame(Sintomas = c("Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor nas Costas", "Conjuntivite", "Artrite", "Artralgia Intensa", "Petequias", "Leucopenia", "Prova do Laco Positiva", "Dor Retroorbital"),
                       Notificados = NA,
                       Confirmados = NA)

AUX_GRAF[1,2] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_NOTIFICADOS[, 1])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 4])*100)))
AUX_GRAF[2,2] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_NOTIFICADOS[, 2])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 4])*100)))
AUX_GRAF[3,2] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_NOTIFICADOS[, 3])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 4])*100)))
AUX_GRAF[4,2] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_NOTIFICADOS[, 4])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 4])*100)))
AUX_GRAF[5,2] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_NOTIFICADOS[, 5])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 4])*100)))
AUX_GRAF[6,2] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_NOTIFICADOS[, 6])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 4])*100)))
AUX_GRAF[7,2] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_NOTIFICADOS[, 7])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 4])*100)))
AUX_GRAF[8,2] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_NOTIFICADOS[, 8])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 4])*100)))
AUX_GRAF[9,2] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_NOTIFICADOS[, 9])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 4])*100)))
AUX_GRAF[10,2] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_NOTIFICADOS[, 10])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 4])*100)))
AUX_GRAF[11,2] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_NOTIFICADOS[, 11])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 4])*100)))
AUX_GRAF[12,2] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_NOTIFICADOS[, 12])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 4])*100)))
AUX_GRAF[13,2] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_NOTIFICADOS[, 13])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 4])*100)))
AUX_GRAF[14,2] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_NOTIFICADOS[, 14])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 4])*100)))

AUX_GRAF[1,3] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_Confirmados[, 1])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 5])*100)))
AUX_GRAF[2,3] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_Confirmados[, 2])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 5])*100)))
AUX_GRAF[3,3] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_Confirmados[, 3])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 5])*100)))
AUX_GRAF[4,3] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_Confirmados[, 4])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 5])*100)))
AUX_GRAF[5,3] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_Confirmados[, 5])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 5])*100)))
AUX_GRAF[6,3] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_Confirmados[, 6])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 5])*100)))
AUX_GRAF[7,3] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_Confirmados[, 7])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 5])*100)))
AUX_GRAF[8,3] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_Confirmados[, 8])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 5])*100)))
AUX_GRAF[9,3] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_Confirmados[, 9])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 5])*100)))
AUX_GRAF[10,3] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_Confirmados[, 10])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 5])*100)))
AUX_GRAF[11,3] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_Confirmados[, 11])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 5])*100)))
AUX_GRAF[12,3] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_Confirmados[, 12])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 5])*100)))
AUX_GRAF[13,3] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_Confirmados[, 13])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 5])*100)))
AUX_GRAF[14,3] <- as.numeric(format(round(sum(PR_2025_CHIK_SINAIS_Confirmados[, 14])/(PR_CHIK_2025_GERAL[nrow(PR_CHIK_2025_GERAL), 5])*100)))

PR_2025_GRAF_SINAIS_CHIK <- ggplot (AUX_GRAF, 
                                    aes(x = Sintomas)) + 
  theme(axis.text.x = element_text(angle = 80, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 12)) +
  labs(caption = Fonte, 
       x = "Sintomas",
       y = "Percentual de Casos",
       title = "Distribuição das Notificações por Sinais/Sintomas PARANÁ - 2025",
       subtitle = "Sinais/Sintomas Clínicos em Notificações de CHIKUNGUNYA Assinalados no Campo 33 da Ficha do SINAN") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold",
                                   size = 19,
                                   colour = "#556B2F")) +
  geom_bar(aes( y = Notificados, 
                fill = "Notificados"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.1) + 
  scale_fill_manual(name = "", 
                    values = c("Notificados" = "#4D5656", 
                               "Confirmados" = "#B03A2E")) +
  theme(legend.position = "bottom") +
  geom_bar(aes( y = Confirmados, 
                fill = "Confirmados"),
           stat = "identity",
           color = "black",
           width = .4,
           position = position_nudge(x = .20)) +
  geom_label(aes(y = Confirmados,
                 label = Confirmados),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))


AUX_GRAF <- data.frame(Sintomas = c("Febre", "Mialgia", "Cefaleia", "Exantema", 
                                    "Vômito", "Náusea", "Dor nas Costas", "Conjuntivite", 
                                    "Artrite", "Artralgia Intensa", "Petéquias", "Leucopenia", 
                                    "Prova do Laco Positiva", "Dor Retroorbital"),
                       Notificados = NA,
                       Confirmados = NA)

AUX_GRAF[1,2] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Notificados[, 1])/(sum(PR_DENGUE_2025_GERAL[, 4]))*100)))
AUX_GRAF[2,2] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Notificados[, 2])/(sum(PR_DENGUE_2025_GERAL[, 4]))*100)))
AUX_GRAF[3,2] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Notificados[, 3])/(sum(PR_DENGUE_2025_GERAL[, 4]))*100)))
AUX_GRAF[4,2] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Notificados[, 4])/(sum(PR_DENGUE_2025_GERAL[, 4]))*100)))
AUX_GRAF[5,2] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Notificados[, 5])/(sum(PR_DENGUE_2025_GERAL[, 4]))*100)))
AUX_GRAF[6,2] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Notificados[, 6])/(sum(PR_DENGUE_2025_GERAL[, 4]))*100)))
AUX_GRAF[7,2] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Notificados[, 7])/(sum(PR_DENGUE_2025_GERAL[, 4]))*100)))
AUX_GRAF[8,2] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Notificados[, 8])/(sum(PR_DENGUE_2025_GERAL[, 4]))*100)))
AUX_GRAF[9,2] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Notificados[, 9])/(sum(PR_DENGUE_2025_GERAL[, 4]))*100)))
AUX_GRAF[10,2] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Notificados[, 10])/(sum(PR_DENGUE_2025_GERAL[, 4]))*100)))
AUX_GRAF[11,2] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Notificados[, 11])/(sum(PR_DENGUE_2025_GERAL[, 4]))*100)))
AUX_GRAF[12,2] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Notificados[, 12])/(sum(PR_DENGUE_2025_GERAL[, 4]))*100)))
AUX_GRAF[13,2] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Notificados[, 13])/(sum(PR_DENGUE_2025_GERAL[, 4]))*100)))
AUX_GRAF[14,2] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Notificados[, 14])/(sum(PR_DENGUE_2025_GERAL[, 4]))*100)))

AUX_GRAF[1,3] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Confirmados[, 1])/((sum(PR_DENGUE_2025_GERAL[, 6]) + 
                                                                                         sum(PR_DENGUE_2025_GERAL[, 7]) +
                                                                                         sum(PR_DENGUE_2025_GERAL[, 8])))*100)))

AUX_GRAF[2,3] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Confirmados[, 2])/((sum(PR_DENGUE_2025_GERAL[, 6]) + 
                                                                                         sum(PR_DENGUE_2025_GERAL[, 7]) +
                                                                                         sum(PR_DENGUE_2025_GERAL[, 8])))*100)))

AUX_GRAF[3,3] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Confirmados[, 3])/((sum(PR_DENGUE_2025_GERAL[, 6]) + 
                                                                                         sum(PR_DENGUE_2025_GERAL[, 7]) +
                                                                                         sum(PR_DENGUE_2025_GERAL[, 8])))*100)))

AUX_GRAF[4,3] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Confirmados[, 4])/((sum(PR_DENGUE_2025_GERAL[, 6]) + 
                                                                                         sum(PR_DENGUE_2025_GERAL[, 7]) +
                                                                                         sum(PR_DENGUE_2025_GERAL[, 8])))*100)))

AUX_GRAF[5,3] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Confirmados[, 5])/((sum(PR_DENGUE_2025_GERAL[, 6]) + 
                                                                                         sum(PR_DENGUE_2025_GERAL[, 7]) +
                                                                                         sum(PR_DENGUE_2025_GERAL[, 8])))*100)))

AUX_GRAF[6,3] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Confirmados[, 6])/((sum(PR_DENGUE_2025_GERAL[, 6]) + 
                                                                                         sum(PR_DENGUE_2025_GERAL[, 7]) +
                                                                                         sum(PR_DENGUE_2025_GERAL[, 8])))*100)))

AUX_GRAF[7,3] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Confirmados[, 7])/((sum(PR_DENGUE_2025_GERAL[, 6]) + 
                                                                                         sum(PR_DENGUE_2025_GERAL[, 7]) +
                                                                                         sum(PR_DENGUE_2025_GERAL[, 8])))*100)))

AUX_GRAF[8,3] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Confirmados[, 8])/((sum(PR_DENGUE_2025_GERAL[, 6]) + 
                                                                                         sum(PR_DENGUE_2025_GERAL[, 7]) +
                                                                                         sum(PR_DENGUE_2025_GERAL[, 8])))*100)))

AUX_GRAF[9,3] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Confirmados[, 9])/((sum(PR_DENGUE_2025_GERAL[, 6]) + 
                                                                                         sum(PR_DENGUE_2025_GERAL[, 7]) +
                                                                                         sum(PR_DENGUE_2025_GERAL[, 8])))*100)))

AUX_GRAF[10,3] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Confirmados[, 10])/((sum(PR_DENGUE_2025_GERAL[, 6]) + 
                                                                                           sum(PR_DENGUE_2025_GERAL[, 7]) +
                                                                                           sum(PR_DENGUE_2025_GERAL[, 8])))*100)))

AUX_GRAF[11,3] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Confirmados[, 11])/((sum(PR_DENGUE_2025_GERAL[, 6]) + 
                                                                                           sum(PR_DENGUE_2025_GERAL[, 7]) +
                                                                                           sum(PR_DENGUE_2025_GERAL[, 8])))*100)))

AUX_GRAF[12,3] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Confirmados[, 12])/((sum(PR_DENGUE_2025_GERAL[, 6]) + 
                                                                                           sum(PR_DENGUE_2025_GERAL[, 7]) +
                                                                                           sum(PR_DENGUE_2025_GERAL[, 8])))*100)))

AUX_GRAF[13,3] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Confirmados[, 13])/((sum(PR_DENGUE_2025_GERAL[, 6]) + 
                                                                                           sum(PR_DENGUE_2025_GERAL[, 7]) +
                                                                                           sum(PR_DENGUE_2025_GERAL[, 8])))*100)))

AUX_GRAF[14,3] <- as.numeric(format(round(sum(PR_2025_DENGUE_SINAIS_Confirmados[, 14])/((sum(PR_DENGUE_2025_GERAL[, 6]) + 
                                                                                           sum(PR_DENGUE_2025_GERAL[, 7]) +
                                                                                           sum(PR_DENGUE_2025_GERAL[, 8])))*100)))


AUX_GRAF[,1] <- c("Febre", "Mialgia", "Cefaleia", "Exantema", "Vômito", "Náusea", "Dor nas 
Costas", "Conjuntivite", "Artrite", 
                  "Artralgia 
Intensa", "Petéquias", "Leucopenia", "Prova do 
Laço Positiva", "Dor 
Retroorbital")

######  Construção do Gráfico de SInais Estadual   #####

PR_DENGUE_2025_GRAF_SINAIS <- ggplot (AUX_GRAF, 
                                      aes(x = Sintomas)) + 
  theme(axis.text.x = element_text(angle = 80, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14),
        panel.grid.major = element_line(color = "#C0C0C0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.title = element_text(face = "bold",
                                  size = 19,
                                  colour = "#556B2F"),
        legend.position = "bottom") +
  labs(caption = Fonte, 
       x = "Sintomas",
       y = "Percentual de Casos",
       title = "Distribuição das Notificações por Sinais/Sintomas PARANÁ - 2025",
       subtitle = "Sinais Clínicos/Sintomas em Notificações de DENGUE Assinalados no Campo 33 da Ficha do SINAN") +
  geom_bar(
    aes( y = Notificados, fill = "Notificados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.1) + 
  scale_fill_manual(name = "", 
                    values = c("Notificados" = "#9F9F5F", 
                               "Confirmados" = "#4E2F2F")) +
  geom_bar(
    aes( y = Confirmados, 
         fill = "Confirmados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = Confirmados,
                 label = Confirmados),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))

####################################################################################################
####      Elaborando Quadro com dados de sexo, idade, zona de moradia e escolaridade           #####
####################################################################################################

AUX01 <- tibble(Menos_1_ano = as.integer(PR_DENGUE_2025_SINAN %>% 
                                           filter(NU_IDADE_N <=3012) %>% 
                                           count()),
                Um_a_Cinco_Anos = as.integer(PR_DENGUE_2025_SINAN %>% 
                                               filter(NU_IDADE_N > 4000 
                                                      & 
                                                        NU_IDADE_N <=4005) %>% 
                                               count()),
                Cinco_a_Doze_Anos = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                 filter(NU_IDADE_N > 4005 
                                                        & 
                                                          NU_IDADE_N <=4012) %>% 
                                                 count()),
                Doze_a_Dezoito_Anos = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                   filter(NU_IDADE_N > 4012 
                                                          & 
                                                            NU_IDADE_N <=4018) %>% 
                                                   count()),
                Dezoito_a_Cinq_Nove = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                   filter(NU_IDADE_N > 4018 
                                                          & 
                                                            NU_IDADE_N <= 4059) %>%
                                                   count()),
                Maior_Sessenta = as.integer(PR_DENGUE_2025_SINAN %>% 
                                              filter(NU_IDADE_N > 4059 ) %>%
                                              count()),
                Area_Urbana = as.integer(PR_DENGUE_2025_SINAN %>% 
                                           filter(CS_ZONA == 1) %>% 
                                           count()),
                Area_Rural = as.integer(PR_DENGUE_2025_SINAN %>% 
                                          filter(CS_ZONA == 2) %>% 
                                          count()), 
                Sexo_Feminino = as.integer(PR_DENGUE_2025_SINAN %>% 
                                             filter(CS_SEXO == "F") %>% 
                                             count()),
                Sexo_Masculino = as.integer(PR_DENGUE_2025_SINAN %>% 
                                              filter(CS_SEXO == "M") %>% 
                                              count()),
                Analfabeto = as.integer(PR_DENGUE_2025_SINAN %>% 
                                          filter(CS_ESCOL_N == 0) %>% 
                                          count()),
                Fundamental_Incompleto = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                      filter(CS_ESCOL_N == 1 
                                                             | 
                                                               CS_ESCOL_N == 2 
                                                             | 
                                                               CS_ESCOL_N == 3) %>%
                                                      count()),
                Fundamental = as.integer(PR_DENGUE_2025_SINAN %>% 
                                           filter(CS_ESCOL_N == 4) %>% 
                                           count()),
                Ens_Medio_Incompleto = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                    filter(CS_ESCOL_N == 5) %>% 
                                                    count()),
                Ens_Medio = as.integer(PR_DENGUE_2025_SINAN %>% 
                                         filter(CS_ESCOL_N == 6) %>% 
                                         count()),
                Ens_Superior_Incompleto = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                       filter(CS_ESCOL_N == 7) %>% 
                                                       count()),
                Ens_Superior = as.integer(PR_DENGUE_2025_SINAN %>% 
                                            filter(CS_ESCOL_N == 8) %>% 
                                            count()),
                Escolaridade_Ignorad = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                    filter(CS_ESCOL_N == 9) %>% 
                                                    count()))

AUX02 <- tibble(Menos_1_ano = as.integer(PR_DENGUE_2025_SINAN %>% 
                                           filter(NU_IDADE_N <=3012,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12) %>% 
                                           count()),
                Um_a_Cinco_Anos = as.integer(PR_DENGUE_2025_SINAN %>% 
                                               filter(NU_IDADE_N > 4000 
                                                      & 
                                                        NU_IDADE_N <=4005,
                                                      CLASSI_FIN == 10 
                                                      | 
                                                        CLASSI_FIN == 11 
                                                      |
                                                        CLASSI_FIN == 12) %>% 
                                               count()),
                Cinco_a_Doze_Anos = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                 filter(NU_IDADE_N > 4005 
                                                        & 
                                                          NU_IDADE_N <=4012,
                                                        CLASSI_FIN == 10 
                                                        | 
                                                          CLASSI_FIN == 11 
                                                        |
                                                          CLASSI_FIN == 12) %>% 
                                                 count()),
                Doze_a_Dezoito_Anos = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                   filter(NU_IDADE_N > 4012 
                                                          & 
                                                            NU_IDADE_N <=4018,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12) %>% 
                                                   count()),
                Dezoito_a_Cinq_Nove = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                   filter(NU_IDADE_N > 4018 
                                                          & 
                                                            NU_IDADE_N <= 4059,
                                                          CLASSI_FIN == 10 
                                                          | 
                                                            CLASSI_FIN == 11 
                                                          |
                                                            CLASSI_FIN == 12) %>%
                                                   count()),
                Maior_Sessenta = as.integer(PR_DENGUE_2025_SINAN %>% 
                                              filter(NU_IDADE_N > 4059,
                                                     CLASSI_FIN == 10 
                                                     | 
                                                       CLASSI_FIN == 11 
                                                     |
                                                       CLASSI_FIN == 12) %>%
                                              count()),
                Area_Urbana = as.integer(PR_DENGUE_2025_SINAN %>% 
                                           filter(CS_ZONA == 1,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12) %>% 
                                           count()),
                Area_Rural = as.integer(PR_DENGUE_2025_SINAN %>% 
                                          filter(CS_ZONA == 2,
                                                 CLASSI_FIN == 10 
                                                 | 
                                                   CLASSI_FIN == 11 
                                                 |
                                                   CLASSI_FIN == 12) %>% 
                                          count()), 
                Sexo_Feminino = as.integer(PR_DENGUE_2025_SINAN %>% 
                                             filter(CS_SEXO == "F",
                                                    CLASSI_FIN == 10 
                                                    | 
                                                      CLASSI_FIN == 11 
                                                    |
                                                      CLASSI_FIN == 12) %>% 
                                             count()),
                Sexo_Masculino = as.integer(PR_DENGUE_2025_SINAN %>% 
                                              filter(CS_SEXO == "M",
                                                     CLASSI_FIN == 10 
                                                     | 
                                                       CLASSI_FIN == 11 
                                                     |
                                                       CLASSI_FIN == 12) %>% 
                                              count()),
                Analfabeto = as.integer(PR_DENGUE_2025_SINAN %>% 
                                          filter(CS_ESCOL_N == 0,
                                                 CLASSI_FIN == 10 
                                                 | 
                                                   CLASSI_FIN == 11 
                                                 |
                                                   CLASSI_FIN == 12) %>% 
                                          count()),
                Fundamental_Incompleto = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                      filter(CS_ESCOL_N == 1 
                                                             | 
                                                               CS_ESCOL_N == 2 
                                                             | 
                                                               CS_ESCOL_N == 3,
                                                             CLASSI_FIN == 10 
                                                             | 
                                                               CLASSI_FIN == 11 
                                                             |
                                                               CLASSI_FIN == 12) %>%
                                                      count()),
                Fundamental = as.integer(PR_DENGUE_2025_SINAN %>% 
                                           filter(CS_ESCOL_N == 4,
                                                  CLASSI_FIN == 10 
                                                  | 
                                                    CLASSI_FIN == 11 
                                                  |
                                                    CLASSI_FIN == 12) %>% 
                                           count()),
                Ens_Medio_Incompleto = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                    filter(CS_ESCOL_N == 5,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12) %>% 
                                                    count()),
                Ens_Medio = as.integer(PR_DENGUE_2025_SINAN %>% 
                                         filter(CS_ESCOL_N == 6,
                                                CLASSI_FIN == 10 
                                                | 
                                                  CLASSI_FIN == 11 
                                                |
                                                  CLASSI_FIN == 12) %>% 
                                         count()),
                Ens_Superior_Incompleto = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                       filter(CS_ESCOL_N == 7,
                                                              CLASSI_FIN == 10 
                                                              | 
                                                                CLASSI_FIN == 11 
                                                              |
                                                                CLASSI_FIN == 12) %>% 
                                                       count()),
                Ens_Superior = as.integer(PR_DENGUE_2025_SINAN %>% 
                                            filter(CS_ESCOL_N == 8,
                                                   CLASSI_FIN == 10 
                                                   | 
                                                     CLASSI_FIN == 11 
                                                   |
                                                     CLASSI_FIN == 12) %>% 
                                            count()),
                Escolaridade_Ignorad = as.integer(PR_DENGUE_2025_SINAN %>% 
                                                    filter(CS_ESCOL_N == 9,
                                                           CLASSI_FIN == 10 
                                                           | 
                                                             CLASSI_FIN == 11 
                                                           |
                                                             CLASSI_FIN == 12) %>% 
                                                    count()))

##########   Escolaridade   #####

AUX_GRAF <- tibble(Rotulos = colnames(AUX01[11:18]),
                   Ordem = as.factor(1:(18-10)),
                   Notificados = t(AUX01[, 11:18]),
                   Confirmados = t(AUX02[, 11:18]))

AUX_GRAF[, 5] <- (AUX_GRAF[, 3]/ apply(AUX_GRAF[, 3], 2, sum)) *100

AUX_GRAF[, 6] <- (AUX_GRAF[, 4]/ apply(AUX_GRAF[, 4], 2, sum)) *100

colnames(AUX_GRAF)[5:6] <- c("Not", "Conf")

AUX_GRAF$Not <- as.numeric(format(round(AUX_GRAF$Not , 2))
)

AUX_GRAF$Conf <- as.numeric(format(round(AUX_GRAF$Conf , 2))
)

AUX_GRAF <- AUX_GRAF[, c(1, 2, 5:6)]

AUX_GRAF[,1] <- c("Analfabeto", "Fundamental 
Incompleto", "Fundamental", "Médio 
Incompleto", 
                  "Médio", "Superior 
Incompleto", "Superior", "Ignorado")

colnames(AUX_GRAF)[3:4] <- c("Notificados", "Confirmados")

PR_GRAF_Escolaridade <- ggplot (AUX_GRAF, 
                                aes(x = Ordem)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Percentagem de Casos",
       title = "Distribuição das Notificações por Escolaridade PARANÁ 
(2025)") +
  geom_bar(
    aes( y = Notificados, 
         fill = "Notificados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.1) + 
  scale_fill_manual(name = "", 
                    values = c("Notificados" = "#C4BF7A", 
                               "Confirmados" = "#C4A37A")) +
  geom_bar(
    aes( y = Confirmados, 
         fill = "Confirmados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = Confirmados,
                 label = Confirmados),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:(21-13)),
                   labels = AUX_GRAF$Rotulos) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 55),
        plot.title = element_text(face = "bold",
                                  size = 16,
                                  colour = "#556B2F"))

#########   ZONA de ocorrência  #####

AUX_GRAF <- tibble(Rotulos = colnames(AUX01[7:8]),
                   Ordem = as.factor(1:2),
                   Notificados = t(AUX01[, 7:8]),
                   Confirmados = t(AUX02[, 7:8]))

AUX_GRAF[, 5] <- (AUX_GRAF[, 3]/ apply(AUX_GRAF[, 3], 2, sum)) *100

AUX_GRAF[, 6] <- (AUX_GRAF[, 4]/ apply(AUX_GRAF[, 4], 2, sum)) *100

colnames(AUX_GRAF)[5:6] <- c("Not", "Conf")

AUX_GRAF$Not <- as.numeric(format(round(AUX_GRAF$Not , 2))
)

AUX_GRAF$Conf <- as.numeric(format(round(AUX_GRAF$Conf , 2))
)

AUX_GRAF <- AUX_GRAF[, c(1, 2, 5:6)]

AUX_GRAF[,1] <- c("Urbana", "Rural")

colnames(AUX_GRAF)[3:4] <- c("Notificados", "Confirmados")

PR_GRAF_Zona <- ggplot (AUX_GRAF, 
                        aes(x = Ordem)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Percentual de Casos",
       title = "Distribuição das Notificações por Zona de Ocorrência 
PARANÁ (2025)") +
  geom_bar(
    aes( y = Notificados, fill = "Notificados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = Notificados,
                 label = Notificados),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.1) + 
  scale_fill_manual(name = "",
                    values = c("Notificados" = "#614352", 
                               "Confirmados" = "#436155")) +
  geom_bar(
    aes( y = Confirmados, 
         fill = "Confirmados"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = Confirmados,
                 label = Confirmados),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.1) +
  scale_x_discrete(breaks = c(1:2),
                   labels = AUX_GRAF$Rotulos) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 0),
        plot.title = element_text(face = "bold",
                                  size = 16,
                                  colour = "#556B2F"))

#####################################################################################################################

#####     Planilhas Google Sheets. Realizando o download das planilhas do                  ######
#####     google sheets e fazendo o upload da planilha de notificações e Geral Resumida    ######

###              Upload de Notificações para posterior download da mesma planilha com 
###          as coordenadas. A planilha que irá subir para o google sheets é derivada 
###         da BASE DBF do SINAN e NÃO CONTÉM COORDENADAS. As coordenadas estão em planilha 
####             própria no google drive, preenchida pelos municípios, e é vinculada 
###                        no google sheets com esta planilha.####

gs4_auth()

RS22_2025_SINAN_DECODIFICADO$SINAN <- as.numeric(as.character(RS22_2025_SINAN_DECODIFICADO$SINAN))

sheet_write(RS22_2025_SINAN_DECODIFICADO, ss = "https://docs.google.com/spreadsheets/d/1z-cXrCe0ZRMRBG2rqW2BmG09HEa4tMmplXhHMnLbRg4/edit#gid=668044240", 
            sheet = "Registros_SINAN")

# sheet_write(PR_CHIK_2025_GERAL, ss = "https://docs.google.com/spreadsheets/d/1DNR7tyVt2tZ6ItiwJN96Bb-I4nODOmcP0G_oG0Tf_28/edit#gid=1817054379", 
#             sheet = "Chikungunya")
# 
# sheet_write(PR_DENGUE_2025_GERAL, ss = "https://docs.google.com/spreadsheets/d/1DNR7tyVt2tZ6ItiwJN96Bb-I4nODOmcP0G_oG0Tf_28/edit#gid=1817054379", 
#             sheet = "Dengue")
# 
# PR_ZIKA_2025_GERAL <- read_sheet ("https://docs.google.com/spreadsheets/d/1DNR7tyVt2tZ6ItiwJN96Bb-I4nODOmcP0G_oG0Tf_28/edit#gid=1817054379", 
#                                   sheet ="Zika")

RS22_2025_REDE_OVITRAMPAS <- read_sheet("https://docs.google.com/spreadsheets/d/1cFq9tkfZYApiOdtJHM1WrQtwIMtUMKt8H2NZ9rDyduU/edit?gid=863361484#gid=863361484", 
                                        sheet = "Consolidado")

RS22_2025_INDICES_OVITRAMPAS <- read_sheet("https://docs.google.com/spreadsheets/d/1QWxmCxl7fPiE_6wnyPLZ-B7a0yXm6NxRhFAbhGOtHeU/edit?gid=863361484#gid=863361484", 
                                           sheet = "Consolidado")

# RS22_2025_CICLOS_LOCALIDADES <- read_sheet("https://docs.google.com/spreadsheets/d/1JbvsInTDKdgRXNSIW75glk6v2abEXvDpUlcSK_PDA4E/edit?gid=764914932#gid=764914932")
# 
# ###Substituindo NA por 200 na planilha. O QGIS não está reconhecendo NA e as variáveis ficam como String no SIG.###
# 
# RS22_2025_CICLOS_LOCALIDADES[is.na(RS22_2025_CICLOS_LOCALIDADES)] <- 200
# 
# RS22_2025_CICLOS_MUNICIPIOS <- read_sheet("https://docs.google.com/spreadsheets/d/1RpgiwUAtj09LNWtLRaqRo4zUehpqhxMO7u8mFRWbkW4/edit?gid=1734395963#gid=1734395963")
# 
# ###Substituindo NA por 200 (QGIS não está reconhecendo NA. O SIG importa a planilha como character e não possibilita realizar análise dos dados de forma numérica)
# 
# RS22_2025_CICLOS_MUNICIPIOS[is.na(RS22_2025_CICLOS_MUNICIPIOS)] <- 200
# 
# #RS22_2025_CICLOS_MUNICIPIOS <- as.data.frame(lapply(RS22_2025_CICLOS_MUNICIPIOS, 
# #                                               unlist))

RS22_2025_RG_MUNICIPIOS <- read_sheet("https://docs.google.com/spreadsheets/d/1KQKPavrdg8ki2IK0nx_ZhbnrQlNuy3cY0kk_gpLUqDU/edit?gid=1585473376#gid=1585473376")

###Por alguma razão a planilha veio como lista###

#RS22_2025_RG_MUNICIPIOS <- as.data.frame(lapply(RS22_2025_RG_MUNICIPIOS, 
#                                                 unlist))

RS22_2025_RG_LOCALIDADES <- read_sheet("https://docs.google.com/spreadsheets/d/176wM7py-JuHl5gmQXnEzP_tohiNdCAItTiQ5VW1FUDk/edit?gid=877642872#gid=877642872")

RS22_2025_PE <- read_sheet("https://docs.google.com/spreadsheets/d/1JdRMkASZRTpDa9V7lBIXY5aJxlLoSp01PVQ_WmziyjA/edit?gid=863361484#gid=863361484", 
                           sheet = "Consolidado")

####Por alguma razão a planilha veio como uma lista####

#RS22_2025_PE <- as.data.frame(lapply(RS22_2025_PE, 
#                                    unlist))

RS22_2025_ASSISTENCIA <- read_sheet("https://docs.google.com/spreadsheets/d/1Xx7t478C3knk-OZAkBc3exz0Ki7vDgFA-RPy98kmpiA/edit?gid=863361484#gid=863361484",
                                    sheet = "Consolidado")

###Upload tabelas a serem usadas nos Dashboards mmunicipais

# sheet_write(RS22_2025_SE_Notificados, ss = "https://docs.google.com/spreadsheets/d/1F8Yij8Opo5lcv9mlgVh-N37hEOrI9mnKw-mc2j4kQhk/edit#gid=0", 
#             sheet = "Notificados")
# 
# sheet_write(RS22_2025_SE_Confirmados, ss = "https://docs.google.com/spreadsheets/d/1F8Yij8Opo5lcv9mlgVh-N37hEOrI9mnKw-mc2j4kQhk/edit#gid=0", 
#             sheet = "Confirmados")
# 
# sheet_write(RS22_2025_EXTRA, ss = "https://docs.google.com/spreadsheets/d/1F8Yij8Opo5lcv9mlgVh-N37hEOrI9mnKw-mc2j4kQhk/edit#gid=0", 
#             sheet = "EXTRA")
# 
# sheet_write(RS22_2025_SINAIS_Notificados, ss = "https://docs.google.com/spreadsheets/d/1F8Yij8Opo5lcv9mlgVh-N37hEOrI9mnKw-mc2j4kQhk/edit#gid=0", 
#             sheet = "Sinais_Notificados")
# 
# sheet_write(RS22_2025_SINAIS_Confirmados, ss = "https://docs.google.com/spreadsheets/d/1F8Yij8Opo5lcv9mlgVh-N37hEOrI9mnKw-mc2j4kQhk/edit#gid=0", 
#             sheet = "Sinais_Confirmados")
# 
# sheet_write(RS22_2025_GERAL, ss = "https://docs.google.com/spreadsheets/d/1F8Yij8Opo5lcv9mlgVh-N37hEOrI9mnKw-mc2j4kQhk/edit#gid=0", 
#             sheet = "GERAL")
# 
# sheet_write(RS22_2025_DOENCAS_PRE_EXISTENTES, ss = "https://docs.google.com/spreadsheets/d/1F8Yij8Opo5lcv9mlgVh-N37hEOrI9mnKw-mc2j4kQhk/edit#gid=0", 
#             sheet = "Doencas_Pre_Existentes")
# 
# ###Criando tabela Geral Resumida para utilização no Informe###
# 
# RS22_2025_Resumida <- tibble(Notificados = sum(RS22_2025_GERAL$Notificados),
#                              Dengue = sum(RS22_2025_GERAL$Dengue),
#                              DSA = sum(RS22_2025_GERAL$D_S_A),
#                              Dengue_Grave = sum(RS22_2025_GERAL$Dengue_Grave),
#                              Obitos = sum(RS22_2025_GERAL$Obitos)
# )
# 
# sheet_write(RS22_2025_Resumida, ss = "https://docs.google.com/spreadsheets/d/1bAPfOaZfUOf7ZP8-sxNLXa91YRGJ9QtnBL5cFeLpJPc/edit#gid=0", 
#             sheet = "Resumo")

#####Salvando as tabelas#####

####  Fazendo o Dowload da planilha estadual do google drive para manter controle de sorotipos igual Estado.  ###
#### foi feito upload prévio da planilha baseada no SINAN. 

#PR_DENGUE_2025_GERAL <- read_sheet("https://docs.google.com/spreadsheets/d/1DNR7tyVt2tZ6ItiwJN96Bb-I4nODOmcP0G_oG0Tf_28/edit?pli=1#gid=111480216", 
#                                    sheet= "Dengue_Estado")

#PR_DENGUE_2025_GERAL <- PR_DENGUE_2025_GERAL[, -c(22:29)]

###########Incluindo Sorotipos na Planilha RS22_2025_GERAL. Essa etapa está sendo realizada somente agora pois depende da tabela 
###########PR_2025_DENGUE_MUNICÍPIOS, a qual só foi realizado o download neste ponto do script###########

#for (i in RS22_2025_GERAL$Município){
#  RS22_2025_GERAL[which(RS22_2025_GERAL$Município  == i), 22] <- as.character(PR_DENGUE_2025_GERAL[which(PR_DENGUE_2025_GERAL$Município_sem_Código  == i), 22])
#}

write.csv(PR_DENGUE_2025_GERAL, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/PR_2025_DENGUE_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_CHIK_2025_GERAL, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/PR_2025_CHIKUNGUNYA_MUNICIPIOS.csv",
          row.names = FALSE)

# write.csv(PR_ZIKA_2025_GERAL, 
#           "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/PR_2025_ZIKA_MUNICIPIOS.csv",
#           row.names = FALSE)

write.csv(RS22_2025_REDE_OVITRAMPAS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_REDE_OVITRAMPAS.csv",
          row.names = FALSE)

# write.csv(RS22_2025_CICLOS_LOCALIDADES, 
#           "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_CICLOS_LOCALIDADES.csv",
#           row.names = FALSE)
# 
# write.csv(RS22_2025_CICLOS_MUNICIPIOS, 
#           "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_CICLOS_MUNICIPIOS.csv",
#           row.names = FALSE)

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


####     Buscando a planilha RS22_2025_SINAN_DECODIFICADO do google sheets com as coordenadas geográficas inseridas pelos municípios   ####

RS22_2025_SINAN_DECODIFICADO <- read_sheet("https://docs.google.com/spreadsheets/d/167vkgU2HLIRk2JKAr_dsxOvhu1luR60e6eMnpw9iIPI/edit?gid=1437725143#gid=1437725143", 
                                           sheet= "SINAN")

####      Gravando a planilha RS22_2025_SINAN_DECODIFICADO no diretório para ser utilizada pelo QGIS    ###

write.csv(RS22_2025_SINAN_DECODIFICADO,  "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAN_DECODIFICADO.csv",
          row.names = FALSE)

AUX <- RS22_2025_SINAN_DECODIFICADO[-which(RS22_2025_SINAN_DECODIFICADO$Classificacao_Final == "DESCARTADO"),] 

write.csv(AUX, "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/Arboviroses/RS22_2025_SINAN_PROVAVEIS_DECODIFICADO.csv",
          row.names = FALSE)

###    MAPAS      ###
###########################################################################################################
##############   Abaixo, preparando o objeto PR_DENGUE_22_23 para entrar no left-Join  ####################
##############   com o read_municipality do geobr. Futuramente, padronizar o Base_IBGE ####################
###########################################################################################################

#####   Criando o Mapa Regional Dissolvida  e Mapa Base ####

SHAPEFILE_REGIONAL <- st_read("Shapefiles/22ª_Regional_de_Saúde/22ª_Regional_de_Saúde.shp")

SHAPEFILE_REGIONAL_Dissolvido <- st_read("Shapefiles/22ª_Regional_de_Saúde/22ª_Regional_de_Saúde_Dissolvido.shp")

SHAPEFILE_ESTADUAL <- st_read("Shapefiles/Paraná/41MUE250GC_SIR.shp")

MAPA_BASE <- SHAPEFILE_ESTADUAL

MAPA_BASE$NM_MUN <- toupper(MAPA_BASE$NM_MUN)

#####   Trabalhando os Mapas de dengue estaduais   ###

PR_DENGUE_2025_GERAL[104, 2] <- "BELA VISTA DA CAROBA"
PR_DENGUE_2025_GERAL[360, 2] <- "DIAMANTE D'OESTE"
PR_DENGUE_2025_GERAL[93, 2] <- "ITAPEJARA D'OESTE"
PR_DENGUE_2025_GERAL[265, 2] <- "MUNHOZ DE MELO"
PR_DENGUE_2025_GERAL[117, 2] <- "PÉROLA D'OESTE"
PR_DENGUE_2025_GERAL[184, 2] <- "RANCHO ALEGRE D'OESTE"
PR_DENGUE_2025_GERAL[239, 2] <- "SANTA CRUZ DE MONTE CASTELO"
PR_DENGUE_2025_GERAL[127, 2] <- "SÃO JORGE D'OESTE"

####  SOROTIPOS ESTADO   ##########

MAPA_BASE_PR <- left_join(MAPA_BASE, PR_DENGUE_2025_GERAL, by = c("NM_MUN" = "Município_sem_Código"))

AUX_I <- MAPA_BASE_PR %>% filter(DENV_I > 0 & DENV_II == 0 & DENV_III == 0 & DENV_IV == 0)
AUX_II <- MAPA_BASE_PR %>% filter(DENV_I == 0 & DENV_II > 0 & DENV_III == 0 & DENV_IV == 0)
AUX_III <- MAPA_BASE_PR %>% filter(DENV_I > 0 & DENV_II > 0 & DENV_III == 0 & DENV_IV == 0)
AUX_IV <- MAPA_BASE_PR %>% filter(DENV_I > 0 & DENV_II > 0 & DENV_III > 0 & DENV_IV == 0)
AUX_V <- MAPA_BASE_PR %>% filter(DENV_I > 0 & DENV_II == 0 & DENV_III > 0 & DENV_IV == 0)
AUX_VI <- MAPA_BASE_PR %>% filter(DENV_I == 0 & DENV_II > 0 & DENV_III > 0 & DENV_IV == 0)
AUX_VII <- MAPA_BASE_PR %>% filter(DENV_I == 0 & DENV_II == 0 & DENV_III > 0 & DENV_IV == 0)

PR_2025_GRAF_SOROTIPO_PR <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          fill = "white") + 
  geom_sf(data = AUX_I, 
          color ="black", 
          aes(fill = "I")) +  
  geom_sf(data = AUX_II, 
          color ="black", 
          aes(fill = "II")) + 
  geom_sf(data = AUX_III, 
          color ="black", 
          aes(fill = "I, II")) + 
  geom_sf(data = AUX_IV, 
          color ="black", 
          aes(fill = "I, II, III")) +
  geom_sf(data = AUX_V, 
          color ="black", 
          aes(fill = "I, III")) + 
  geom_sf(data = AUX_VI, 
          color ="black", 
          aes(fill = "II, III")) + 
  geom_sf(data = AUX_VII, 
          color ="black", 
          aes(fill = "III")) + 
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme_minimal() +
  scale_fill_manual (name = "Sorotipos",
                     values = c("I" = "green", 
                                "II" = "blue",
                                "I, II" = "#2F4F4F",
                                "I, III" ="#A0522D",
                                "I, II, III" ="#00FFFF",
                                "III" = "yellow",
                                "II, III" = "red")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Sorotipo Circulante - Paraná") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 24,
                                   colour = "#556B2F")
  ) +
  geom_sf(data = SHAPEFILE_REGIONAL_Dissolvido, 
          fill = "#F0FFF0") +
  geom_sf_label(data = SHAPEFILE_REGIONAL_Dissolvido, 
                aes(label = "22 RS"),
                size = 4,
                position = "identity")

### Mapa Incidência Paraná  ####

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = Incidencia,
                                           breaks = c(-Inf, 0, 50, 100, 300, 500, 100000),
                                           labels = c("0 casos", "0,001 - 50", "50,001 - 100", 
                                                      "100,001 - 300", "300,001 - 500", ">500"))
)

PR_2025_GRAF_INCIDENCIA_PR <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme_minimal() +
  scale_fill_manual (name = "Casos/100.000 hab",
                     values = c("0 casos" = "white", 
                                "0,001 - 50" = "#FDF5E6",
                                "50,001 - 100" = "#EEE8AA",
                                "100,001 - 300" ="#FFD700",
                                "300,001 - 500" = "#DAA520",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Incidência Casos Autóctones de Dengue - Paraná",
       subtitle = "Casos Autóctones por 100.000 Habitantes") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 24,
                                   colour = "#556B2F")
  ) +
  geom_sf(data = SHAPEFILE_REGIONAL_Dissolvido, 
          fill = "#F0FFF0") +
  geom_sf_label(data = SHAPEFILE_REGIONAL_Dissolvido, 
                aes(label = "22 RS"),
                size = 4,
                position = "identity")

### Mapa Incidência Paraná  ####

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = Incidencia_Provaveis,
                                           breaks = c(-Inf, 0, 50, 100, 300, 500, 100000),
                                           labels = c("0 casos", "0,001 - 50", "50,001 - 100", 
                                                      "100,001 - 300", "300,001 - 500", ">500"))
)

PR_2025_GRAF_INCIDENCIA_PROV_PR <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme_minimal() +
  scale_fill_manual (name = "Casos/100.000 hab",
                     values = c("0 casos" = "white", 
                                "0,001 - 50" = "#FDF5E6",
                                "50,001 - 100" = "#EEE8AA",
                                "100,001 - 300" ="#FFD700",
                                "300,001 - 500" = "#DAA520",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE) +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Incidência Casos Prováveis de Dengue - Paraná",
       subtitle = "Casos Prováveis por 100.000 Habitantes
Casos Prováveis = Casos Notificados - Casos Descartados") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 24,
                                   colour = "#556B2F")
  ) +
  geom_sf(data = SHAPEFILE_REGIONAL_Dissolvido, 
          fill = "#F0FFF0") +
  geom_sf_label(data = SHAPEFILE_REGIONAL_Dissolvido, 
                aes(label = "22 RS"),
                size = 4,
                position = "identity")

#######   Chikungunya    #######

PR_CHIK_2025_GERAL$Provaveis <- as.integer(PR_CHIK_2025_GERAL$Notificados) - as.integer(PR_CHIK_2025_GERAL$Descartados)

PR_CHIK_2025_GERAL[104, 2] <- "BELA VISTA DA CAROBA"
PR_CHIK_2025_GERAL[360, 2] <- "DIAMANTE D'OESTE"
PR_CHIK_2025_GERAL[93, 2] <- "ITAPEJARA D'OESTE"
PR_CHIK_2025_GERAL[265, 2] <- "MUNHOZ DE MELO"
PR_CHIK_2025_GERAL[117, 2] <- "PÉROLA D'OESTE"
PR_CHIK_2025_GERAL[184, 2] <- "RANCHO ALEGRE D'OESTE"
PR_CHIK_2025_GERAL[239, 2] <- "SANTA CRUZ DE MONTE CASTELO"
PR_CHIK_2025_GERAL[127, 2] <- "SÃO JORGE D'OESTE"

MAPA_BASE_PR <- left_join(MAPA_BASE, PR_CHIK_2025_GERAL, by = c("NM_MUN" = "Município_sem_Código"))

###### Mapa Notificados   ############################

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = Notificados,
                                           breaks = c(-Inf, 0, 10, 50, 100, 500, 10000000),
                                           labels = c("0 casos", "1 - 10", "11 - 50", 
                                                      "51 - 100", "101 - 500", ">500"))
)

PR_2025_GRAF_CHIK_Notificados <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          aes(fill = Cat))  +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  annotation_scale(location = "br") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 24,
                                   colour = "#556B2F")) +
  scale_fill_manual (name = "Notificações", 
                     values = c("0 casos" = "white", 
                                "1 - 10" = "#FDF5E6",    
                                "11 - 50" = "#EEE8AA",
                                "51 - 100" = "#FFD700",
                                "101 - 500" = "#DAA520",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE) +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Casos Notificados de Chikungunya - Paraná",
       subtitle = "Números absolutos") +
  geom_sf(data = SHAPEFILE_REGIONAL_Dissolvido, 
          fill = "#F0FFF0") +
  geom_sf_label(data = SHAPEFILE_REGIONAL_Dissolvido, 
                aes(label = "22 RS"),
                size = 4,
                position = "identity")

###### Mapa Incidência  ############################

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = Incidencia,
                                           breaks = c(-Inf, 0, 50, 100, 300, 500, 100000),
                                           labels = c("0 casos", "0,001 - 50", "50,001 - 100", 
                                                      "100,001 - 300", "300,001 - 500", ">500")
)
)

PR_2025_GRAF_CHIK_Incidência <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          aes(fill = Cat))  +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  annotation_scale(location = "br") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 24,
                                   colour = "#556B2F")) +
  scale_fill_manual (name = "Casos/100.000 hab",
                     values = c("0 casos" = "white", 
                                "0,001 - 50" = "#FDF5E6",
                                "50,001 - 100" = "#EEE8AA",
                                "100,001 - 300" ="#FFD700",
                                "300,001 - 500" = "#DAA520",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE) +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Incidência de Chikungunya - Paraná",
       subtitle = "Casos autóctones por 100.000 hab") +
  geom_sf(data = SHAPEFILE_REGIONAL_Dissolvido, 
          fill = "#F0FFF0") +
  geom_sf_label(data = SHAPEFILE_REGIONAL_Dissolvido, 
                aes(label = "22 RS"),
                size = 4,
                position = "identity")

#####################   ZIKA VIRUS   ################################

####Elaborando for loop para criar tabela de dados gerais de notificação da 22ª RS###

PR_ZIKA_2025_GERAL <- BASE_IBGE[, -c(4, 6)]

PR_ZIKA_2025_GERAL$Notificados <- NA

PR_ZIKA_2025_GERAL$Confirmados <- NA

PR_ZIKA_2025_GERAL$Descartados <- NA

PR_ZIKA_2025_GERAL$Autoctones <- NA

PR_ZIKA_2025_GERAL$Incidencia <- NA

PR_ZIKA_2025_GERAL$Em_Investigacao <- NA

for(i in BASE_IBGE[, 2]){
  
  ###Notiicações###  
  PR_ZIKA_2025_GERAL[which(PR_ZIKA_2025_GERAL$Código_IBGE == i), 5] <- as.integer(NINDINET2025%>% 
                                                                                    filter(ID_MN_RESI == i) %>%   
                                                                                    count()
  )   
  
  ###ZIKA###
  
  PR_ZIKA_2025_GERAL[which(PR_ZIKA_2025_GERAL$Código_IBGE == i), 6] <-as.integer(NINDINET2025%>% 
                                                                                   filter(CLASSI_FIN == 1, 
                                                                                          ID_MN_RESI == i) %>%
                                                                                   count() 
  )
  
  ###Descartados###
  
  
  
  PR_ZIKA_2025_GERAL[which(PR_ZIKA_2025_GERAL$Código_IBGE == i), 7]<- as.integer(NINDINET2025%>% 
                                                                                   filter(CLASSI_FIN == 2,
                                                                                          ID_MN_RESI == i) %>% 
                                                                                   count()
  )  
  
  ###Autóctones###
  
  
  PR_ZIKA_2025_GERAL[which(PR_ZIKA_2025_GERAL$Código_IBGE == i), 8]<- as.integer(NINDINET2025%>% 
                                                                                   filter(ID_MN_RESI == i, 
                                                                                          TPAUTOCTO == 1,
                                                                                          CLASSI_FIN == 13) %>% 
                                                                                   count() 
  )
  
}

###Incidência###FORA DO LOOP###

PR_ZIKA_2025_GERAL$Incidencia <- (PR_ZIKA_2025_GERAL$Autoctones/PR_ZIKA_2025_GERAL$População)*100000  
PR_ZIKA_2025_GERAL$Incidencia <- format(round(PR_ZIKA_2025_GERAL$Incidencia, 2))
PR_ZIKA_2025_GERAL$Incidencia <- as.numeric(PR_ZIKA_2025_GERAL$Incidencia)

PR_ZIKA_2025_GERAL$Em_Investigacao <- as.integer(PR_ZIKA_2025_GERAL$Notificados) - as.integer(PR_ZIKA_2025_GERAL$Confirmados + PR_ZIKA_2025_GERAL$Descartados)

PR_ZIKA_2025_GERAL[400, 4:10] <- apply(PR_ZIKA_2025_GERAL[, 4:10], 2, sum)

##############   MAPAS ZIKA    ##########################################

PR_ZIKA_2025_GERAL[104, 3] <- "BELA VISTA DA CAROBA"
PR_ZIKA_2025_GERAL[360, 3] <- "DIAMANTE D'OESTE"
PR_ZIKA_2025_GERAL[93, 3] <- "ITAPEJARA D'OESTE"
PR_ZIKA_2025_GERAL[265, 3] <- "MUNHOZ DE MELO"
PR_ZIKA_2025_GERAL[117, 3] <- "PÉROLA D'OESTE"
PR_ZIKA_2025_GERAL[184, 3] <- "RANCHO ALEGRE D'OESTE"
PR_ZIKA_2025_GERAL[239, 3] <- "SANTA CRUZ DE MONTE CASTELO"
PR_ZIKA_2025_GERAL[127, 3] <- "SÃO JORGE D'OESTE"

MAPA_BASE_PR <- left_join(MAPA_BASE, PR_ZIKA_2025_GERAL, by = c("NM_MUN" = "Município_sem_Código"))

###### Mapa Notificados   ############################

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = Notificados,
                                           breaks = c(-Inf, 0, 10, 50, 100, 500, 100000000),
                                           labels = c("0 casos", "1 - 10", "11 - 50", 
                                                      "51 - 100", "101 - 500", ">500"))
)

PR_2025_ZIKA_CHIK_Notificados <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          aes(fill = Cat))  +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  annotation_scale(location = "br") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 24,
                                   colour = "#556B2F")) +
  scale_fill_manual (name = "Notificações", 
                     values = c("0 casos" = "white", 
                                "1 - 10" = "#FDF5E6",    
                                "11 - 50" = "#EEE8AA",
                                "51 - 100" = "#FFD700",
                                "101 - 500" = "#DAA520",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE) +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Casos Notificados de Zika - Paraná",
       subtitle = "Números absolutos") +
  geom_sf(data = SHAPEFILE_REGIONAL_Dissolvido, 
          fill = "#F0FFF0") +
  geom_sf_label(data = SHAPEFILE_REGIONAL_Dissolvido, 
                aes(label = "22 RS"),
                size = 4,
                position = "identity")

###### Mapa Incidência  ############################

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = Incidencia,
                                           breaks = c(-Inf, 0, 50, 100, 300, 500, 100000),
                                           labels = c("0 casos", "0,001 - 50", "50,001 - 100", 
                                                      "100,001 - 300", "300,001 - 500", ">500")
)
)

PR_2025_GRAF_ZIKA_Incidência <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          aes(fill = Cat))  +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  annotation_scale(location = "br") +
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 24,
                                   colour = "#556B2F")) +
  scale_fill_manual (name = "Casos/100.000 hab",
                     values = c("0 casos" = "white", 
                                "0,001 - 50" = "#FDF5E6",
                                "50,001 - 100" = "#EEE8AA",
                                "100,001 - 300" ="#FFD700",
                                "300,001 - 500" = "#DAA520",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE) +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Incidência de Zika - Paraná",
       subtitle = "Casos autóctones por 100.000 hab") +
  geom_sf(data = SHAPEFILE_REGIONAL_Dissolvido, 
          fill = "#F0FFF0") +
  geom_sf_label(data = SHAPEFILE_REGIONAL_Dissolvido, 
                aes(label = "22 RS"),
                size = 4,
                position = "identity")

#####  Mapas Regional   #####

MAPA_BASE_RS <- left_join(MAPA_BASE, RS22_2025_GERAL, 
                          by = c("NM_MUN" = "Município")
)

MAPA_BASE_RS <- MAPA_BASE_RS %>% filter(RS == RS)

AUX_I <- MAPA_BASE_RS %>%   filter(DENV_I > 0 & DENV_II == 0 & DENV_III == 0 & DENV_IV == 0)
AUX_II <- MAPA_BASE_RS %>% filter(DENV_I == 0 & DENV_II > 0 & DENV_III == 0 & DENV_IV == 0)
AUX_III <- MAPA_BASE_RS %>% filter(DENV_I > 0 & DENV_II > 0 & DENV_III == 0 & DENV_IV == 0)
AUX_IV <- MAPA_BASE_RS %>% filter(DENV_I > 0 & DENV_II > 0 & DENV_III > 0 & DENV_IV == 0)
AUX_V <- MAPA_BASE_RS %>% filter(DENV_I > 0 & DENV_II == 0 & DENV_III > 0 & DENV_IV == 0)
AUX_VI <- MAPA_BASE_RS %>% filter(DENV_I == 0 & DENV_II > 0 & DENV_III > 0 & DENV_IV == 0)

RS22_2025_GRAF_Sorotipo <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          fill = "white") + 
  geom_sf(data = AUX_I, 
          color ="black", 
          aes(fill = "I")) +  
  geom_sf(data = AUX_II, 
          color ="black", 
          aes(fill = "II")) + 
  geom_sf(data = AUX_III, 
          color ="black", 
          aes(fill = "I, II")) + 
  geom_sf(data = AUX_IV, 
          color ="black", 
          aes(fill = "I, II, III")) +
  geom_sf(data = AUX_V, 
          color ="black", 
          aes(fill = "I, III")) + 
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  theme_minimal() +
  scale_fill_manual (name = "Sorotipos",
                     values = c("I" = "green", 
                                "II" = "blue",
                                "I, II" = "#2F4F4F",
                                "I, III" ="#A0522D")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Sorotipo Circulante - 22ª Regional de Saúde") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 24,
                                   colour = "#556B2F")
  ) 

#########################################################################################################################
#########################   Mapa Chikungunya notificados REGIONAL   #####################################################

MAPA_BASE_RS <- left_join(MAPA_BASE, PR_CHIK_2025_GERAL, 
                          by = c("NM_MUN" = "Município_sem_Código")
)

MAPA_BASE_RS <- MAPA_BASE_RS %>% filter(RS == 22)

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, 
                         cut(x = Notificados,
                             breaks = c(-Inf, 0, 5, 10, 50, 100, 500, 1000000),
                             labels = c("0 casos", "1 - 5", "6 - 10", "11 - 50", 
                                        "51 - 100", "101 - 500", ">500"))
)

RS22_2025_GRAF_CHK_Not <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  coord_sf(expand = FALSE) +
  scale_fill_manual (name = "Notificações", 
                     values = c("0 casos" = "white", 
                                "1 - 5" = "#FDF5E6",
                                "6 - 10" = "#EEE8AA",    
                                "11 - 50" = "#FFD700",
                                "51 - 100" = "#DAA520",
                                "101 - 500" = "#FF8C00",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Chikungunya Casos Notificados - 22ªRS",
       subtitle = "Números absolutos") + 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 24,
                                   colour = "#556B2F")
  ) 

#########################################################################################################################
#########################   Mapa Chikungunya Confirmados REGIONAL   #####################################################

MAPA_BASE_RS <- left_join(MAPA_BASE, PR_CHIK_2025_GERAL, 
                          by = c("NM_MUN" = "Município_sem_Código")
)

MAPA_BASE_RS <- MAPA_BASE_RS %>% filter(RS == 22)

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS,
                         cut(x = Incidencia,
                             breaks = c(-Inf, 0, 50, 100, 300, 500, 100000),
                             labels = c("0 casos", "0,001 - 50", "50,001 - 100", 
                                        "100,001 - 300", "300,001 - 500", ">500")
                         )
)

RS22_2025_GRAF_CHK_Conf <- ggplot() + 
  geom_sf(data = MAPA_BASE_RS, 
          color = "black", 
          aes(fill = Cat)) + 
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl", 
                         which_north = "true") +
  theme_minimal() +
  scale_fill_manual (name = "Casos/100.000 hab",
                     values = c("0 casos" = "white", 
                                "0,001 - 50" = "#FDF5E6",
                                "50,001 - 100" = "#EEE8AA",
                                "100,001 - 300" ="#FFD700",
                                "300,001 - 500" = "#DAA520",
                                ">500" = "#DC143C")) +
  theme(legend.position = "bottom") +
  coord_sf(expand = FALSE) +
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Incidência de Chikungunya - 22ªRS",
       subtitle = "Casos autóctones por 100.000 habitantes")+ 
  theme( panel.grid.major = element_line(color = "#C0C0C0"),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F5F5F5"),
         plot.title = element_text(face = "bold", 
                                   size = 24,
                                   colour = "#556B2F")
  ) 

#########################################################################################################################

#######################################################################################################################
##################################################################################################################
#####     Salvando os Gráficos e Mapas

###      Série Histórica - Pag_04

RS22_2025_INFORME_Pag_05 <- (RS22_GRAF_Serie_Historica_Not_Conf / RS22_GRAF_Serie_Historica_Sorotipo /RS22_GRAF_Serie_Historica_Hospitalizados)

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_05.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_INFORME_Pag_05 

dev.off()

###          Canal Endêmicos Notificados/Confirmados - Pag 05

RS22_2025_INFORME_Pag_06 <- (RS_2025_GRAF_CE_Notificados / RS_2025_GRAF_CE_Provaveis / RS_2025_GRAF_CE_Confirmados)

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_06.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_INFORME_Pag_06 

dev.off()

###            Notificados/Confirmados - Pag 06

RS22_2025_INFORME_Pag_07 <- (RS22_GRAF_2025_Not_Conf / RS22_GRAF_2025_Autoctones / RS22_GRAF_2025_Investigacao)

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES//RS22_2025_INFORME_Pag_07.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_INFORME_Pag_07

dev.off()

###      Autóctones/Descartados

RS22_2025_INFORME_Pag_08 <- (RS22_GRAF_2025_Hospitalizados / RS22_GRAF_2025_Descartados / RS22_GRAF_2025_Inconclusivos)

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES//RS22_2025_INFORME_Pag_08.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_INFORME_Pag_08

dev.off()


###    Em Investigação/Incidência

RS22_2025_INFORME_Pag_09 <- (RS22_GRAF_2025_Encerramento / PR_DENGUE_2025_GRAF_SINAIS / RS22_GRAF_2025_SINAIS)

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES//RS22_2025_INFORME_Pag_09.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_INFORME_Pag_09

dev.off()

####           Incidencia - Pag 05

RS22_2025_INFORME_Pag_10 <- RS22_GRAF_2025_Incidencia

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES//RS22_2025_INFORME_Pag_10.png", 
    width = 33,
    height = 20,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_INFORME_Pag_10 

dev.off()

####           Incidencia Provaveis - Pag 05

RS22_2025_INFORME_Pag_10 <- RS22_GRAF_2025_Incidencia_Provaveis

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES//RS22_2025_INFORME_Pag_10B.png", 
    width = 33,
    height = 20,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_INFORME_Pag_10 

dev.off()
####   EXTRA  #####

RS22_2025_INFORME_PAG_11 <- ((PR_2025_GRAF_PIRAMIDE + RS_2025_GRAF_PIRAMIDE) / 
                               (PR_GRAF_Escolaridade + RS22_GRAF_Escolaridade)/
                               (PR_GRAF_Zona + RS22_GRAF_Zona))

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_11.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_INFORME_PAG_11

dev.off()

####  Canais Endêmicos IVAIPORÃ

RS_2025_CE_SEDE <- (RS_2025_GRAF_CE_Notificados_SEDE / RS_2025_GRAF_CE_Provaveis_SEDE / RS_2025_GRAF_CE_Confirmados_SEDE)

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_12.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS_2025_CE_SEDE

dev.off()

####  Canais Endêmicos JARDIM

RS_2025_CE_JARDIM <- (RS_2025_GRAF_CE_Notificados_JARDIM / RS_2025_GRAF_CE_Provaveis_JARDIM / RS_2025_GRAF_CE_Confirmados_JARDIM)

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_13.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS_2025_CE_JARDIM

dev.off()

####  Canais Endêmicos SAO JOAO

RS_2025_CE_SAO_JOAO <- (RS_2025_GRAF_CE_Notificados_SAO_JOAO_DO_IVAI / RS_2025_GRAF_CE_Provaveis_SAO_JOAO_DO_IVAI / RS_2025_GRAF_CE_Confirmados_SAO_JOAO_DO_IVAI)

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_14.png", 
    width = 33,
    height = 40,
    units = "cm", pointsize = 8, res = 300)

RS_2025_CE_SAO_JOAO

dev.off()
###      Histogramas Notificados - Pag 10

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_15.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_Histograma_Notificados_01

dev.off()

###        Histogramas Notificados - Pag 11

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_16.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_Histograma_Notificados_02

dev.off()

###      Histogramas Confirmados - ##PAG 12

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_17.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_Histograma_Confirmados_01

dev.off()

###      Histogramas Confirmados - ##PAG 13

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_18.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_Histograma_Confirmados_02

dev.off()

###     Histogramas Prováveis - ##PAG 14

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_19.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_Histograma_Provaveis_01

dev.off()

###     Histogramas Prováveis - ##PAG 15

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_20.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_Histograma_Provaveis_02

dev.off()

RS22_2025_GRAF_1 <- (PR_2025_GRAF_SOROTIPO_PR / RS22_2025_GRAF_Sorotipo)
png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_21.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

RS22_2025_GRAF_1 <- (RS22_GRAF_2025_US_TOTAL / RS22_GRAF_2025_US_DETEC)
png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_22A.png", 
    width = 36,
    height = 23,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_SORO_TOTAL / RS22_2025_GRAF_SORO_REAG)
png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_22B.png", 
    width = 36,
    height = 23,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

##### Exames Municípios  

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_23A.png", 
    width = 36,
    height = 23,
    units = "cm", pointsize = 8, res = 300)

RS22_GRAF_LACEN_MUNIC

dev.off()

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_SORO_CHIK_TOTAL / RS22_2025_GRAF_SORO_CHIK_REAG)
png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_23B.png", 
    width = 36,
    height = 23,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_24.png", 
    width = 36,
    height = 23,
    units = "cm", pointsize = 8, res = 300)

RS22_GRAF_LACEN_MUNIC_CHIK

dev.off()

RS22_2025_GRAF_1 <- (PR_2025_GRAF_INCIDENCIA_PR / PR_2025_GRAF_INCIDENCIA_PROV_PR)
png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_25.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

RS22_2025_GRAF_1 <- (PR_2025_GRAF_CHIK_Notificados / PR_2025_GRAF_CHIK_Incidência)
png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_26.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

RS22_2025_GRAF_1 <- PR_2025_GRAF_SINAIS_CHIK 
png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_27A.png", 
    width = 36,
    height = 23,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_CHK_Not + RS22_2025_GRAF_CHK_Conf) 
png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_27B.png", 
    width = 36,
    height = 23,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_28.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_CHIK_Histograma_Notificados_01

dev.off()

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_29.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_CHIK_Histograma_Notificados_02

dev.off()

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_30.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_CHIK_Histograma_Confirmados_01

dev.off()

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_31.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_CHIK_Histograma_Confirmados_02

dev.off()

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_32.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_CHIK_Histograma_Provaveis_01

dev.off()

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_33.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS_2025_GRAF_CHIK_Histograma_Provaveis_02

dev.off()

RS22_2025_GRAF_1 <- (PR_2025_ZIKA_CHIK_Notificados / PR_2025_GRAF_ZIKA_Incidência)
png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_34.png", 
    width = 36,
    height = 46,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_IPO_ARAPUA / RS22_2025_GRAF_IDO_ARAPUA / 
                       RS22_2025_GRAF_IPO_ARIRANHA / RS22_2025_GRAF_IDO_ARIRANHA  /
                       RS22_2025_GRAF_IPO_CANDIDO / RS22_2025_GRAF_IDO_CANDIDO)

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_35.png", 
    width = 46,
    height = 56,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_IPO_CRUZMALTINA / RS22_2025_GRAF_IDO_CRUZMALTINA / 
                       RS22_2025_GRAF_IPO_GODOY / RS22_2025_GRAF_IDO_GODOY / 
                       RS22_2025_GRAF_IPO_IVAIPORA / RS22_2025_GRAF_IDO_IVAIPORA )

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_36.png", 
    width = 46,
    height = 56,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_IPO_JARDIM / RS22_2025_GRAF_IDO_JARDIM / 
                       RS22_2025_GRAF_IPO_Lidianopolis / RS22_2025_GRAF_IDO_Lidianopolis / 
                       RS22_2025_GRAF_IPO_LUNARDELLI / RS22_2025_GRAF_IDO_LUNARDELLI )

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_37.png", 
    width = 46,
    height = 56,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_IPO_MANUEL_RIBAS / RS22_2025_GRAF_IDO_MANUEL_RIBAS / 
                       RS22_2025_GRAF_IPO_MATO_RICO / RS22_2025_GRAF_IDO_MATO_RICO / 
                       RS22_2025_GRAF_IPO_NOVA_TEBAS / RS22_2025_GRAF_IDO_NOVA_TEBAS )

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_38.png", 
    width = 46,
    height = 56,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_IPO_RIO_BRANCO / RS22_2025_GRAF_IDO_RIO_BRANCO / 
                       RS22_2025_GRAF_IPO_ROSARIO / RS22_2025_GRAF_IDO_ROSARIO / 
                       RS22_2025_GRAF_IPO_SANTA_MARIA / RS22_2025_GRAF_IDO_SANTA_MARIA )

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_39.png", 
    width = 46,
    height = 56,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

RS22_2025_GRAF_1 <- (RS22_2025_GRAF_IPO_SAO_JOAO / RS22_2025_GRAF_IDO_SAO_JOAO  )

png(filename = "/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/ARBOVIROSES/RS22_2025_INFORME_Pag_40.png", 
    width = 46,
    height = 14,
    units = "cm", pointsize = 8, res = 300)

RS22_2025_GRAF_1

dev.off()

##############################    Removendo Objetos desnecessários

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

