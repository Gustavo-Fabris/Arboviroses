##########################################################################################################################################
###################################   Vigilância Laboratorial  ###########################################################################

###########################################################################################################################################
###############################  Buscando CSV com dados do LACEN   ########################################################################
###########################################################################################################################################

RS22_2026_LACEN_PESQ_ARBO <- read.csv("/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/LACEN/Arboviroses/LACEN_PESQUISA_ARBOVIRUS_2026.csv",
                                      header = TRUE,
                                      sep = ",")

# RS22_2026_LACEN_ZDC <- read.csv("/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/LACEN/Arboviroses/LACEN_PESQUISA_ARBOVIRUS_ZDC_LACEN_2026.csv",
#                                 header = TRUE,
#                                 sep = ",")

#######################    Manipulando dados para criação da coluna SE   #########################################################

### stringr para separar data de hora da coluna Dt_Cadastro ####

AUX <- as.data.frame(str_split(RS22_2026_LACEN_PESQ_ARBO$Dt_Cadastro, 
                               pattern = " ")
)

AUX <- t(AUX)

colnames(AUX)<- c("Data", "Hora")

AUX <- as.data.frame(AUX)

AUX$Data <- dmy(AUX$Data)

RS22_2026_LACEN_PESQ_ARBO$Dt_Cadastro <- AUX$Data

###### Lubridate  para criar SE a partir de darta ######

RS22_2026_LACEN_PESQ_ARBO$SE <- epiweek(AUX$Data)


####################  uSANDO STRINGR PARA ACENTUAR A TABELA DO lacen TORNANDO-A COMPATÍVEL COM A base_ibge   ###################################

RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia, "ARAPUA", "ARAPUÃ")

RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia, "ARIRANHA DO IVAI", "ARIRANHA DO IVAÍ")

RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia, "CANDIDO DE ABREU", "CÂNDIDO DE ABREU")

RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia, "IVAIPORA", "IVAIPORÃ")

RS22_2026_LACEN_PESQ_ARBO$Municipio_Requisitante <- str_replace(RS22_2026_LACEN_PESQ_ARBO$Municipio_Requisitante, "IVAIPORA", "IVAIPORÃ")

RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia, "LIDIANOPOLIS", "LIDIANÓPOLIS")

RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia, "RIO BRANCO DO IVAI", "RIO BRANCO DO IVAÍ")

RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia, "ROSARIO DO IVAI", "ROSÁRIO DO IVAÍ")

RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia <- str_replace(RS22_2026_LACEN_PESQ_ARBO$Municipio_Residencia, "SAO JOAO DO IVAI", "SÃO JOÃO DO IVAÍ")

####################   Transformando colunas em fatores para o for loop funcionar   ##################################

RS22_2026_LACEN_PESQ_ARBO[, 9] <- as.factor(RS22_2026_LACEN_PESQ_ARBO[, 9])

RS22_2026_LACEN_PESQ_ARBO[, 23] <- as.factor(RS22_2026_LACEN_PESQ_ARBO[, 23])

RS22_2026_LACEN_PESQ_ARBO[, 24] <- as.factor(RS22_2026_LACEN_PESQ_ARBO[, 24])

###########################################################################################################################################
###############################  Buscando CSV com dados do LACEN   ########################################################################
###########################################################################################################################################

RS22_2026_LACEN_SOROLOGIA <- read.csv("/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/LACEN/Arboviroses/LACEN_SOROLOGIA_2026.csv",
                                      header = TRUE,
                                      sep = ",")

#######################    Manipulando dados para criação da coluna SE   #########################################################

### stringr para separar data de hora da coluna Dt_Cadastro ####

AUX <- as.data.frame(str_split(RS22_2026_LACEN_SOROLOGIA$Dt_Cadastro, 
                               pattern = " ")
)

AUX <- t(AUX)

colnames(AUX)<- c("Data", "Hora")

AUX <- as.data.frame(AUX)

AUX$Data <- dmy(AUX$Data)

RS22_2026_LACEN_SOROLOGIA$Dt_Cadastro <- AUX$Data

###### Lubridate  para criar SE a partir de data ######

RS22_2026_LACEN_SOROLOGIA$SE <- epiweek(AUX$Data)

####################  USANDO STRINGR PARA ACENTUAR A TABELA DO lacen TORNANDO-A COMPATÍVEL COM A base_ibge   ###################################

RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia, "ARAPUA", "ARAPUÃ")

RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia, "ARIRANHA DO IVAI", "ARIRANHA DO IVAÍ")

RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia, "CANDIDO DE ABREU", "CÂNDIDO DE ABREU")

RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia, "IVAIPORA", "IVAIPORÃ")

RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia, "LIDIANOPOLIS", "LIDIANÓPOLIS")

RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia, "RIO BRANCO DO IVAI", "RIO BRANCO DO IVAÍ")

RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia, "ROSARIO DO IVAI", "ROSÁRIO DO IVAÍ")

RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA$Municipio_Residencia, "SAO JOAO DO IVAI", "SÃO JOÃO DO IVAÍ")

####################   Transformando colunas em fatores para o for loop funcionar   ##################################

RS22_2026_LACEN_SOROLOGIA[, 9] <- as.factor(RS22_2026_LACEN_SOROLOGIA[, 9])

RS22_2026_LACEN_SOROLOGIA[, 24] <- as.factor(RS22_2026_LACEN_SOROLOGIA[, 24])

RS22_2026_LACEN_SOROLOGIA[, 25] <- as.factor(RS22_2026_LACEN_SOROLOGIA[, 25])

#######################################  LACE CHIKUNGUNYA  #############################################################################
###########################################################################################################################################

RS22_2026_LACEN_SOROLOGIA_CHIK <- read.csv("/home/gustavo/Área de trabalho/Análise_de_Dados/Base_de_Dados/LACEN/Arboviroses/LACEN_SOROLOGIA_CHIK_2026.csv",
                                           header = TRUE,
                                           sep = ",")

#######################    Manipulando dados para criação da coluna SE   #########################################################

### stringr para separar data de hora da coluna Dt_Cadastro ####

AUX <- as.data.frame(str_split(RS22_2026_LACEN_SOROLOGIA_CHIK$Dt_Cadastro, 
                               pattern = " ")
)

AUX <- t(AUX)

colnames(AUX)<- c("Data", "Hora")

AUX <- as.data.frame(AUX)

AUX$Data <- dmy(AUX$Data)

RS22_2026_LACEN_SOROLOGIA_CHIK$Dt_Cadastro <- AUX$Data

###### Lubridate  para criar SE a partir de data ######

RS22_2026_LACEN_SOROLOGIA_CHIK$SE <- epiweek(AUX$Data)

####################  USANDO STRINGR PARA ACENTUAR A TABELA DO lacen TORNANDO-A COMPATÍVEL COM A base_ibge   ###################################

RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia, "ARAPUA", "ARAPUÃ")

RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia, "ARIRANHA DO IVAI", "ARIRANHA DO IVAÍ")

RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia, "CANDIDO DE ABREU", "CÂNDIDO DE ABREU")

RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia, "IVAIPORA", "IVAIPORÃ")

RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia, "LIDIANOPOLIS", "LIDIANÓPOLIS")

RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia, "RIO BRANCO DO IVAI", "RIO BRANCO DO IVAÍ")

RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia, "ROSARIO DO IVAI", "ROSÁRIO DO IVAÍ")

RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia <- str_replace(RS22_2026_LACEN_SOROLOGIA_CHIK$Municipio_Residencia, "SAO JOAO DO IVAI", "SÃO JOÃO DO IVAÍ")

####################   Transformando colunas em fatores para o for loop funcionar   ##################################

RS22_2026_LACEN_SOROLOGIA_CHIK[, 9] <- as.factor(RS22_2026_LACEN_SOROLOGIA_CHIK[, 9])

RS22_2026_LACEN_SOROLOGIA_CHIK[, 24] <- as.factor(RS22_2026_LACEN_SOROLOGIA_CHIK[, 24])

RS22_2026_LACEN_SOROLOGIA_CHIK[, 25] <- as.factor(RS22_2026_LACEN_SOROLOGIA_CHIK[, 25])

############################################################################################################################################
#####################  Realizando a contagem de exames por SE PESQ ARBO GERAL  #############################################################

RS22_2026_SE_PESQ_ARB <- matrix(data = NA, 
                                nrow = nrow, 
                                ncol = 54)

RS22_2026_SE_PESQ_ARB <- as.data.frame(RS22_2026_SE_PESQ_ARB)

colnames(RS22_2026_SE_PESQ_ARB)[1] <- "Município" 

RS22_2026_SE_PESQ_ARB[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

colnames (RS22_2026_SE_PESQ_ARB)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 3]){
    RS22_2026_SE_PESQ_ARB[which(RS22_2026_SE_PESQ_ARB == i), O] <- as.integer(RS22_2026_LACEN_PESQ_ARBO%>%
                                                                                filter(Municipio_Residencia == i,
                                                                                       SE == N,
                                                                                       Status_Exame == "Resultado Liberado" |
                                                                                         Status_Exame == "Automaçăo em Processamento" |
                                                                                         Status_Exame == "Disponivel para Encaminhar" |
                                                                                         Status_Exame == "Aguardando Triagem" |
                                                                                         Status_Exame == "Exame Aprovado. Aguardando Automaçăo") %>%
                                                                                count()
    )
  }
  
  N <- N +1
  O <- O +1
}

RS22_2026_SE_PESQ_ARB[(nrow(RS22_2026_SE_PESQ_ARB)+1),2:54] <- apply(RS22_2026_SE_PESQ_ARB[,2:54], 2, sum)

RS22_2026_SE_PESQ_ARB[nrow(RS22_2026_SE_PESQ_ARB),1] <- "Total"

############################################################################################################################################
#####################  Realizando a contagem de exames por SE PESQ ARBO  DETECTÁVEIS GERAL  #############################################################

RS22_2026_SE_PESQ_ARB_DETECTAVEL <- matrix(data = NA, 
                                           nrow = nrow, 
                                           ncol = 54)

RS22_2026_SE_PESQ_ARB_DETECTAVEL <- as.data.frame(RS22_2026_SE_PESQ_ARB_DETECTAVEL)

colnames(RS22_2026_SE_PESQ_ARB_DETECTAVEL)[1] <- "Município" 

RS22_2026_SE_PESQ_ARB_DETECTAVEL[,1] <- BASE_IBGE[which(BASE_IBGE$RS == 22), 3]

colnames (RS22_2026_SE_PESQ_ARB_DETECTAVEL)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[(which(BASE_IBGE$RS == 22)), 3]){
    RS22_2026_SE_PESQ_ARB_DETECTAVEL[which(RS22_2026_SE_PESQ_ARB_DETECTAVEL == i), O] <- as.integer(RS22_2026_LACEN_PESQ_ARBO%>%
                                                                                                      filter(Municipio_Residencia == i,
                                                                                                             SE == N,
                                                                                                             Resultado == "Detectável ") %>%
                                                                                                      count()
    )
  }
  
  N <- N +1
  O <- O +1
}

RS22_2026_SE_PESQ_ARB_DETECTAVEL[(nrow(RS22_2026_SE_PESQ_ARB_DETECTAVEL)+1),2:54] <- apply(RS22_2026_SE_PESQ_ARB_DETECTAVEL[,2:54], 2, sum)

RS22_2026_SE_PESQ_ARB_DETECTAVEL[nrow(RS22_2026_SE_PESQ_ARB_DETECTAVEL),1] <- "Total"

############################################################################################################################################
#####################  Realizando a contagem de exames por SE UNIDADE SENTINELA #############################################################

RS22_2026_SE_US <- matrix(data = NA, 
                          nrow = nrow, 
                          ncol = 54)

RS22_2026_SE_US <- as.data.frame(RS22_2026_SE_US)

colnames(RS22_2026_SE_US)[1] <- "Município" 

RS22_2026_SE_US[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

colnames (RS22_2026_SE_US)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 3]){
    RS22_2026_SE_US[which(RS22_2026_SE_US == i), O] <- as.integer(RS22_2026_LACEN_PESQ_ARBO%>%
                                                                    filter(Municipio_Requisitante == i,
                                                                           SE == N,
                                                                           Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                           Status_Exame == "Resultado Liberado" |
                                                                             Status_Exame == "Automaçăo em Processamento" |
                                                                             Status_Exame == "Disponivel para Encaminhar" |
                                                                             Status_Exame == "Exame Aprovado. Aguardando Automaçăo") %>% 
                                                                    count()
    )
  }
  
  N <- N +1
  O <- O +1
}

RS22_2026_SE_US[(nrow(RS22_2026_SE_US)+1),2:54] <- apply(RS22_2026_SE_US[,2:54], 2, sum)

RS22_2026_SE_US[nrow(RS22_2026_SE_US),1] <- "Total"


############################################################################################################################################
#####################  Realizando a contagem de exames por SE UNIDADE SENTINELA DETECTÁVEL #############################################################

RS22_2026_SE_US_DETECTAVEL <- matrix(data = NA, 
                                     nrow = nrow, 
                                     ncol = 54)

RS22_2026_SE_US_DETECTAVEL <- as.data.frame(RS22_2026_SE_US_DETECTAVEL)

colnames(RS22_2026_SE_US_DETECTAVEL)[1] <- "Município" 

RS22_2026_SE_US_DETECTAVEL[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

colnames (RS22_2026_SE_US_DETECTAVEL)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 3]){
    RS22_2026_SE_US_DETECTAVEL[which(RS22_2026_SE_US_DETECTAVEL == i), O] <- as.integer(RS22_2026_LACEN_PESQ_ARBO%>%
                                                                                          filter(Municipio_Requisitante == i,
                                                                                                 SE == N,
                                                                                                 Requisitante == "POSTO DE SAUDE CENTRAL DE IVAIPORA",
                                                                                                 Resultado == "Detectável ") %>%
                                                                                          count()
    )
  }
  
  N <- N +1
  O <- O +1
}

RS22_2026_SE_US_DETECTAVEL[(nrow(RS22_2026_SE_US_DETECTAVEL)+1), 2:54] <- apply(RS22_2026_SE_US_DETECTAVEL[, 2:54], 
                                                                                2, 
                                                                                sum)

RS22_2026_SE_US_DETECTAVEL[nrow(RS22_2026_SE_US_DETECTAVEL), 1] <- "Total"

############################################################################################################################################
#####################  Realizando a contagem de exames por SE SOROLOGIA GERAL  #############################################################

RS22_2026_SE_SOROLOGIA <- matrix(data = NA, 
                                 nrow = nrow, 
                                 ncol = 54)

RS22_2026_SE_SOROLOGIA <- as.data.frame(RS22_2026_SE_SOROLOGIA)

colnames(RS22_2026_SE_SOROLOGIA)[1] <- "Município" 

RS22_2026_SE_SOROLOGIA[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

colnames (RS22_2026_SE_SOROLOGIA)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 3]){
    RS22_2026_SE_SOROLOGIA[which(RS22_2026_SE_SOROLOGIA == i), O] <- as.integer(RS22_2026_LACEN_SOROLOGIA%>%
                                                                                  filter(Municipio_Residencia == i,
                                                                                         SE == N,
                                                                                         Status_Exame == "Resultado Liberado" |
                                                                                           Status_Exame == "Automaçăo em Processamento" |
                                                                                           Status_Exame == "Exame em Análise" |
                                                                                           Status_Exame == "Disponível para Encaminhar" |
                                                                                           Status_Exame == "Aguardando Triagem") %>% 
                                                                                  count()
    )
  }
  
  N <- N +1
  O <- O +1
}

RS22_2026_SE_SOROLOGIA[(nrow(RS22_2026_SE_SOROLOGIA) +1), 2:54] <- apply(RS22_2026_SE_SOROLOGIA[, 2:54], 
                                                                         2, 
                                                                         sum)

RS22_2026_SE_SOROLOGIA[nrow(RS22_2026_SE_SOROLOGIA),1] <- "Total"

############################################################################################################################################
#####################  Realizando a contagem de exames por SE SOROLOGIA REAGENTES GERAL  #############################################################

RS22_2026_SE_SOROLOGIA_REAGENTE <- matrix(data = NA, 
                                          nrow = nrow, 
                                          ncol = 54)

RS22_2026_SE_SOROLOGIA_REAGENTE <- as.data.frame(RS22_2026_SE_SOROLOGIA_REAGENTE)

colnames(RS22_2026_SE_SOROLOGIA_REAGENTE)[1] <- "Município" 

RS22_2026_SE_SOROLOGIA_REAGENTE[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

colnames (RS22_2026_SE_SOROLOGIA_REAGENTE)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 3]){
    RS22_2026_SE_SOROLOGIA_REAGENTE[which(RS22_2026_SE_SOROLOGIA_REAGENTE == i), O] <- as.integer(RS22_2026_LACEN_SOROLOGIA%>%
                                                                                                    filter(Municipio_Residencia == i,
                                                                                                           SE == N,
                                                                                                           Resultado == "Reagente " |
                                                                                                             Resultado == "Reagente") %>%
                                                                                                    count()
    )
  }
  
  N <- N +1
  O <- O +1
}

RS22_2026_SE_SOROLOGIA_REAGENTE[, 13] <- as.numeric(RS22_2026_SE_SOROLOGIA_REAGENTE[, 13])

RS22_2026_SE_SOROLOGIA_REAGENTE[(nrow(RS22_2026_SE_SOROLOGIA_REAGENTE) +1), 2:54] <- apply(RS22_2026_SE_SOROLOGIA_REAGENTE[, 2:54], 
                                                                                           2, 
                                                                                           sum)

RS22_2026_SE_SOROLOGIA_REAGENTE[nrow(RS22_2026_SE_SOROLOGIA_REAGENTE),1] <- "Total"

##########################LACEN CHIKUNGUNYA   ############################################################
############################################################################################################################################
#####################  Realizando a contagem de exames por SE SOROLOGIA GERAL  #############################################################

RS22_2026_SE_SOROLOGIA_CHIK <- matrix(data = NA, 
                                      nrow = nrow, 
                                      ncol = 54)

RS22_2026_SE_SOROLOGIA_CHIK <- as.data.frame(RS22_2026_SE_SOROLOGIA_CHIK)

colnames(RS22_2026_SE_SOROLOGIA_CHIK)[1] <- "Município" 

RS22_2026_SE_SOROLOGIA_CHIK[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

colnames (RS22_2026_SE_SOROLOGIA_CHIK)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 3]){
    RS22_2026_SE_SOROLOGIA_CHIK[which(RS22_2026_SE_SOROLOGIA_CHIK == i), O] <- as.integer(RS22_2026_LACEN_SOROLOGIA_CHIK%>%
                                                                                            filter(Municipio_Residencia == i,
                                                                                                   SE == N,
                                                                                                   Status_Exame == "Resultado Liberado" |
                                                                                                     Status_Exame == "Automaçăo em Processamento" |
                                                                                                     Status_Exame == "Exame em Análise" |
                                                                                                     Status_Exame == "Disponível para Encaminhar" |
                                                                                                     Status_Exame == "Aguardando Triagem") %>% 
                                                                                            count()
    )
  }
  
  N <- N +1
  O <- O +1
}

RS22_2026_SE_SOROLOGIA_CHIK[(nrow(RS22_2026_SE_SOROLOGIA_CHIK) +1), 2:54] <- apply(RS22_2026_SE_SOROLOGIA_CHIK[, 2:54], 
                                                                                   2, 
                                                                                   sum)

RS22_2026_SE_SOROLOGIA_CHIK[nrow(RS22_2026_SE_SOROLOGIA_CHIK),1] <- "Total"

############################################################################################################################################
#####################  Realizando a contagem de exames por SE SOROLOGIA REAGENTES GERAL  #############################################################

RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK <- matrix(data = NA, 
                                               nrow = nrow, 
                                               ncol = 54)

RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK <- as.data.frame(RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK)

colnames(RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK)[1] <- "Município" 

RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

colnames (RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 3]){
    RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK[which(RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK == i), O] <- as.integer(RS22_2026_LACEN_SOROLOGIA_CHIK%>%
                                                                                                              filter(Municipio_Residencia == i,
                                                                                                                     SE == N,
                                                                                                                     Resultado == "Reagente " |
                                                                                                                       Resultado == "Reagente") %>%
                                                                                                              count()
    )
  }
  
  N <- N +1
  O <- O +1
}

RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK[, 13] <- as.numeric(RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK[, 13])

RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK[(nrow(RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK) +1), 2:54] <- apply(RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK[, 2:54], 
                                                                                                     2, 
                                                                                                     sum)

RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK[nrow(RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK),1] <- "Total"

#############################################################################################################################
#############################################################################################################################
#########################    Elaboração dos Gráficos para inserção no Informe  ##############################################
#############################################################################################################################

#################   Gráfico de amostras encaminhadas pela U.S.   ###########################################################

AUX <- RS22_2026_SE_US[6,]

AUX[2, ] <- colnames(RS22_2026_SE_PESQ_ARB)

AUX <- AUX[c(2, 1),]

AUX <- as.data.frame(t(AUX))

colnames(AUX) <- AUX[1, ]

AUX <- AUX[-1, ]

AUX[, 2] <- as.numeric(AUX[, 2])

AUX$Sem_EPI <-as.numeric(AUX$Município)

AUX <- AUX[-nrow(AUX), ]

RS22_GRAF_2026_US_TOTAL <- ggplot(AUX, aes(x = Sem_EPI, y = IVAIPORÃ))  + 
  labs(caption = Fonte_1, 
       x = "Semana Epidemiológica",
       y = "Número de Amostras Encaminhadas",
       title = "Quantidade de Amostras Encaminhadas/SE - Unidade Sentinela") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#EEE8AA") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX$Sem_EPI) +
  Theme() +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14))

#######  Separando a quantidade de casos detectáveis da U.S.   ##########################

AUX_2 <- as.data.frame(RS22_2026_SE_US_DETECTAVEL[6, ])

AUX_2 <- AUX_2[, -1]

AUX_2 <- t(AUX_2)

AUX_2 <- AUX_2[-nrow(AUX_2),]

AUX$US_DETEC <- AUX_2

colnames(AUX)[4] <- "US_DETEC"

#####  Criando coluna com porcentagem de casos detectáveis   #####

AUX$PORC_US_DETEC <- (AUX$US_DETEC/AUX$IVAIPORÃ) * 100

AUX$PORC_US_DETEC[which(is.nan(AUX$PORC_US_DETEC), 5)] <- 0

AUX$PORC_US_DETEC <- format(round(AUX$PORC_US_DETEC, 2))

AUX$PORC_US_DETEC <- as.numeric(AUX$PORC_US_DETEC)

#############  Criando gráfico com dados de detectáveis  ##############

RS22_GRAF_2026_US_DETEC <-  ggplot(AUX, aes(x = Sem_EPI, y = PORC_US_DETEC))  + 
  labs(caption = Fonte_1, 
       x = "Semana Epidemiológica",
       y = "% de Amostras Encaminhadas Positivas",
       title = "Taxa de Positividade/SE - Unidade Sentinela") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#EEE8AA") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.001))
  ) +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX$Sem_EPI) +
  Theme() +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14))

################################################################################################################
######################  Amostras encaminhadas para o LACEN (sorologia e pesq. de arbovírus)  ###################

AUX <- RS22_2026_SE_SOROLOGIA[17,]

AUX[2, ] <- colnames(RS22_2026_SE_SOROLOGIA)

AUX <- AUX[c(2, 1),]

AUX <- as.data.frame(t(AUX))

colnames(AUX) <- AUX[1, ]

AUX <- AUX[-1, ]

AUX[, 2] <- as.numeric(AUX[, 2])

AUX$Sem_EPI <-as.numeric(AUX$Município)

AUX <- AUX[-nrow(AUX), ]

RS22_2026_GRAF_SORO_TOTAL <- ggplot(AUX, aes(x = Sem_EPI, y = Total))  + 
  labs(caption = Fonte_1, 
       x = "Semana Epidemiológica",
       y = "Número de Amostras",
       title = "Quantidade de Amostras (Sorologia) Encaminhadas/SE - 22ª RS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#9ad2b0") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX$Sem_EPI) +
  Theme() +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14))

#######  Amostras Encaminhadas ao LACEN que tiveram resultado REAGENTE ENZIMAIMUNOENSAIO   ##################
#############################################################################################################

AUX <- as.data.frame(RS22_2026_SE_SOROLOGIA_REAGENTE[17, ])

AUX[2, ] <- colnames(RS22_2026_SE_SOROLOGIA)

AUX <- AUX[c(2, 1),]

AUX <- as.data.frame(t(AUX))

colnames(AUX) <- AUX[1, ]

AUX <- AUX[-1, ]

AUX[, 2] <- as.numeric(AUX[, 2])

AUX$Sem_EPI <-as.numeric(AUX$Município)

AUX <- AUX[, -1]

colnames(AUX)[1] <- "Amostras"

AUX$Total <- t(RS22_2026_SE_SOROLOGIA[17, 2:54])

colnames(AUX)[3] <- "Total"

#####  Criando coluna com porcentagem de casos detectáveis   #####

AUX$PORC_SORO_REAG <- (AUX$Amostras/AUX$Total) * 100

AUX$PORC_SORO_REAG[which(is.nan(AUX$PORC_SORO_REAG), 5)] <- 0

AUX$PORC_USORO_REAG <- format(round(AUX$PORC_SORO_REAG, 2))

AUX$PORC_SORO_REAG <- as.numeric(AUX$PORC_SORO_REAG)

AUX <- AUX[-nrow(AUX), ]

#############  Criando gráfico com dados de detectáveis  ##############

RS22_2026_GRAF_SORO_REAG <- ggplot(AUX, aes(x = Sem_EPI, y = PORC_SORO_REAG))  + 
  labs(caption = Fonte_1, 
       x = "Semana Epidemiológica",
       y = "% de Amostras Encaminhadas Reagentes",
       title = "Taxa de Positividade para Dengue (Sorologia)/SE - 22ª RS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#9ad2b0") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.001))) +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX$Sem_EPI) +
  Theme() +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14))

#####################################################################################################################
####################  LACEN - MUNICÍPIOS   ##########################################################################
#####################################################################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sorologia <- NA

AUX$Sorologia_Reag <- NA

AUX$Pesq_Arb <- NA

AUX$Pesq_Arb_Detec <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 3]){
  
  ###Sorologia/Município###  
  AUX[which(AUX$Município == i), 5] <- as.integer(RS22_2026_LACEN_SOROLOGIA %>% 
                                                    filter(Municipio_Residencia == i) %>%   
                                                    count()
  )   
  
  ###Sorologia reagente/Município##  
  AUX[which(AUX$Município == i), 6] <- as.integer(RS22_2026_LACEN_SOROLOGIA %>% 
                                                    filter(Municipio_Residencia == i,
                                                           Resultado == "Reagente ") %>%   
                                                    count()
  )
  
  ###Pesquisa arbovírus/Município##  
  AUX[which(AUX$Município == i), 7] <- as.integer(RS22_2026_LACEN_PESQ_ARBO %>% 
                                                    filter(Municipio_Residencia == i) %>%   
                                                    count()
  )
  
  ###Pesquisa arbovírus detectável/Município##  
  AUX[which(AUX$Município == i), 8] <- as.integer(RS22_2026_LACEN_PESQ_ARBO %>% 
                                                    filter(Municipio_Residencia == i,
                                                           Resultado == "Detectável") %>%   
                                                    count()
  )
}

AUX$ENCAMINHADAS <- (AUX$Sorologia)

AUX$POSITIVAS <- (AUX$Sorologia_Reag)

RS22_GRAF_LACEN_MUNIC <- ggplot (AUX, 
                                 aes(x = Município)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Amostras",
       title = "Amostras de Dengue Encaminhadas/Positivas - 22ªRS",
       subtitle = "Amostras de sorologia") +
  geom_bar(
    aes( y = ENCAMINHADAS, fill = "ENCAMINHADAS"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = ENCAMINHADAS,
                 label = ENCAMINHADAS),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.1) + 
  scale_fill_manual(name = "", 
                    values = c("ENCAMINHADAS" = "#556B2F", 
                               "POSITIVAS" = "#FF6347")) +
  geom_bar(
    aes( y = POSITIVAS, 
         fill = "POSITIVAS"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = POSITIVAS,
                 label = POSITIVAS),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.1) +
  Theme() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(axis.text.x = element_text(angle = 75,
                                   vjust = .5,
                                   face = "bold",
                                   size = 14))

################################################################################################################
######################  Amostras encaminhadas para o LACEN (sorologia chikungunya)  ###################

AUX <- RS22_2026_SE_SOROLOGIA_CHIK[17,]

AUX[2, ] <- colnames(RS22_2026_SE_SOROLOGIA_CHIK)

AUX <- AUX[c(2, 1),]

AUX <- as.data.frame(t(AUX))

colnames(AUX) <- AUX[1, ]

AUX <- AUX[-1, ]

AUX[, 2] <- as.numeric(AUX[, 2])

AUX$Sem_EPI <-as.numeric(AUX$Município)

AUX <- AUX[-nrow(AUX), ]

RS22_2026_GRAF_SORO_CHIK_TOTAL <- ggplot(AUX, 
                                         aes(x = Sem_EPI, 
                                             y = Total))  + 
  labs(caption = Fonte_1, 
       x = "Semana Epidemiológica",
       y = "Número de Amostras",
       title = "Quantidade de Amostras de Chikungunya (Sorologia) Encaminhadas/SE - 22ª RS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#9ad2b0") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX$Sem_EPI) +
  Theme() +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14))

#######  Amostras Encaminhadas ao LACEN que tiveram resultado REAGENTE ENZIMAIMUNOENSAIO   ##################
#############################################################################################################

AUX <- as.data.frame(RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK[17, ])

AUX[2, ] <- colnames(RS22_2026_SE_SOROLOGIA_CHIK)

AUX <- AUX[c(2, 1),]

AUX <- as.data.frame(t(AUX))

colnames(AUX) <- AUX[1, ]

AUX <- AUX[-1, ]

AUX[, 2] <- as.numeric(AUX[, 2])

AUX$Sem_EPI <-as.numeric(AUX$Município)

AUX <- AUX[, -1]

colnames(AUX)[1] <- "Amostras"

AUX$Total <- t(RS22_2026_SE_SOROLOGIA_CHIK[17, 2:54])

colnames(AUX)[3] <- "Total"

#####  Criando coluna com porcentagem de casos detectáveis   #####

AUX$PORC_SORO_REAG <- (AUX$Amostras/AUX$Total) * 100

AUX$PORC_SORO_REAG[which(is.nan(AUX$PORC_SORO_REAG), 5)] <- 0

AUX$PORC_USORO_REAG <- format(round(AUX$PORC_SORO_REAG, 2))

AUX$PORC_SORO_REAG <- as.numeric(AUX$PORC_SORO_REAG)

AUX <- AUX[-nrow(AUX), ]

#############  Criando gráfico com dados de detectáveis  ##############

RS22_2026_GRAF_SORO_CHIK_REAG <- ggplot(AUX, aes(x = Sem_EPI, y = PORC_SORO_REAG))  + 
  labs(caption = Fonte_1, 
       x = "Semana Epidemiológica",
       y = "% de Amostras Encaminhadas Reagentes",
       title = "Taxa de Positividade para Chikungunya (Sorologia)/SE - 22ª RS") +
  geom_bar(stat = "identity",
           color = "black",
           fill = "#9ad2b0") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.001))) +
  scale_x_continuous(breaks = c(1:52), 
                     label = AUX$Sem_EPI) +
  Theme() +
  theme(axis.text.x = element_text(angle = 0, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14))

#####################################################################################################################
####################  LACEN - MUNICÍPIOS   ##########################################################################
#####################################################################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Sorologia <- NA

AUX$Sorologia_Reag <- NA

AUX$Pesq_Arb <- NA

AUX$Pesq_Arb_Detec <- NA

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 3]){
  
  ###Sorologia/Município###  
  AUX[which(AUX$Município == i), 5] <- as.integer(RS22_2026_LACEN_SOROLOGIA_CHIK %>% 
                                                    filter(Municipio_Residencia == i) %>%   
                                                    count()
  )   
  
  ###Sorologia reagente/Município##  
  AUX[which(AUX$Município == i), 6] <- as.integer(RS22_2026_LACEN_SOROLOGIA_CHIK %>% 
                                                    filter(Municipio_Residencia == i,
                                                           Resultado == "Reagente ") %>%   
                                                    count()
  )
  
  ###Pesquisa arbovírus/Município##  
  AUX[which(AUX$Município == i), 7] <- as.integer(RS22_2026_LACEN_PESQ_ARBO %>% 
                                                    filter(Municipio_Residencia == i) %>%   
                                                    count()
  )
  
  ###Pesquisa arbovírus detectável/Município##  
  AUX[which(AUX$Município == i), 8] <- as.integer(RS22_2026_LACEN_PESQ_ARBO %>% 
                                                    filter(Municipio_Residencia == i,
                                                           Resultado == "Detectável") %>%   
                                                    count()
  )
}

AUX$ENCAMINHADAS <- (AUX$Sorologia)

AUX$POSITIVAS <- (AUX$Sorologia_Reag)

RS22_GRAF_LACEN_MUNIC_CHIK <- ggplot (AUX, 
                                      aes(x = Município)) + 
  labs(caption = Fonte, 
       x = NULL,
       y = "Número de Amostras",
       title = "AMOSTRAS de Chikungunya (Sorologia) ENCAMINHADAS/POSITIVAS - 22ªRS") +
  geom_bar(
    aes( y = ENCAMINHADAS,
         fill = "ENCAMINHADAS"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = -.20)) + 
  geom_label(aes(y = ENCAMINHADAS,
                 label = ENCAMINHADAS),
             size = 3, 
             alpha = 0.5,
             nudge_x = -.20,
             vjust = 0.1) + 
  scale_fill_manual(name = "", 
                    values = c("ENCAMINHADAS" = "#556B2F", 
                               "POSITIVAS" = "#FF6347")) +
  geom_bar(
    aes( y = POSITIVAS, 
         fill = "POSITIVAS"),
    stat = "identity",
    color = "black",
    width = .4,
    position = position_nudge(x = .20)) +
  geom_label(aes(y = POSITIVAS,
                 label = POSITIVAS),
             size = 3, 
             alpha = 0.5,
             nudge_x = .20,
             vjust = 0.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  Theme() +
  theme(axis.text.x = element_text(angle = 85, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14))

#### Taxa de Positividade Dengue MUNICÍPIOS

AUX <- (RS22_2026_SE_SOROLOGIA_REAGENTE[, c(2:54)]/RS22_2026_SE_SOROLOGIA[, c(2:54)])*100

AUX[is.na(AUX)] <- 0

AUX <- format(round(AUX))

AUX[ncol(AUX) +1 ] <- RS22_2026_SE_SOROLOGIA[,1]

AUX <- AUX[, c(ncol(AUX), 1: (ncol(AUX) -1))]

AUX_GRAF <- as.data.frame(AUX[, 1])

AUX_GRAF[, 2] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(AUX[, which(colnames(AUX) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(AUX)[which(colnames(AUX) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(AUX)[which(colnames(AUX) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(AUX)[which(colnames(AUX) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(AUX)[which(colnames(AUX) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(AUX)[which(colnames(AUX) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(AUX)[which(colnames(AUX) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(AUX)[which(colnames(AUX) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(AUX)[which(colnames(AUX) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(AUX)[which(colnames(AUX) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(AUX)[which(colnames(AUX) == SE)]

AUX_GRAF[nrow(AUX_GRAF), ] <- colnames(AUX_GRAF)

AUX_GRAF <- AUX_GRAF[c(nrow(AUX_GRAF), 1:(nrow(AUX_GRAF) -1)), ]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF[, 2: ncol(AUX_GRAF)] <- apply(AUX_GRAF[, 2: ncol(AUX_GRAF)], 2, as.numeric)

rownames(AUX_GRAF) <- 1: nrow(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

AUX <- AUX_GRAF %>%
  pivot_longer(-SE, names_to = "Municipios") %>% 
  mutate(
    SE = SE
  ) %>%
  group_split(Municipios) %>% 
  lapply(function(dados) {
    titulo <- dados$Municipios %>% 
      unique() %>% 
      paste0(" - DENGUE")
    ggplot(dados, aes(x = SE, 
                      y = value)
    ) + 
      geom_col(color = "black", 
               fill = "#DAA520") + 
      geom_label(aes(label = value), 
                 alpha = 0.5, 
                 vjust = 0.1) +
      labs(
        caption = Fonte_1, 
        x = "Semana Epidemiológica",
        y = "Taxa de Positividade",
        title = titulo
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      Theme_Hist()
  })

RS_2026_GRAF_Taxa_Positividade_Dengue_Mun <- (AUX[[1]] + AUX[[2]]) / 
  (AUX[[3]] + AUX[[4]]) / 
  (AUX[[5]] + AUX[[6]]) / 
  (AUX[[7]] + AUX[[8]]) /
  (AUX[[9]] + AUX[[10]]) / 
  (AUX[[11]] + AUX[[12]]) / 
  (AUX[[13]] + AUX[[14]]) / 
  (AUX[[15]] + AUX[[16]])

#### Taxa de Positividade Chikungunya MUNICÍPIOS

AUX <- (RS22_2026_SE_SOROLOGIA_REAGENTE_CHIK[, c(2:54)]/RS22_2026_SE_SOROLOGIA_CHIK[, c(2:54)])*100

AUX[is.na(AUX)] <- 0

AUX <- format(round(AUX))

AUX[ncol(AUX) +1 ] <- RS22_2026_SE_SOROLOGIA_CHIK[,1]

AUX <- AUX[, c(ncol(AUX), 1: (ncol(AUX) -1))]

AUX_GRAF <- as.data.frame(AUX[, 1])

AUX_GRAF[, 2] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 9])
AUX_GRAF[, 3] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 8])
AUX_GRAF[, 4] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 7])
AUX_GRAF[, 5] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 6])
AUX_GRAF[, 6] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 5])
AUX_GRAF[, 7] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 4])
AUX_GRAF[, 8] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 3])
AUX_GRAF[, 9] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 2])
AUX_GRAF[, 10] <- as.data.frame(AUX[, which(colnames(AUX) == SE) - 1])
AUX_GRAF[, 11] <- as.data.frame(AUX[, which(colnames(AUX) == SE)])

colnames(AUX_GRAF)[1] <- "Municipios"
colnames(AUX_GRAF)[2] <- colnames(AUX)[which(colnames(AUX) == SE) - 9]
colnames(AUX_GRAF)[3] <- colnames(AUX)[which(colnames(AUX) == SE) - 8]
colnames(AUX_GRAF)[4] <- colnames(AUX)[which(colnames(AUX) == SE) - 7]
colnames(AUX_GRAF)[5] <- colnames(AUX)[which(colnames(AUX) == SE) - 6]
colnames(AUX_GRAF)[6] <- colnames(AUX)[which(colnames(AUX) == SE) - 5]
colnames(AUX_GRAF)[7] <- colnames(AUX)[which(colnames(AUX) == SE) - 4]
colnames(AUX_GRAF)[8] <- colnames(AUX)[which(colnames(AUX) == SE) - 3]
colnames(AUX_GRAF)[9] <- colnames(AUX)[which(colnames(AUX) == SE) - 2]
colnames(AUX_GRAF)[10] <- colnames(AUX)[which(colnames(AUX) == SE) - 1]
colnames(AUX_GRAF)[11] <- colnames(AUX)[which(colnames(AUX) == SE)]

AUX_GRAF[nrow(AUX_GRAF), ] <- colnames(AUX_GRAF)

AUX_GRAF <- AUX_GRAF[c(nrow(AUX_GRAF), 1:(nrow(AUX_GRAF) -1)), ]

AUX_GRAF <- t(AUX_GRAF)

colnames(AUX_GRAF) <- AUX_GRAF[1,]

AUX_GRAF <- AUX_GRAF[-1,]

colnames(AUX_GRAF)[1] <- "SE"

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF[, 2: ncol(AUX_GRAF)] <- apply(AUX_GRAF[, 2: ncol(AUX_GRAF)], 2, as.numeric)

rownames(AUX_GRAF) <- 1: nrow(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

AUX <- AUX_GRAF %>%
  pivot_longer(-SE, names_to = "Municipios") %>% 
  mutate(
    SE = SE
  ) %>%
  group_split(Municipios) %>% 
  lapply(function(dados) {
    titulo <- dados$Municipios %>% 
      unique() %>% 
      paste0(" - CHIKUNGUNYA")
    ggplot(dados, aes(x = SE, 
                      y = value)
    ) + 
      geom_col(color = "black", 
               fill = "#DAA520") + 
      geom_label(aes(label = value), 
                 alpha = 0.5, 
                 vjust = 0.1) +
      labs(
        caption = Fonte_1, 
        x = "Semana Epidemiológica",
        y = "Taxa de Positividade",
        title = titulo
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      Theme_Hist()
  })

RS_2026_GRAF_Taxa_Positividade_Chik_Mun <- (AUX[[1]] + AUX[[2]]) / 
  (AUX[[3]] + AUX[[4]]) / 
  (AUX[[5]] + AUX[[6]]) / 
  (AUX[[7]] + AUX[[8]]) /
  (AUX[[9]] + AUX[[10]]) / 
  (AUX[[11]] + AUX[[12]]) / 
  (AUX[[13]] + AUX[[14]]) / 
  (AUX[[15]] + AUX[[16]])
