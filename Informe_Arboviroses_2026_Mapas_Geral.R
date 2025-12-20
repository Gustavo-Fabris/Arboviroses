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
