##### Tema para os Gráficos

Theme <- function(){
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
        panel.grid.major = element_line(color = "#C0C0C0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold",
                                    size = 14), 
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 24)
  )
}


#####  Construindo AUX_IPO

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Municipio" 

AUX[,1] <- c("Arapuã", "Ariranha do Ivaí", "Cândido de Abreu", "Cruzmaltina", "Godoy Moreira",
             "Ivaiporã", "Jardim Alegre", "Lidianópolis", "Lunardelli", "Manoel Ribas", "Mato Rico",
             "Nova Tebas", "Rio Branco do Ivaí", "Rosário do Ivaí", "Santa Maria do Oeste", "São João do Ivaí")

colnames (AUX)[2:54] <- c(1:53)

AUX_IPO <- RS22_2026_INDICES_OVITRAMPAS %>%
  select(Municipio, SE, IPO) %>%
  pivot_wider(names_from = SE,   
              values_from = IPO)

AUX_IPO <- AUX_IPO %>% 
  select(Municipio, matches("^[0-9]"))

AUX <- AUX %>%
  select(Municipio) %>% 
  left_join(AUX_IPO, by = "Municipio")

AUX01 <- setdiff(as.character(1:53), colnames(AUX))

AUX[AUX01] <- 0

AUX_IPO <- AUX

#####  Construindo AUX_IDO

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Municipio" 

AUX[,1] <- c("Arapuã", "Ariranha do Ivaí", "Cândido de Abreu", "Cruzmaltina", "Godoy Moreira",
             "Ivaiporã", "Jardim Alegre", "Lidianópolis", "Lunardelli", "Manoel Ribas", "Mato Rico",
             "Nova Tebas", "Rio Branco do Ivaí", "Rosário do Ivaí", "Santa Maria do Oeste", "São João do Ivaí")

colnames (AUX)[2:54] <- c(1:53)

AUX_IDO <- RS22_2026_INDICES_OVITRAMPAS %>%
  select(Municipio, SE, IDO) %>%
  pivot_wider(names_from = SE,   
              values_from = IDO)

AUX_IDO <- AUX_IDO %>% 
  select(Municipio, matches("^[0-9]"))

AUX <- AUX %>%
  select(Municipio) %>% 
  left_join(AUX_IDO, by = "Municipio")

AUX01 <- setdiff(as.character(1:53), colnames(AUX))

AUX[AUX01] <- 0

AUX_IDO <- AUX

##### Construindo AUX_IMO

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Municipio" 

AUX[,1] <- c("Arapuã", "Ariranha do Ivaí", "Cândido de Abreu", "Cruzmaltina", "Godoy Moreira",
             "Ivaiporã", "Jardim Alegre", "Lidianópolis", "Lunardelli", "Manoel Ribas", "Mato Rico",
             "Nova Tebas", "Rio Branco do Ivaí", "Rosário do Ivaí", "Santa Maria do Oeste", "São João do Ivaí")

colnames (AUX)[2:54] <- c(1:53)

AUX_IMO <- RS22_2026_INDICES_OVITRAMPAS %>%
  select(Municipio, SE, IMO) %>%
  pivot_wider(names_from = SE,   
              values_from = IMO)

AUX_IMO <- AUX_IMO %>% 
  select(Municipio, matches("^[0-9]"))

AUX <- AUX %>%
  select(Municipio) %>% 
  left_join(AUX_IMO, by = "Municipio")

AUX01 <- setdiff(as.character(1:53), colnames(AUX))

AUX[AUX01] <- 0

AUX_IMO <- AUX

#########################   Índices Ovitrampas     ########################################################

################   Arapuã   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Arapuã"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Arapuã"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Arapuã"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_ARAPUA <- ggplot(AUX_GRAF, 
                                    aes(x = as.factor(SE),
                                        y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) ARAPUÃ",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Arapuã", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_ARAPUA <- ggplot(AUX_GRAF, 
                                    aes(x = as.factor(SE)
                                    )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - ARAPUÃ",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Arapuã", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   Ariranha do Ivaí   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Ariranha do Ivaí"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Ariranha do Ivaí"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Ariranha do Ivaí"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_ARIRANHA <- ggplot(AUX_GRAF, 
                                      aes(x = as.factor(SE),
                                          y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) ARIRANHA DO IVAÍ",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Ariranha do Ivaí", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_ARIRANHA <- ggplot(AUX_GRAF, 
                                      aes(x = as.factor(SE)
                                      )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - ARIRANHA DO IVAÍ",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Ariranha do Ivaí", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   Cândido de Abreu   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Cândido de Abreu"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Cândido de Abreu"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Cândido de Abreu"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_CANDIDO <- ggplot(AUX_GRAF, 
                                     aes(x = as.factor(SE),
                                         y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) CÂNDIDO DE ABREU",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Cândido de Abreu", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_CANDIDO <- ggplot(AUX_GRAF, 
                                     aes(x = as.factor(SE)
                                     )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - CÂNDIDO DE ABREU",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Cândido de Abreu", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   Cruzmaltina   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Cruzmaltina"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Cruzmaltina"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Cruzmaltina"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_CRUZMALTINA <- ggplot(AUX_GRAF, 
                                         aes(x = as.factor(SE),
                                             y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) CRUZMALTINA",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Cruzmaltina", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_CRUZMALTINA <- ggplot(AUX_GRAF, 
                                         aes(x = as.factor(SE)
                                         )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - CRUZMALTINA",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Cruzmaltina", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   Godoy Moreira   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Godoy Moreira"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Godoy Moreira"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Godoy Moreira"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_GODOY <- ggplot(AUX_GRAF, 
                                   aes(x = as.factor(SE),
                                       y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) GODOY MOREIRA",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Godoy Moreira", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_GODOY <- ggplot(AUX_GRAF, 
                                   aes(x = as.factor(SE)
                                   )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - GODOY MOREIRA",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Godoy Moreira", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   Ivaiporã   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Ivaiporã"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Ivaiporã"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Ivaiporã"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_IVAIPORA <- ggplot(AUX_GRAF, 
                                      aes(x = as.factor(SE),
                                          y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) IVAIPORÃ",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Ivaiporã", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_IVAIPORA <- ggplot(AUX_GRAF, 
                                      aes(x = as.factor(SE)
                                      )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - IVAIPORÃ",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Ivaiporã", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   Jardim Alegre   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Jardim Alegre"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Jardim Alegre"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Jardim Alegre"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_JARDIM <- ggplot(AUX_GRAF, 
                                    aes(x = as.factor(SE),
                                        y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) JARDIM ALEGRE",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Jardim Alegre", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_JARDIM <- ggplot(AUX_GRAF, 
                                    aes(x = as.factor(SE)
                                    )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - JARDIM ALEGRE",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Jardim Alegre", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   Lidianópolis   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Lidianópolis"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Lidianópolis"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Lidianópolis"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_Lidianopolis <- ggplot(AUX_GRAF, 
                                          aes(x = as.factor(SE),
                                              y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) LIDIANÓPOLIS",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Lidianópolis", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_Lidianopolis <- ggplot(AUX_GRAF, 
                                          aes(x = as.factor(SE)
                                          )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - LIDIANÓPOLIS",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Lidianópolis", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   Lunardelli   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Lunardelli"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Lunardelli"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Lunardelli"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_LUNARDELLI <- ggplot(AUX_GRAF, 
                                        aes(x = as.factor(SE),
                                            y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) LUNARDELLI",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Lunardelli", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_LUNARDELLI <- ggplot(AUX_GRAF, 
                                        aes(x = as.factor(SE)
                                        )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - LUNARDELLI",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Lunardelli", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   Manoel Ribas   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Manoel Ribas"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Manoel Ribas"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Manoel Ribas"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_MANOEL_RIBAS <- ggplot(AUX_GRAF, 
                                          aes(x = as.factor(SE),
                                              y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) MANOEL RIBAS",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Manoel Ribas", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_MANOEL_RIBAS <- ggplot(AUX_GRAF, 
                                          aes(x = as.factor(SE)
                                          )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - MANOEL RIBAS",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Manoel Ribas", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   Mato Rico   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Mato Rico"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Mato Rico"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Mato Rico"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_MATO_RICO <- ggplot(AUX_GRAF, 
                                       aes(x = as.factor(SE),
                                           y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) MATO RICO",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Mato Rico", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_MATO_RICO <- ggplot(AUX_GRAF, 
                                       aes(x = as.factor(SE)
                                       )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - MATO RICO",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Mato Rico", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   Nova Tebas   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Nova Tebas"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Nova Tebas"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Nova Tebas"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_NOVA_TEBAS <- ggplot(AUX_GRAF, 
                                        aes(x = as.factor(SE),
                                            y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) NOVA TEBAS",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Nova Tebas", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_NOVA_TEBAS <- ggplot(AUX_GRAF, 
                                        aes(x = as.factor(SE)
                                        )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - NOVA TEBAS",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Nova Tebas", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   Rio Branco do Ivaí   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Rio Branco do Ivaí"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Rio Branco do Ivaí"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Rio Branco do Ivaí"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_RIO_BRANCO <- ggplot(AUX_GRAF, 
                                        aes(x = as.factor(SE),
                                            y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) RIO BRANCO DO IVAÍ",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Rio Branco do Ivaí", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_RIO_BRANCO <- ggplot(AUX_GRAF, 
                                        aes(x = as.factor(SE)
                                        )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - RIO BRANCO DO IVAÍ",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Rio Branco do Ivaí", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   Rosário do Ivaí   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Rosário do Ivaí"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Rosário do Ivaí"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Rosário do Ivaí"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_ROSARIO <- ggplot(AUX_GRAF, 
                                     aes(x = as.factor(SE),
                                         y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) ROSÁRIO DO IVAÍ",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Rosário do Ivaí", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_ROSARIO <- ggplot(AUX_GRAF, 
                                     aes(x = as.factor(SE)
                                     )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - ROSÁRIO DO IVAÍ",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Rosário do Ivaí", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   Santa Maria do Oeste   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "Santa Maria do Oeste"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "Santa Maria do Oeste"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "Santa Maria do Oeste"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_SANTA_MARIA <- ggplot(AUX_GRAF, 
                                         aes(x = as.factor(SE),
                                             y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) SANTA MARIA DO OESTE",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Santa Maria do Oeste", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_SANTA_MARIA <- ggplot(AUX_GRAF, 
                                         aes(x = as.factor(SE)
                                         )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - SANTA MARIA DO OESTE",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "Santa Maria do Oeste", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

#########################   Índices Ovitrampas     ########################################################

################   São João do Ivaí   #####

####  IDO

AUX_GRAF <- t(AUX_IPO[which(AUX_IPO == "São João do Ivaí"),])

AUX_GRAF <- AUX_GRAF[-1,]

AUX_GRAF <- as.data.frame(AUX_GRAF)

AUX_GRAF$SE <- rownames(AUX_GRAF)

AUX_GRAF$SE <- factor(as.numeric(AUX_GRAF$SE), 
                      levels = sort(unique(as.numeric(AUX_GRAF$SE))))

colnames(AUX_GRAF)[1] <- "IPO"

####  IDO

AUX <- t(AUX_IDO[which(AUX_IDO == "São João do Ivaí"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IDO"

AUX_GRAF$IDO <- AUX$IDO

####  IMO

AUX <- t(AUX_IMO[which(AUX_IMO == "São João do Ivaí"),])

AUX <- AUX[-1,]

AUX <- as.data.frame(AUX)

AUX$SE <- rownames(AUX)

AUX$SE <- factor(as.numeric(AUX$SE), 
                 levels = sort(unique(as.numeric(AUX$SE))))

colnames(AUX)[1] <- "IMO"

AUX_GRAF$IMO <- AUX$IMO

AUX_GRAF <- AUX_GRAF[!is.na(AUX_GRAF$IPO),]

RS22_2026_GRAF_IPO_SAO_JOAO <- ggplot(AUX_GRAF, 
                                      aes(x = as.factor(SE),
                                          y = as.numeric(IPO))) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IPO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) SÃO JOÃO DO IVAÍ",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "São João do Ivaí", 2]), 
                               collapse = ", "))
  ) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

RS22_2026_GRAF_IDO_SAO_JOAO <- ggplot(AUX_GRAF, 
                                      aes(x = as.factor(SE)
                                      )) +
  geom_bar(aes(y = as.numeric(IMO)),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = as.numeric(IMO), 
                 label = as.numeric(IMO)),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = as.numeric(IDO),
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = as.numeric(IDO)),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = as.numeric(IDO),
                 label = as.numeric(IDO)), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2,
       x = "Semana Epidemiológica",
       y = "IDO (Linha)/IMO (Coluna)",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - SÃO JOÃO DO IVAÍ",
       subtitle = paste0("SE Trabalhadas: ", 
                         paste(unlist(RS22_2026_INDICES_OVITRAMPAS[RS22_2026_INDICES_OVITRAMPAS$Municipio == "São João do Ivaí", 2]), 
                               collapse = ", "))) +
  Theme() +
  scale_y_discrete(expand = expansion(mult = c(0, 0.1))) 

