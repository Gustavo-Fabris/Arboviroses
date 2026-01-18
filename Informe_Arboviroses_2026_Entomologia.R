##################################   Entomologia   ######################################################################
#########################   Índices Ovitrampas     ########################################################

################   Arapuã   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Arapuã"),]

RS22_2026_GRAF_IPO_ARAPUA <- ggplot(AUX_GRAF, 
                                    aes(x = as.factor(SE),
                                        y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) ARAPUÃ") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_ARAPUA <- ggplot(AUX_GRAF, 
                                    aes(x = as.factor(SE)
                                    )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - ARAPUÃ",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

################   Ariranha do Ivaí   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Ariranha do Ivaí"),]

RS22_2026_GRAF_IPO_ARIRANHA <- ggplot(AUX_GRAF, 
                                      aes(x = as.factor(SE),
                                          y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) ARIRANHA DO IVAÍ") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_ARIRANHA <- ggplot(AUX_GRAF, 
                                      aes(x = as.factor(SE)
                                      )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - ARIRANHA DO IVAÍ",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

################   Cândido de Abreu   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Cândido de Abreu"),]

RS22_2026_GRAF_IPO_CANDIDO <- ggplot(AUX_GRAF, 
                                     aes(x = as.factor(SE),
                                         y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) CÂNDIDO DE ABREU") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_CANDIDO <- ggplot(AUX_GRAF, 
                                     aes(x = as.factor(SE)
                                     )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - CÂNDIDO DE ABREU",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))


################   Cruzmaltina   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Cruzmaltina"),]

RS22_2026_GRAF_IPO_CRUZMALTINA <- ggplot(AUX_GRAF, 
                                         aes(x = as.factor(SE),
                                             y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) CRUZMALTINA") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_CRUZMALTINA <- ggplot(AUX_GRAF, 
                                         aes(x = as.factor(SE)
                                         )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - CRUZMALTINA",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

################   Godoy Moreira   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Godoy Moreira"),]

RS22_2026_GRAF_IPO_GODOY <- ggplot(AUX_GRAF, 
                                   aes(x = as.factor(SE),
                                       y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) GODOY MOREIRA") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_GODOY <- ggplot(AUX_GRAF, 
                                   aes(x = as.factor(SE)
                                   )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - GODOY MOREIRA",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

################   Ivaiporã   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Ivaiporã"),]

RS22_2026_GRAF_IPO_IVAIPORA <- ggplot(AUX_GRAF, 
                                      aes(x = as.factor(SE),
                                          y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) IVAIPORÃ") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_IVAIPORA <- ggplot(AUX_GRAF, 
                                      aes(x = as.factor(SE)
                                      )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - IVAIPORÃ",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

################   Jardim Alegre   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Jardim Alegre"),]

RS22_2026_GRAF_IPO_JARDIM <- ggplot(AUX_GRAF, 
                                    aes(x = as.factor(SE),
                                        y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) JARDIM ALEGRE") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_JARDIM <- ggplot(AUX_GRAF, 
                                    aes(x = as.factor(SE)
                                    )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - JARDIM ALEGRE",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

################   Lidianópolis   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Lidianópolis"),]

RS22_2026_GRAF_IPO_Lidianopolis <- ggplot(AUX_GRAF, 
                                          aes(x = as.factor(SE),
                                              y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) LIDIANÓPOLIS") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_Lidianopolis <- ggplot(AUX_GRAF, 
                                          aes(x = as.factor(SE)
                                          )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - LIDIANÓPOLIS",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

################   Lunardelli   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Lunardelli"),]

RS22_2026_GRAF_IPO_LUNARDELLI <- ggplot(AUX_GRAF, 
                                        aes(x = as.factor(SE),
                                            y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) LUNARDELLI") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_LUNARDELLI <- ggplot(AUX_GRAF, 
                                        aes(x = as.factor(SE)
                                        )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - LUNARDELLI",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

################   Manoel Ribas   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Manoel Ribas"),]

RS22_2026_GRAF_IPO_MANUEL_RIBAS <- ggplot(AUX_GRAF, 
                                          aes(x = as.factor(SE),
                                              y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(label = IPO), 
             size = 3, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IPO (%)",
       title = "Índice de Positividade de Ovitrampas (%) MANOEL RIBAS") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_MANUEL_RIBAS <- ggplot(AUX_GRAF, 
                                          aes(x = as.factor(SE)
                                          )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - MANOEL RIBAS",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

################   Mato RIco     #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Mato Rico"),]

RS22_2026_GRAF_IPO_MATO_RICO <- ggplot(AUX_GRAF, 
                                       aes(x = as.factor(SE),
                                           y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) MATO RICO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_MATO_RICO <- ggplot(AUX_GRAF, 
                                       aes(x = as.factor(SE)
                                       )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - MATO RICO",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

################   Nova Tebas   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Nova Tebas"),]

RS22_2026_GRAF_IPO_NOVA_TEBAS <- ggplot(AUX_GRAF, 
                                        aes(x = as.factor(SE),
                                            y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) NOVA TEBAS") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_NOVA_TEBAS <- ggplot(AUX_GRAF, 
                                        aes(x = as.factor(SE)
                                        )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - NOVA TEBAS",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

################   Rio Branco do Ivaí   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Rio Branco do Ivaí"),]

RS22_2026_GRAF_IPO_RIO_BRANCO <- ggplot(AUX_GRAF, 
                                        aes(x = as.factor(SE),
                                            y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) RIO BRANCO DO IVAÍ") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_RIO_BRANCO <- ggplot(AUX_GRAF, 
                                        aes(x = as.factor(SE)
                                        )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - RIO BRANCO DO IVAÍ",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

################   Rosário do Ivaí   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Rosário do Ivaí"),]

RS22_2026_GRAF_IPO_ROSARIO <- ggplot(AUX_GRAF, 
                                     aes(x = as.factor(SE),
                                         y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) ROSÁRIO DO IVAÍ") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_ROSARIO <- ggplot(AUX_GRAF, 
                                     aes(x = as.factor(SE)
                                     )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - ROSÁRIO DO IVAÍ",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))


################   Santa Maria do Oeste   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "Santa Maria do Oeste"),]

RS22_2026_GRAF_IPO_SANTA_MARIA <- ggplot(AUX_GRAF, 
                                         aes(x = as.factor(SE),
                                             y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) SANTA MARIA DO OESTE") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_SANTA_MARIA <- ggplot(AUX_GRAF, 
                                         aes(x = as.factor(SE)
                                         )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - SANTA MARIA DO OESTE",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

################   São João do Ivaí   #####

AUX_GRAF <- RS22_2026_INDICES_OVITRAMPAS[which(RS22_2026_INDICES_OVITRAMPAS[, 1] == "São João do Ivaí"),]

RS22_2026_GRAF_IPO_SAO_JOAO <- ggplot(AUX_GRAF, 
                                      aes(x = as.factor(SE),
                                          y = IPO)) +
  geom_line(aes(x = as.factor(SE),
                y = IPO,
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
       title = "Índice de Positividade de Ovitrampas (%) SÃO JOÃO DO IVAÍ") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

RS22_2026_GRAF_IDO_SAO_JOAO <- ggplot(AUX_GRAF, 
                                      aes(x = as.factor(SE)
                                      )) +
  geom_bar(aes(y = IMO),
           stat = "identity",
           linewidth = 1.8,
           colour = "black") +
  geom_label(aes(y = IMO, 
                 label = IMO),
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  geom_line(aes(x = as.factor(SE),
                y = IDO,
                group = 1),
            linewidth = 1.8,
            colour = "black") +
  geom_point(aes(x = as.factor(SE),
                 y = IDO),
             fill = "grey",
             size = 7,
             shape = 21) +
  geom_label(aes(x = as.factor(SE),
                 y = IDO,
                 label = IDO), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.1) +
  labs(caption = Fonte_2, 
       x = "Semana Epidemiológica",
       y = "IDO/IMO",
       title = "Índice de Densidade de Ovos/Índice Médio de Ovos - SÃO JOÃO DO IVAÍ",
       subtitle = "Coluna = IMO
Linha = IDO") +
  Theme() +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 1,
                                   face = "bold"),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        axis.text.y = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5,
                                   face = "bold"),
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))
