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
  Theme()

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
  Theme() 

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
  theme(axis.text.x = element_text(angle = 55))

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
  scale_fill_manual(name = "", values = c("Notificados" = "#614352", 
                                          "Confirmados" = "#436155")) +
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
  theme(axis.text.x = element_text(angle = 0)
        )

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
    aes( y = DENV_I, 
         fill = "DENV I"),
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

AUX_GRAF <- data.frame(Sintomas = c("Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor nas 
Costas", "Conjuntivite", "Artrite", "Artralgia 
Intensa", "Petequias", "Leucopenia", "Prova do Laco 
Positiva", "Dor 
Retroorbital"),
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
    labs(caption = Fonte, 
       x = "Sintomas",
       y = "Percentual de Casos",
       title = "Distribuição das Notificações por Sinais/Sintomas PARANÁ - 2025",
       subtitle = "Sinais/Sintomas Clínicos em Notificações de CHIKUNGUNYA Assinalados no Campo 33 da Ficha do SINAN") +
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  Theme()


AUX_GRAF <- data.frame(Sintomas = c("Febre", "Mialgia", "Cefaleia", "Exantema", 
                                    "Vômito", "Náusea", "Dor nas 
Costas", "Conjuntivite", 
                                    "Artrite", "Artralgia 
Intensa", "Petéquias", "Leucopenia", 
                                    "Prova do Laco 
Positiva", "Dor 
Retroorbital"),
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
Intensa", "Petéquias", "Leucopenia", "Prova do Laço 
Positiva", "Dor 
Retroorbital")

######  Construção do Gráfico de SInais Estadual   #####

PR_DENGUE_2025_GRAF_SINAIS <- ggplot (AUX_GRAF, 
                                      aes(x = Sintomas)) + 
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  Theme()

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
  theme(axis.text.x = element_text(angle = 55))

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
  Theme() 

#######################################################
######     DENGUE  Municipais   ###################
#######################################################
AUX_SEM <- c("1",  "2", "3", 
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

###       NOTIFICADOS     ########

RS_2025_SE_Notificados[nrow(RS_2025_SE_Notificados) +1, 2:ncol(RS_2025_SE_Notificados)] <-  AUX_SEM

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

RS_2025_SE_Confirmados[nrow(RS_2025_SE_Confirmados) +1, 2:ncol(RS_2025_SE_Confirmados)] <- AUX_SEM

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

RS_2025_SE_Provaveis[nrow(RS_2025_SE_Provaveis) +1, 2:ncol(RS_2025_SE_Provaveis)] <- AUX_SEM

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

#######################################################
######     Chikungunya              ###################
#######################################################

###       NOTIFICADOS     ########
RS_2025_SE_Notificados_CHIK <- RS_2025_SE_Notificados_CHIK[, -54]

# RS_2025_SE_Notificados_CHIK[nrow(RS_2025_SE_Notificados_CHIK) +1, ] <- colnames(RS_2025_SE_Notificados_CHIK)

RS_2025_SE_Notificados_CHIK[nrow(RS_2025_SE_Notificados_CHIK) +1, 2:ncol(RS_2025_SE_Notificados_CHIK)] <- AUX_SEM

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

RS_2025_SE_Confirmados_CHIK[nrow(RS_2025_SE_Confirmados_CHIK) +1, 2:ncol(RS_2025_SE_Confirmados_CHIK)] <- AUX_SEM

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
RS_2025_SE_Provaveis_CHIK[nrow(RS_2025_SE_Provaveis_CHIK) +1, 2:ncol(RS_2025_SE_Provaveis_CHIK)] <- AUX_SEM

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

