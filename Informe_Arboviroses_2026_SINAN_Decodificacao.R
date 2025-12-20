#######################################################################################
####       Trabalhando com a tabela RS22_SINAN do período atual. Realizando a    ######
####      decodificação dos fatores em linguagem mais acessível aos municípios   ######
#######################################################################################

AUX <- rbind(SINAN_DENGUE_RS,
             SINAN_CHIK_RS)

AUX$ID_AGRAVO <- factor(AUX$ID_AGRAVO,
                        label = c("Dengue", "Chikungunya"), 
                        levels = c("A90", "A92.0")
)

###Sintomas###
AUX$FEBRE <- factor(AUX$FEBRE,
                    label = c("SIM", "NÃO"), 
                    levels = c(1, 2)
)

AUX$MIALGIA <- factor(AUX$MIALGIA,
                      label = c("SIM", "NÃO"), 
                      levels = c(1, 2)
)

AUX$CEFALEIA <- factor(AUX$CEFALEIA,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$EXANTEMA <- factor(AUX$EXANTEMA,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$VOMITO <- factor(AUX$VOMITO,
                     label = c("SIM", "NÃO"), 
                     levels = c(1, 2)
)

AUX$NAUSEA <- factor(AUX$NAUSEA,
                     label = c("SIM", "NÃO"), 
                     levels = c(1, 2)
)

AUX$DOR_COSTAS <- factor(AUX$DOR_COSTAS,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$DOR_RETRO <- factor(AUX$DOR_RETRO,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$CONJUNTVIT <- factor(AUX$CONJUNTVIT,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ARTRALGIA <- factor(AUX$ARTRALGIA,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$ARTRITE <- factor(AUX$ARTRITE,
                      label = c("SIM", "NÃO"), 
                      levels = c(1, 2)
)

AUX$PETEQUIA_N <- factor(AUX$PETEQUIA_N,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$LEUCOPENIA <- factor(AUX$LEUCOPENIA,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$LACO <- factor(AUX$LACO,
                   label = c("SIM", "NÃO"), 
                   levels = c(1, 2)
)

###Doenças Pré-existentes

AUX$DIABETES <- factor(AUX$DIABETES,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$HEMATOLOG <- factor(AUX$HEMATOLOG,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$HEPATOPAT <- factor(AUX$HEPATOPAT,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$RENAL <- factor(AUX$RENAL,
                    label = c("SIM", "NÃO"), 
                    levels = c(1, 2)
)

AUX$HIPERTENSA <- factor(AUX$HIPERTENSA,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ACIDO_PEPT <- factor(AUX$ACIDO_PEPT,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$AUTO_IMUNE <- factor(AUX$AUTO_IMUNE,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

####Outros####

AUX$CS_GESTANT <- factor(AUX$CS_GESTANT,
                         label = c("1_TRI", "2_TRI", "3_TRI", "IDADE GESTACIONAL IGN", "NÃO", "NÃO SE APLICA", "IGNORADO"), 
                         levels = c(1, 2, 3, 4, 5, 6, 9)
)

AUX$CS_ESCOL_N <- factor(AUX$CS_ESCOL_N,
                         label = c("ANALFABETO", "1 a 4 SÉRIE DO FUNDAMENTAL INCOMPLETA", "4 SÉRIE DO FUNDAMENTAL COMPLETA", "5 a 8 SÉRIE DO FUNDAMENTAL INCOMPLETA", "FUNDAMENTAL COMPLETO", "ENSINO MÉDIO INCOMPLETO", "ENSINO MÉDIO COMPLETO", "SUPERIOR INCONPLETO", "SUPERIOR COMPLETO", "IGNORADO", "NÃO SE APLICA"), 
                         levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)

AUX$RESUL_SORO <- factor(AUX$RESUL_SORO,
                         label = c("REAGENTE", "NÃO REAGENTE", "INCONCLUSIVO", "NÃO REALIZADO"), 
                         levels = c(1, 2, 3, 4)
)


AUX$RESUL_PCR_ <- factor(AUX$RESUL_PCR_,
                         label = c("DETECTÁVEL", "NÃO DETECTÀVEL", "INCONCLUSIVO", "NÃO REALIZADO"), 
                         levels = c(1, 2, 3, 4)
)

AUX$SOROTIPO <- factor(AUX$SOROTIPO,
                       label = c("I", "II", "III", "IV"), 
                       levels = c(1, 2, 3, 4)
)

AUX$CLASSI_FIN <- factor(AUX$CLASSI_FIN,
                         label = c("DESCARTADO", "INCONCLUSIVO", "DENGUE", "D.S.A.", "IDENGUE_GRAVE", "CHIKUNGUNYA"), 
                         levels = c(5, 8, 10, 11, 12, 13)
)

AUX$CRITERIO <- factor(AUX$CRITERIO,
                       label = c("LABORATORIAL", "CLÍNICO-EPIDEMIOLÓGICO", "EM INVESTIGAÇÃO"), 
                       levels = c(1, 2, 3)
)

AUX$TPAUTOCTO <- factor(AUX$TPAUTOCTO,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$HOSPITALIZ <- factor(AUX$HOSPITALIZ,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$EVOLUCAO <- factor(AUX$EVOLUCAO,
                       label = c("CURA", "ÓBITO PELO AGRAVO", "ÓBITO POR OUTRAS CAUSAS","ÓBITO EM INVESTIGAÇÃO", "INDETERMINADO"), 
                       levels = c(1, 2, 3, 4, 9)
)

AUX$CS_ZONA <- factor(AUX$CS_ZONA,
                      label = c("URBANA", "RURAL", "PERIURBANA","INDETERMINADO"), 
                      levels = c(1, 2, 3, 9)
)

AUX$ALRM_LETAR <- factor(AUX$ALRM_LETAR,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ALRM_HEPAT <- factor(AUX$ALRM_HEPAT,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ALRM_LIQ <- factor(AUX$ALRM_LIQ,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$ALRM_HIPOT <- factor(AUX$ALRM_HIPOT,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ALRM_PLAQ <- factor(AUX$ALRM_PLAQ,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$ALRM_VOM <- factor(AUX$ALRM_VOM,
                       label = c("SIM", "NÃO"), 
                       levels = c(1, 2)
)

AUX$ALRM_SANG <- factor(AUX$ALRM_SANG,
                        label = c("SIM", "NÃO"), 
                        levels = c(1, 2)
)

AUX$ALRM_HEMAT <- factor(AUX$ALRM_LETAR,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)

AUX$ALRM_ABDOM <- factor(AUX$ALRM_ABDOM,
                         label = c("SIM", "NÃO"), 
                         levels = c(1, 2)
)
####AUX$Municipio 

AUX01 <- data.frame(COD = AUX[,12], 
                    Municipio = NA)

for (i in AUX[,12]){
  AUX01[which(AUX01$COD == i), 2] <- BASE_IBGE_BRASIL[which(BASE_IBGE_BRASIL$Código.Município.Completo == i),13]
  
}

AUX[,12] <- AUX01[, 2]

####Município de Residência

AUX02 <- data.frame(COD = AUX[,20], 
                    Municipio = NA)

for (i in AUX[,20]){
  AUX02[which(AUX02$COD == i), 2] <- BASE_IBGE_BRASIL[which(BASE_IBGE_BRASIL$Código.Município.Completo == i),13]
  
}

AUX[,20] <- AUX02[, 2]

colnames(AUX)<- c("RS", "SINAN", "Latitude", "Longitude", 
                  "Agravo", "Data_Notificacao", "ANO", "SE_Notificacao", 
                  "Data_Primeiros_Sintomas", "SE_Primeiros_Sintomas", "UF_Notificacao", 
                  "Municipio", "Nome", "Data_Nascimento", "Idade", "Sexo", "Gestante", 
                  "Escolaridade", "Nome_Mae", "Municipio_Residencia", "UF_Residencia", 
                  "RS_Residencia", "Logradouro", "Numero", "Bairro", "CEP", "Zona", 
                  "Data_Digitacao", "Data_Investigacao", "Febre", "Mialgia", "Cefaleia", 
                  "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", "Artrite", 
                  "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", 
                  "Dor_retroorbital", "Diabetes", "Doenca_Hematologica", "Hepatopatia", 
                  "Doenca_Renal", "Hipertensao", "Doenca_Acido_Peptica", "Doenca_Auto_Imune", 
                  "Data_Sorologia", "Resultado_Sorologia", "Data_PCR", "Resultado_PCR", "Sorotipo", 
                  "Classificacao_Final", "Critério_Encerramento", "Autoctone", "UF_Infeccao", 
                  "Municipio_Infeccao", "Bairro_Infeccao", "Evolucao", "Hospitalizado", 
                  "Data_Internamento", "Data_Obito", "Data_Encerramento", "Data_SNA", "Letargia", 
                  "Hepatomegalia", "Acumulo_Liquidos", "Hipotensao_Lipotimia", "Queda_Abrupta_Plaquetas", 
                  "Vomitos_Persistentes", "Hemorragias", "Aumento_Hematocrito", "Dor_Abdominal", 
                  "Data_Dengue_Grave", "Pulso_Debil", "PA_Convergente", "TPC", "Acumulo_Liq_Insuficiencia_Resp", 
                  "Taquicardia", "Extremidades_Frias", "Hipotensao", "Hematemese", "Melena", "Metrorragia_", 
                  "Sangramento_SNC", "Aumento_AST_ALT", "Miocardite", "Alteracao_Consciencia", "Outros_Orgaos", 
                  "Manifestacao_Hemorragica", "Epistaxe", "Gengivorragia", "Metrorragia", "Observacoes" )

assign(paste0("RS", RS, "_2025_SINAN_DECODIFICADO"), AUX) 


#################   Chikungunya/Paraná   ########################
#################################################################
PR_CHIK_2025_SINAN <- CHIKON2025 

PR_2025_SINAN_DECODIFICADO_CHIK <- PR_CHIK_2025_SINAN

PR_2025_SINAN_DECODIFICADO_CHIK$ID_AGRAVO <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$ID_AGRAVO,
                                                    label = c("Dengue", "Chikungunya"), 
                                                    levels = c("A90", "A92.0")
)

###Sintomas###
PR_2025_SINAN_DECODIFICADO_CHIK$FEBRE <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$FEBRE,
                                                label = c("SIM", "NÃO"), 
                                                levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$MIALGIA <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$MIALGIA,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$CEFALEIA <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$CEFALEIA,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$EXANTEMA <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$EXANTEMA,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$VOMITO <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$VOMITO,
                                                 label = c("SIM", "NÃO"), 
                                                 levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$NAUSEA <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$NAUSEA,
                                                 label = c("SIM", "NÃO"), 
                                                 levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$DOR_COSTAS <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$DOR_COSTAS,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$DOR_RETRO <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$DOR_RETRO,
                                                    label = c("SIM", "NÃO"), 
                                                    levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$CONJUNTVIT <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$CONJUNTVIT,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$ARTRALGIA <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$ARTRALGIA,
                                                    label = c("SIM", "NÃO"), 
                                                    levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$ARTRITE <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$ARTRITE,
                                                  label = c("SIM", "NÃO"), 
                                                  levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$PETEQUIA_N <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$PETEQUIA_N,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$LEUCOPENIA <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$LEUCOPENIA,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$LACO <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$LACO,
                                               label = c("SIM", "NÃO"), 
                                               levels = c(1, 2)
)

###Doenças Pré-existentes

PR_2025_SINAN_DECODIFICADO_CHIK$DIABETES <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$DIABETES,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$HEMATOLOG <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$HEMATOLOG,
                                                    label = c("SIM", "NÃO"), 
                                                    levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$HEPATOPAT <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$HEPATOPAT,
                                                    label = c("SIM", "NÃO"), 
                                                    levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$RENAL <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$RENAL,
                                                label = c("SIM", "NÃO"), 
                                                levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$HIPERTENSA <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$HIPERTENSA,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$ACIDO_PEPT <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$ACIDO_PEPT,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$AUTO_IMUNE <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$AUTO_IMUNE,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

####Outros####

PR_2025_SINAN_DECODIFICADO_CHIK$CS_GESTANT <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$CS_GESTANT,
                                                     label = c("1_TRI", "2_TRI", "3_TRI", "IDADE GESTACIONAL IGN", "NÃO", "NÃO SE APLICA", "IGNORADO"), 
                                                     levels = c(1, 2, 3, 4, 5, 6, 9)
)

PR_2025_SINAN_DECODIFICADO_CHIK$CS_ESCOL_N <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$CS_ESCOL_N,
                                                     label = c("ANALFABETO", "1 a 4 SÉRIE DO FUNDAMENTAL INCOMPLETA", "4 SÉRIE DO FUNDAMENTAL COMPLETA", "5 a 8 SÉRIE DO FUNDAMENTAL INCOMPLETA", "FUNDAMENTAL COMPLETO", "ENSINO MÉDIO INCOMPLETO", "ENSINO MÉDIO COMPLETO", "SUPERIOR INCONPLETO", "SUPERIOR COMPLETO", "IGNORADO", "NÃO SE APLICA"), 
                                                     levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)

PR_2025_SINAN_DECODIFICADO_CHIK$RESUL_SORO <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$RESUL_SORO,
                                                     label = c("REAGENTE", "NÃO REAGENTE", "INCONCLUSIVO", "NÃO REALIZADO"), 
                                                     levels = c(1, 2, 3, 4)
)


PR_2025_SINAN_DECODIFICADO_CHIK$RESUL_PCR_ <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$RESUL_PCR_,
                                                     label = c("DETECTÁVEL", "NÃO DETECTÀVEL", "INCONCLUSIVO", "NÃO REALIZADO"), 
                                                     levels = c(1, 2, 3, 4)
)

PR_2025_SINAN_DECODIFICADO_CHIK$SOROTIPO <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$SOROTIPO,
                                                   label = c("I", "II", "III", "IV"), 
                                                   levels = c(1, 2, 3, 4)
)

PR_2025_SINAN_DECODIFICADO_CHIK$CLASSI_FIN <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$CLASSI_FIN,
                                                     label = c("DESCARTADO", "DENGUE", "D.S.A.", "IDENGUE_GRAVE", "CHIKUNGUNYA"), 
                                                     levels = c(5, 10, 11, 12, 13)
)

PR_2025_SINAN_DECODIFICADO_CHIK$CRITERIO <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$CRITERIO,
                                                   label = c("LABORATORIAL", "CLÍNICO-EPIDEMIOLÓGICO", "EM INVESTIGAÇÃO"), 
                                                   levels = c(1, 2, 3)
)

PR_2025_SINAN_DECODIFICADO_CHIK$TPAUTOCTO <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$TPAUTOCTO,
                                                    label = c("SIM", "NÃO"), 
                                                    levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$HOSPITALIZ <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$HOSPITALIZ,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$EVOLUCAO <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$EVOLUCAO,
                                                   label = c("CURA", "ÓBITO PELO AGRAVO", "ÓBITO POR OUTRAS CAUSAS","ÓBITO EM INVESTIGAÇÃO", "INDETERMINADO"), 
                                                   levels = c(1, 2, 3, 4, 9)
)

PR_2025_SINAN_DECODIFICADO_CHIK$CS_ZONA <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$CS_ZONA,
                                                  label = c("URBANA", "RURAL", "PERIURBANA","INDETERMINADO"), 
                                                  levels = c(1, 2, 3, 9)
)

PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_LETAR <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_LETAR,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_HEPAT <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_HEPAT,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_LIQ <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_LIQ,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_HIPOT <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_HIPOT,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_PLAQ <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_PLAQ,
                                                    label = c("SIM", "NÃO"), 
                                                    levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_VOM <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_VOM,
                                                   label = c("SIM", "NÃO"), 
                                                   levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_SANG <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_SANG,
                                                    label = c("SIM", "NÃO"), 
                                                    levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_HEMAT <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_LETAR,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)

PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_ABDOM <- factor(PR_2025_SINAN_DECODIFICADO_CHIK$ALRM_ABDOM,
                                                     label = c("SIM", "NÃO"), 
                                                     levels = c(1, 2)
)
####PR_2025_SINAN_DECODIFICADO_CHIK$Municipio 

PR_2025_SINAN_DECODIFICADO_CHIK_AUX <- data.frame(COD = PR_2025_SINAN_DECODIFICADO_CHIK[,12], 
                                                  Municipio = NA)

for (i in PR_2025_SINAN_DECODIFICADO_CHIK[,12]){
  PR_2025_SINAN_DECODIFICADO_CHIK_AUX[which(PR_2025_SINAN_DECODIFICADO_CHIK_AUX$COD == i), 2] <- BASE_IBGE_BRASIL[which(BASE_IBGE_BRASIL$Código.Município.Completo == i),13]
  
}

PR_2025_SINAN_DECODIFICADO_CHIK[,12] <- PR_2025_SINAN_DECODIFICADO_CHIK_AUX[, 2]

####Município de Residência

PR_2025_SINAN_DECODIFICADO_CHIK_AUX <- data.frame(COD = PR_2025_SINAN_DECODIFICADO_CHIK[,20], 
                                                  Municipio = NA)

for (i in PR_2025_SINAN_DECODIFICADO_CHIK[,20]){
  PR_2025_SINAN_DECODIFICADO_CHIK_AUX[which(PR_2025_SINAN_DECODIFICADO_CHIK_AUX$COD == i), 2] <- BASE_IBGE_BRASIL[which(BASE_IBGE_BRASIL$Código.Município.Completo == i),13]
  
}

PR_2025_SINAN_DECODIFICADO_CHIK[,20] <- PR_2025_SINAN_DECODIFICADO_CHIK_AUX[, 2]

rm (PR_2025_SINAN_DECODIFICADO_CHIK_AUX)

colnames(PR_2025_SINAN_DECODIFICADO_CHIK)<- c("RS", "SINAN", "Latitude", "Longitude", "Agravo", "Data_Notificacao", 
                                              "ANO", "SE_Notificacao", "Data_Primeiros_Sintomas", "SE_Primeiros_Sintomas", 
                                              "UF_Notificacao", "Municipio", "Nome", "Data_Nascimento", "Idade", "Sexo", "Gestante", 
                                              "Escolaridade", "Nome_Mae", "Municipio_Residencia", "UF_Residencia", "RS_Residencia", 
                                              "Logradouro", "Numero", "Bairro", "CEP", "Zona", "Data_Digitacao", "Data_Investigacao",
                                              "Febre", "Mialgia", "Cefaleia", "Exantema", "Vomito", "Nausea", "Dor_nas_Costas", "Conjuntivite", 
                                              "Artrite", "Artralgia_Intensa", "Petequias", "Leucopenia", "Prova_do_Laco_Positiva", 
                                              "Dor_retroorbital", "Diabetes", "Doenca_Hematologica", "Hepatopatia", "Doenca_Renal", 
                                              "Hipertensao", "Doenca_Acido_Peptica", "Doenca_Auto_Imune", "Data_Sorologia", 
                                              "Resultado_Sorologia", "Data_PCR", "Resultado_PCR", "Sorotipo", "Classificacao_Final", 
                                              "Critério_Encerramento", "Autoctone", "UF_Infeccao", "Municipio_Infeccao", "Bairro_Infeccao", 
                                              "Evolucao", "Hospitalizado", "Data_Internamento", "Data_Obito", "Data_Encerramento", "Data_SNA", 
                                              "Letargia", "Hepatomegalia", "Acumulo_Liquidos", "Hipotensao_Lipotimia", "Queda_Abrupta_Plaquetas", 
                                              "Vomitos_Persistentes", "Hemorragias", "Aumento_Hematocrito", "Dor_Abdominal", 
                                              "Data_Dengue_Grave", "Pulso_Debil", "PA_Convergente", "TPC", "Acumulo_Liq_Insuficiencia_Resp", 
                                              "Taquicardia", "Extremidades_Frias", "Hipotensao", "Hematemese", "Melena", "Metrorragia_", 
                                              "Sangramento_SNC", "Aumento_AST_ALT", "Miocardite", "Alteracao_Consciencia", "Outros_Orgaos", 
                                              "Manifestacao_Hemorragica", "Epistaxe", "Gengivorragia", "Metrorragia", "Observacoes" )
