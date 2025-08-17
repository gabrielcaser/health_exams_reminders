# This program construct the dataset for analysis

# Opening clean datasets
dt_p   <- as.data.table(readRDS("data/intermediary/clean_data_pacient.rds"))
dt_pde <- as.data.table(readRDS("data/intermediary/clean_data_pacient-day-exam.rds"))

# Creating variables
dt_p <- dt_p[, homem := fcase(genero == "M", 1L,
                              genero == "F", 0L,
                              default = NA_integer_)]

dt_pde <- dt_pde[, dias_espera := as.integer(diaserafeito - diagendou)]

dt_pde <- dt_pde[, dia_feito_semana := format(as.Date(diaserafeito), "%a")]

dt_pde[, dia_feito_semana := case_when(
  dia_feito_semana == "seg" ~ "Mon",
  dia_feito_semana == "ter" ~ "Tue",
  dia_feito_semana == "qua" ~ "Wed",
  dia_feito_semana == "qui" ~ "Thu",
  dia_feito_semana == "sex" ~ "Fri",
  dia_feito_semana == "sab" ~ "Sat",
  dia_feito_semana == "dom" ~ "Sun",
  TRUE ~ NA_character_
)]


dt_pde <- dt_pde[, dia_feito_semana := factor(
  dia_feito_semana,
  levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
  ordered = FALSE
)]


dt_pde <- dt_pde[, compareceu := fcase(situacao == "Não Faltou", 1L,
                           situacao == "Faltou", 0L,
                           default = NA_integer_)]

#------------------------------------------------------------------
# 1) Funções utilitárias
#------------------------------------------------------------------
clean_text <- function(x) {
  x <- toupper(x)
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[^A-Z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Grepl helper (case-insensitive, já estamos com upper)
has <- function(x, pattern) grepl(pattern, x, perl = TRUE)

#------------------------------------------------------------------
# 2) BAIRROS — padronização mantendo individualizados
#------------------------------------------------------------------


bairros_propria <- c(
  "CENTRO",
  "MATADOURO",
  "FERNANDES",
  "BRASILIA",
  "REMANSO",
  "SANTO ANTONIO",
  "NOSSA SENHORA DE FATIMA",
  "AMERICA",
  "ARAME",
  "CONJUNTO MARIA DO CARMO",
  "CAMPO DO PADRE",
  "CAMPO JOAO ALVES"
)

# 3) Limpeza de bairros + matching aproximado (amatch)
dt_pde[, bairro_clean := clean_text(bairro)]

# índice do melhor match por distância de edição (tolerância = 3)
idx_match <- amatch(dt_pde$bairro_clean, bairros_propria, maxDist = 3)

# cria bairro_final reduzindo cardinalidade
dt_pde[, bairro_final := ifelse(!is.na(idx_match), bairros_propria[idx_match], "OUTROS")]

#------------------------------------------------------------------
# 3) PROCEDIMENTOS — ~10 categorias
#------------------------------------------------------------------
dt_pde[, proc_clean := clean_text(procedimento)]

# Padrões (você pode expandir livremente)
pat_consulta_med   <- "(\\bCONSULTA\\b|ATENDIMENTO\\s+MEDIC|CLINICO|PEDIATR|GERIATR|OFTALMO|GINECO|CIRURGIAO|CARDIO)"
pat_consulta_nao   <- "(PSICO(LOGIA)?|NUTRI(CAO|CIONISTA)|SERVICO\\s+SOCIAL|ENFERMAGEM\\s+CONSULTA|FONOAUDIO|TERAPIA\\s+OCUP)"
pat_lab            <- "(HEMOGRAMA|HEMOGLOB|GLICEM|COLESTER|TRIGLIC|UREIA|CREATIN|TGO|TGP|TSH|HORMON|SOROL|LABORATORIO|URINA|UROCULT)"
pat_imagem         <- "(RAIO\\s?X|RX\\b|RADIOL|ULTRA(SSOM|SON)|USG|MAMO(GRAFIA)?|TOMO(GRAFIA)?|RESSON(ANCIA)?|RMN|IMAGEM)"
pat_diag_outros    <- "(ELETRO(CARDIO|ENCEF)|ECG\\b|EEG\\b|ESPIROMET|AUDIOMET|ERGOMET|TESTE\\s+(RAPIDO|DE)|CITO(LOGIA)?|PAPANIC)"
pat_terapias       <- "(FISIO(TERAPIA)?|REABILIT|FONOAUDIO|TERAPIA\\s+OCUP|ACOMPANHAMENTO\\s+EM\\s+REABILIT)"
pat_odont          <- "(ODONTO(LOGIA)?|DENTAR|EXODONTIA|EXTRACAO\\s+DENT|REST(AURACAO)?|APLICACAO\\s+DE\\s+FLUOR)"
pat_ambulatoriais  <- "(CURATIVO|NEBULIZ|APLICACAO\\s+DE\\s+MEDIC|RETIRADA\\s+DE\\s+PONTO|PUNCAO|DRENAGEM\\s+PEQ)"
pat_vacina         <- "(VACINA|IMUNIZ)"
pat_intern_cir     <- "(INTERNAC|CIRURG(IA|ICO)|OPERACAO)"

# 10 categorias alvo (ajuste nomes conforme seu gosto)
dt_pde[, proc_cat10 := fifelse(has(proc_clean, pat_consulta_med),   "Consulta Medica",
                               fifelse(has(proc_clean, pat_consulta_nao),   "Consulta/NAO Medica",
                                       fifelse(has(proc_clean, pat_lab),            "Exame Laboratorial",
                                               fifelse(has(proc_clean, pat_imagem),         "Exame de Imagem",
                                                       fifelse(has(proc_clean, pat_diag_outros),    "Outros Exames Diagnosticos",
                                                               fifelse(has(proc_clean, pat_terapias),       "Terapias/Reabilitacao",
                                                                       fifelse(has(proc_clean, pat_odont),          "Odontologia",
                                                                               fifelse(has(proc_clean, pat_ambulatoriais),  "Proc Ambulatoriais",
                                                                                       fifelse(has(proc_clean, pat_vacina),         "Vacinacao/Imunizacao",
                                                                                               fifelse(has(proc_clean, pat_intern_cir),     "Internacao/Cirurgia", 
                                                                                                       "Outros"))))))))))]

#------------------------------------------------------------------
# 4) Pós-ajustes para reduzir "Outros" (opcional, mas recomendado)
#     - Reclassifica alguns termos muito comuns que tenham escapado
#------------------------------------------------------------------
# Exemplos de resgate de "Outros" -> categorias relevantes
reclass <- list(
  # se “Outros” contém "ECOCARDIOGR" => imagem (ou diagnóstico)
  list(pattern = "ECOCARDIO", new = "Exame de Imagem"),
  # se “Outros” contém "CITOLOGIA" => outros exames diagn.
  list(pattern = "CITOLOGIA", new = "Outros Exames Diagnosticos"),
  # se “Outros” contém "ELETROFORESE" => lab
  list(pattern = "ELETROFORESE", new = "Exame Laboratorial")
)

for (rr in reclass) {
  dt_pde[proc_cat10 == "Outros" & has(proc_clean, rr$pattern), proc_cat10 := rr$new]
}

# Reshaping to Pacient-Day level

# Função para calcular a moda
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Aplicando no seu data.table
dt_pd <- dt_pde[, .(
  n_exames = .N,
  n_compareceu = sum(compareceu),
  mean_dias_espera = mean(dias_espera, na.rm = TRUE),
  mode_unidade = get_mode(unidade),
  mode_dia_semana = get_mode(dia_feito_semana),
  mode_procedimento = get_mode(proc_cat10),
  mode_bairro = get_mode(bairro_final)
  
), by = .(pacient_id, diagendou)][order(diagendou)]

dt_pd <- dt_pd[, mean_compareceu := n_compareceu / n_exames]

dt_pd <- dt_pd[, compareceu_dummy := ifelse(mean_compareceu > 0, 1, 0)]

dt_pd <- dt_pd[, pd_id := paste0(pacient_id,"|",as.character(diagendou))]

setcolorder(dt_pd, c("pd_id", "pacient_id"))

# Removing variables
dt_p[, genero := NULL]
dt_pde[, situacao := NULL]

# Changing types
dt_pde[, compareceu := factor(compareceu, levels = c(0, 1), labels = c("No", "Yes"))]
dt_p[, homem := factor(homem, levels = c(0, 1), labels = c("No", "Yes"))]

# Labels for dt_p
var_label(dt_p$pacient_id) <- "Anonymized patient identifier"
var_label(dt_p$raca)       <- "Patient race/color"
var_label(dt_p$homem)      <- "Male indicator"

# Labels for dt_pde
var_label(dt_pde$compareceu)       <- "Attendance indicator"
var_label(dt_pde$dias_espera)      <- "Days between scheduling and performance"
var_label(dt_pde$dia_feito_semana) <- "Day of the week to attend"

# Labels for dt_pd
var_label(dt_pd$pd_id)            <- "Unique patient|day identifier"
var_label(dt_pd$n_exames)         <- "Number of exams scheduled on the day"
var_label(dt_pd$n_compareceu)     <- "Number of attended exams on the day"
var_label(dt_pd$mean_compareceu)  <- "Proportion of exams attended on the day"
var_label(dt_pd$mean_dias_espera) <- "Average waiting time (in days) for exams on the day"
var_label(dt_pd$mode_unidade)     <- "Most frequent health unit where exams were done"
var_label(dt_pd$mode_dia_semana)  <- "Most frequent day of the week when exams were done"


# Changing columns order
setcolorder(dt_pde, c(
  "pde_id",
  "pacient_id",
  "procedimento",
  "diagendou",
  "diaserafeito",
  "dia_feito_semana",
  "dias_espera",
  "unidade",
  "bairro",
  "compareceu"
))

# Save constructed datasets
saveRDS(dt_p,  "data/intermediary/constructed_data_pacient.rds")
saveRDS(dt_pd, "data/intermediary/constructed_data_pacient-day.rds")
saveRDS(dt_pde,"data/intermediary/constructed_data_pacient-day-exam.rds")

# Creating analysis dataset
dt_a <- merge(dt_pd, dt_p, by = "pacient_id", all = TRUE)

# Saving
saveRDS(dt_a, "data/final/analysis_data_pacient-day.rds")

# Testing
summary(lm(mean_compareceu ~ homem + mean_dias_espera + n_exames + mode_dia_semana + mode_unidade + raca + diagendou + mode_procedimento + mode_bairro, data = dt_a))
skim(dt_a)
## dias de espera não estão afetando muito a probabilidade da pessoa faltar (tempo de espera médio de 4 dias)
## O grande causador de faltas é a unidade de IOSE (81% de chance da pessoa faltar ao exame)