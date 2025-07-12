# This program construct the dataset for analysis

# Opening clean datasets
dt_p   <- as.data.table(readRDS("data/intermediary/clean_data_pacient.rds"))
dt_pde <- as.data.table(readRDS("data/intermediary/clean_data_pacient-day-exam.rds"))

# Creating variables
dt_p <- dt_p[, homem := fcase(genero == "MASCULINO", 1L,
                              genero == "FEMININO", 0L,
                              default = NA_integer_)]

dt_pde <- dt_pde[, dias_espera := as.integer(diaserafeito - diagendou)]

dt_pde <- dt_pde[, dia_feito_semana := format(as.Date(diaserafeito), "%a")]
dt_pde <- dt_pde[, dia_feito_semana := factor(
  dia_feito_semana,
  levels = c("seg", "ter", "qua", "qui", "sex", "sáb", "dom"),
  ordered = TRUE
)]

dt_pde <- dt_pde[, compareceu := fcase(situacao == "Realizou", 1L,
                           situacao == "Faltou", 0L,
                           default = NA_integer_)]

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
  mode_dia_semana = get_mode(dia_feito_semana)
  
), by = .(pacient_id, diagendou)][order(diagendou)]

dt_pd <- dt_pd[, mean_compareceu := n_compareceu / n_exames]

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
  "n_exam",
  "diagendou",
  "diaserafeito",
  "dia_feito_semana",
  "dias_espera",
  "unidade",
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
summary(lm(mean_compareceu ~ homem + mean_dias_espera + n_exames + mode_dia_semana + mode_unidade + raca, data = dt_a))
## dias de espera não estão afetando muito a probabilidade da pessoa faltar (tempo de espera médio de 4 dias)
## O grande causador de faltas é a unidade de IOSE (81% de chance da pessoa faltar ao exame)