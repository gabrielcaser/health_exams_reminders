# This program construct the dataset for analysis

# Opening clean datasets
dt_p <- as.data.table(readRDS("data/intermediary/clean_data_pacient.rds"))
dt_e <- as.data.table(readRDS("data/intermediary/clean_data_exam.rds"))

# Creating variables

## dummies
dt_p[, homem := fcase(genero == "MASCULINO", 1L,
                      genero == "FEMININO", 0L,
                      default = NA_integer_)]

dt_e[, dias_espera := diaserafeito - diagendou]

dt_e[, compareceu := fcase(situacao == "Realizou", 1L,
                           situacao == "Faltou", 0L,
                           default = NA_integer_)]
## number of faltas
dt_e[, .(n_faltas = sum(compareceu)), by = pacient_id][order(-n_faltas)]
dt_e[, .(n_obs = .N), by = pacient_id][order(-n_obs)]

dt_e[, n_faltas := sum(compareceu), by = pacient_id]
dt_e[, n_obs := .N, by = pacient_id]
dt_e[, perc_faltas := n_faltas / n_obs]

# Removing variables
dt_p[, genero := NULL]
dt_e[, situacao := NULL]



# Reshaping from exam-level to person-level