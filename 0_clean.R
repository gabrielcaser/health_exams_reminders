# This program cleans the dataset

# Setting the ambience

setwd("C:/Users/gabri/Documents/Github/teste")

# Common used packages
library(data.table)
library(skimr)

# Opening dataset
dt <- fread(
  "data/propria.txt",
  sep = "|",
  header = FALSE
)

# Adding names to columns
setnames(
  dt,
  c(
    "situacao",
    "raca",
    "genero",
    "cns",
    "nome",
    "diagendou",
    "diaserafeito",
    "unidade"
  )
)

# Droping duplicates
nrow(dt[duplicated(dt), ])
dt_duplicatet <- dt[duplicated(dt), ]

nrow(dt[duplicated(dt), ]) / nrow(dt) # 82% da base são dados duplicados

dt_c <- dt[!duplicated(dt), ]

# Creating variables
## dummies
dt_c[, homem := fcase(genero == "MASCULINO", 1L,
                      genero == "FEMININO", 0L,
                      default = NA_integer_)]

dt_c[, dias_espera := diaserafeito - diagendou]

dt_c[, compareceu := fcase(situacao == "Realizou", 1L,
                           situacao == "Faltou", 0L,
                           default = NA_integer_)]
## number of faltas
dt_c[, .(n_faltas = sum(compareceu)), by = cns][order(-n_faltas)]
dt_c[, .(n_obs = .N), by = cns][order(-n_obs)]

dt_c[, n_faltas := sum(compareceu), by = cns]
dt_c[, n_obs := .N, by = cns]
dt_c[, perc_faltas := n_faltas / n_obs]

# Removing variables
dt_c[, nome := NULL]
dt_c[, genero := NULL]
dt_c[, situacao := NULL]

# Sum stats
skim(dt_c)

# Saving clean dataset
