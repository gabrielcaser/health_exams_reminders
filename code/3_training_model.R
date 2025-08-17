set.seed(1235)
# Opening dataset
rm(list = ls()) 
dt  <- as.data.table(readRDS("data/final/analysis_data_pacient-day.rds"))

# Preparação dos dados para ML -------------------------------------------

# Dropando casos em que age é missing
dt <- dt[!is.na(age), ]

# Dividir em treino e teste

idx <- sample(1:nrow(dt), size = round(0.7 * nrow(dt)))
training <- dt[idx, ]
test <- dt[-idx, ]

# Função de custo customizada: penaliza FP com peso 1.5
custom_cost <- function(sens, spec) {
  FNR <- 1 - sens
  FPR <- 1 - spec
  cost <- FNR * 1 + FPR * 1.3
  return(cost)
}

# Função auxiliar para obter melhor threshold com penalização customizada
obter_melhor_threshold <- function(roc_obj) {
  coords_custom <- coords(
    roc_obj,
    x = "all",
    ret = c("threshold", "sensitivity", "specificity"),
    transpose = FALSE
  )
  coords_custom$cost <- with(coords_custom, custom_cost(sensitivity, specificity))
  best_thresh <- coords_custom$threshold[which.min(coords_custom$cost)]
  return(best_thresh)
}

# Modelo 1: Regressão Logística -------------------------------------------

# Ajustar modelo com controle de convergência
logreg <- glm(
  compareceu_dummy ~ homem + mean_dias_espera + n_exames + mode_dia_semana + mode_unidade + raca + diagendou + age + mode_procedimento + mode_bairro,
  data = training,
  family = binomial,
  control = list(maxit = 100)
)

# Applying the model into test data
prob_logreg <- predict(logreg, newdata = test, type = "response") # THAT'S THE PART WE'RE GOING TO USE IN THE NEW DATA SET. We're applying the estimated model into new data!

# Melhor threshold
roc_logreg <- roc(test$compareceu_dummy, prob_logreg)
best_threshold_logreg <- obter_melhor_threshold(roc_logreg)


y_hat_logreg <- factor(ifelse(prob_logreg >= best_threshold_logreg, 1, 0))
acc_logreg <- mean(y_hat_logreg == test$compareceu_dummy, na.rm = TRUE)
auc_logreg <- auc(roc_logreg)

cat("Acurácia:", round(acc_logreg * 100, 2), "%\n")
cat("AUC:", round(auc_logreg, 3), "\n")

# Modelo 2: Árvore de Decisão --------------------------------------------
ctree <-
  tree::tree(
    compareceu_dummy ~ homem + mean_dias_espera + n_exames + mode_dia_semana + mode_unidade + raca + diagendou + age + mode_procedimento + mode_bairro,
    data = training,
    na.action = na.exclude
  )

prob_ctree <- predict(ctree, newdata = test, type = "vector")

roc_ctree <- roc(test$compareceu_dummy, prob_ctree)
best_threshold_ctree <- obter_melhor_threshold(roc_ctree)


y_hat_ctree <- factor(ifelse(prob_ctree >= best_threshold_ctree, 1, 0))
acc_ctree <- mean(y_hat_ctree == test$compareceu_dummy, na.rm = TRUE)
auc_ctree <- auc(roc_ctree)

cat("Acurácia:", round(acc_ctree * 100, 2), "%\n")
cat("AUC:", round(auc_ctree, 3), "\n")


# Modelo 3: Random Forest -------------------------------------------------

rf <- randomForest::randomForest(
  compareceu_dummy ~ homem + mean_dias_espera + n_exames + mode_dia_semana + mode_unidade + raca + diagendou + age + mode_procedimento + mode_bairro,
  data = training,
  na.action = na.exclude,
  ntree = 50,
  importance = TRUE
)

prob_rf <- predict(rf, newdata = test) # Probabilidade "Sim"

roc_rf <- roc(test$compareceu_dummy, prob_rf)
best_threshold_rf <- obter_melhor_threshold(roc_rf)


y_hat_rf <- factor(ifelse(prob_rf >= best_threshold_rf, 1, 0))
acc_rf <- mean(y_hat_rf == test$compareceu_dummy, na.rm = TRUE)
auc_rf <- auc(roc_rf)

cat("Acurácia:", round(acc_rf * 100, 2), "%\n")
cat("AUC:", round(auc_rf, 3), "\n")

# Mostrar importância das variáveis
cat("Top 5 variáveis mais importantes:\n")
importance_data <- randomForest::importance(rf)[order(-randomForest::importance(rf)[, 2]), , drop = FALSE]
print(head(importance_data, 5))

# Modelo 4: Boosting ------------------------------------------------------
# Variáveis explicativas
vars <- c("homem", "mean_dias_espera", "n_exames", "mode_dia_semana", "mode_unidade", "raca", "diagendou", "age", "mode_procedimento", "mode_bairro")

# Criar matriz de preditores com dummies
X_train <- model.matrix(~ . - 1, data = training[, ..vars])
X_test  <- model.matrix(~ . - 1, data = test[, ..vars])

# Variável resposta
y_train <- training$compareceu_dummy
y_test  <- test$compareceu_dummy

# Criar objetos DMatrix
train_matrix <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
test_matrix  <- xgboost::xgb.DMatrix(data = X_test, label = y_test)

# Treinar o modelo XGBoost
boosting <- xgboost::xgboost(
  data = train_matrix,
  objective = "binary:logistic",
  nrounds = 50,
  verbose = 0
)

# Prever probabilidades no conjunto de teste
prob_boosting <- predict(boosting, newdata = test_matrix)

# Calcular AUC e threshold ótimo
roc_boosting <- roc(y_test, prob_boosting)
# Obter melhor threshold com penalização customizada
best_threshold_boosting <- obter_melhor_threshold(roc_boosting)

print(paste("Melhor threshold penalizando FP:", round(best_threshold_boosting, 3)))

# Classificação binária
y_hat_boosting <- factor(
  ifelse(prob_boosting >= best_threshold_boosting, "Sim", "Não"),
  levels = c("Não", "Sim")
)

# Versão fator real para comparação
compareceu_factor <- factor(ifelse(y_test == 1, "Sim", "Não"), levels = c("Não", "Sim"))

# Métricas
acc_boosting <- mean(y_hat_boosting == compareceu_factor, na.rm = TRUE)
auc_boosting <- auc(roc_boosting)

cat("Acurácia:", round(acc_boosting * 100, 2), "%\n")
cat("AUC:", round(auc_boosting, 3), "\n")


# Comparação dos modelos --------------------------------------------------
cat("\n=== COMPARAÇÃO DOS MODELOS ===\n")

resultados <- data.frame(
  Modelo = c(
    "Regressão Logística",
    "Árvore de Decisão",
    "Random Forest",
    "Boosting"
  ),
  Acuracia = c(acc_logreg, acc_ctree, acc_rf, acc_boosting) * 100,
  AUC = c(auc_logreg, auc_ctree, auc_rf, auc_boosting)
) %>%
  dplyr::arrange(desc(AUC))

print(resultados)

# Melhor modelo
melhor_modelo <- resultados$Modelo[1]
cat("\nMelhor modelo:", melhor_modelo, "\n")

# Gráfico da Curva ROC ----------------------------------------------------
cat("\n=== GERANDO CURVA ROC ===\n")

# Verificar se as probabilidades existem antes de plotar
  # Criar o gráfico
  plot.roc(
    test$compareceu_dummy,
    prob_logreg,
    col = 1,
    grid = TRUE,
    xlab = "Taxa de Falsos Positivos (1 - Especificidade)",
    ylab = "Taxa de Verdadeiros Positivos (Sensibilidade)",
    main = "Curva ROC - Predição de Comparecimentos CRAS",
    legacy.axes = TRUE,
    asp = FALSE,
    las = 1
  )
  
    plot.roc(test$compareceu_dummy,
             prob_ctree,
             col = 2,
             add = TRUE)
  
    plot.roc(test$compareceu_dummy,
             prob_rf,
             col = 3,
             add = TRUE)
  
    plot.roc(test$compareceu_dummy,
             prob_boosting,
             col = 4,
             add = TRUE)


# Matriz de confusão do melhor modelo
cat("\nMatriz de Confusão do Melhor Modelo (",
    melhor_modelo,
    "):\n")
if (melhor_modelo == "Random Forest") {
  print(addmargins(table(Predição = y_hat_rf, Real = test$compareceu_dummy)))
} else if (melhor_modelo == "Boosting") {
  print(addmargins(table(Predição = y_hat_boosting, Real = test$compareceu_dummy)))
} else if (melhor_modelo == "Regressão Logística") {
  print(addmargins(table(Predição = y_hat_logreg, Real = test$compareceu_dummy)))
} else {
  print(addmargins(table(Predição = y_hat_ctree, Real = test$compareceu_dummy)))
}

# Relatório final
cat("\n=== RELATÓRIO FINAL ===\n")
cat("Sistema de Lembretes Automatizados - Saúde\n")
cat("Modelo com melhor performance:", melhor_modelo, "\n")
cat("AUC do melhor modelo:", round(max(resultados$AUC), 3), "\n")
cat("Acurácia do melhor modelo:", round(max(resultados$Acuracia), 2), "%\n")
cat("\nAnálise concluída! Verifique os gráficos e resultados acima.\n")
