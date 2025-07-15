# Descrição: Este script cria visualizações da variável compareceu por diferentes grupos

# Abrir o conjunto de dados
dt_pde <- as.data.table(readRDS("data/intermediary/constructed_data_pacient-day-exam.rds"))

# Geração de gráficos

## Total de exames por unidade preenchendo compareceu
dt_pde %>%
  group_by(unidade, compareceu) %>%
  summarise(total_exames = n()) %>%
  ggplot(aes(x = reorder(unidade, total_exames), y = total_exames, fill = compareceu)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = total_exames), position = position_dodge(width = 0.9), hjust = -0.1) +
  labs(title = "Total de Exames por Unidade e Comparecimento", x = "Unidade", y = "Total de Exames") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.x = element_blank())

## Porcentagem de comparecimento por unidade
dt_pde %>%
  group_by(unidade) %>%
  summarise(pct_compareceu = mean(as.integer(compareceu) - 1, na.rm = TRUE) * 100) %>%
  ggplot(aes(x = reorder(unidade, -pct_compareceu), y = pct_compareceu)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(pct_compareceu, 2)), hjust = -0.1) +
  labs(title = "Porcentagem de Comparecimento por Unidade", x = "Unidade", y = "Comparecimento (%)") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.x = element_blank())

## Dias de espera por unidade
dt_pde %>%
  group_by(unidade) %>%
  summarise(media_dias_espera = mean(dias_espera, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(unidade, media_dias_espera), y = media_dias_espera)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = round(media_dias_espera, 1)), hjust = -0.1) +
  labs(title = "Média de Dias de Espera por Unidade", x = "Unidade", y = "Média de Dias de Espera") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.x = element_blank())

## Porcentagem de comparecimento por mês
dt_pde %>%
  mutate(mes = floor_date(diagendou, "month")) %>%
  group_by(mes) %>%
  summarise(pct_compareceu = mean(as.integer(compareceu) - 1, na.rm = TRUE) * 100) %>%
  ggplot(aes(x = mes, y = pct_compareceu)) +
  geom_line() +
  labs(title = "Porcentagem de Comparecimento por Mês", x = "Mês", y = "Comparecimento (%)") +
  theme_minimal() +
  theme(panel.grid = element_blank())

## Evolução dos exames por mês
dt_pde %>%
  mutate(mes = floor_date(diagendou, "month")) %>%
  group_by(mes) %>%
  summarise(total_exames = n()) %>%
  ggplot(aes(x = mes, y = total_exames)) +
  geom_line() +
  labs(title = "Total de Exames por Mês", x = "Mês", y = "Total de Exames") +
  theme_minimal() +
  theme(panel.grid = element_blank())


## Total de exames por unidade
dt_pde %>%
  group_by(unidade) %>%
  summarise(total_exames = n()) %>%
  ggplot(aes(x = reorder(unidade, total_exames), y = total_exames)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_exames), hjust = -0.1) +
  labs(title = "Total de Exames por Unidade", x = "Unidade", y = "Total de Exames") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.x = element_blank())


## Total de "Não compareceu" por unidade
dt_pde %>%
  filter(compareceu == "No") %>%
  group_by(unidade) %>%
  summarise(total_nao = n()) %>%
  ggplot(aes(x = reorder(unidade, total_nao), y = total_nao)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = total_nao), hjust = -0.1) +
  labs(title = "Total de 'Não Compareceu' por Unidade", x = "Unidade", y = "Total de 'Não Compareceu'") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.x = element_blank())

## Total de exames por unidade e mês (sem legenda)
dt_pde %>%
  mutate(mes = floor_date(diagendou, "month")) %>%
  group_by(unidade, mes) %>%
  summarise(total_exames = n()) %>%
  ggplot(aes(x = mes, y = total_exames, color = unidade)) +
  geom_line() +
  labs(title = "Total de Exames por Unidade e Mês", x = "Mês", y = "Total de Exames") +
  theme_minimal() +
  theme(panel.grid = element_blank(), legend.position = "none") +
  facet_wrap(~unidade)

## Porcentagem de comparecimento por unidade e mês
dt_pde %>%
  mutate(mes = floor_date(diagendou, "month")) %>%
  group_by(unidade, mes) %>%
  summarise(pct_compareceu = mean(as.integer(compareceu) - 1, na.rm = TRUE) * 100) %>%
  ggplot(aes(x = mes, y = pct_compareceu, color = unidade)) +
  geom_line() +
  labs(title = "Porcentagem de Comparecimento por Unidade e Mês", x = "Mês", y = "Comparecimento (%)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), legend.position = "none") +
  facet_wrap(~unidade)

## Percentual de exames por dia da semana
dt_pde %>%
  mutate(dia_da_semana = wday(diagendou, label = TRUE)) %>%
  group_by(dia_da_semana) %>%
  summarise(total_exames = n()) %>%
  mutate(pct_exames = total_exames / sum(total_exames) * 100) %>%
  ggplot(aes(x = dia_da_semana, y = pct_exames)) +
  geom_text(aes(label = round(pct_exames,1)), vjust = -0.2) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Percentual de Exames por Dia da Semana", x = "Dia da Semana", y = "Percentual de Exames (%)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.y = element_blank())

## Percentual de comparecimento por dia da semana
dt_pde %>%
  mutate(dia_da_semana = wday(diagendou, label = TRUE)) %>%
  group_by(dia_da_semana) %>%
  summarise(pct_compareceu = mean(as.integer(compareceu) - 1, na.rm = TRUE) * 100) %>%
  ggplot(aes(x = dia_da_semana, y = pct_compareceu)) +
  geom_text(aes(label = round(pct_compareceu,1)), vjust = -0.2) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Percentual de Comparecimento por Dia da Semana", x = "Dia da Semana", y = "Comparecimento (%)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.y = element_blank())

## Evolução dos dias de espera
dt_pde %>%
  mutate(mes = floor_date(diagendou, "month")) %>%
  group_by(mes) %>%
  summarise(media_dias_espera = mean(dias_espera, na.rm = TRUE)) %>%
  ggplot(aes(x = mes, y = media_dias_espera)) +
  geom_line(color = "purple") +
  labs(title = "Média de Dias de Espera por Mês", x = "Mês", y = "Média de Dias de Espera") +
  theme_minimal() +
  theme(panel.grid = element_blank())

