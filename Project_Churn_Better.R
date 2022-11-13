
# ========== Projeto - Churn Telecom ==========

# Prever o churn de uma empresa de telecomunicações

# Fonte dos dados:
# https://raw.githubusercontent.com/carlosfab/dsnp2/master/datasets/WA_Fn-UseC_-Telco-Customer-Churn.csv

# Post origem dos dados
# https://medium.com/data-hackers/
# prever-o-churn-com-python-sim-9fda9c7dda64#id_token=eyJhbGciOiJSUzI1NiIsImtpZCI6IjQwMmYzMDViNzA1ODEzMjlmZ
# jI4OWI1YjNhNjcyODM4MDZlY2E4OTMiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJuYmYiOjE2NjE3NzYxNTUsImF1ZC
# I6IjIxNjI5NjAzNTgzNC1rMWs2cWUwNjBzMnRwMmEyamFtNGxqZGNtczAwc3R0Zy5hcHBzLmdvb2dsZXVzZXJjb250ZW50LmNvbSIsInN1YiI6IjExMDgzOTkyOTU1MjQ3NzE
# 3OTUxMCIsImVtYWlsIjoiaWNhcm9waW5oZWlybzAxQGdtYWlsLmNvbSIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJhenAiOiIyMTYyOTYwMzU4MzQtazFrNnFlMDYwczJ0cDJhMmph
# bTRsamRjbXMwMHN0dGcuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJuYW1lIjoiw41DQVJPIFBJTkhFSVJPIiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW5
# 0LmNvbS9hLS9BRmRadWNxZV9iaHZZa09MNVRvaldfT1h2WkxmSTkyZkRrZlhpZ2E0OGFpeDlnPXM5Ni1jIiwiZ2l2ZW5fbmFtZSI6IsONQ0FSTyIsImZhbWlseV9uYW1lIjoiUElOSEVJUk8iLCJpYX
# QiOjE2NjE3NzY0NTUsImV4cCI6MTY2MTc4MDA1NSwianRpIjoiYzdiMjBlMWMxNzhmNjExMzYzNWY3YTkxYTNiNjcwYTc1MDIyOTFkYSJ9.bQ6NN1Ot3qvbCmM2i82TLLgONHefyz0WZ3bjsOUY1vibdau
# vLyU8_5YyGCN7Qce4CnJcXgQRnbuE6w9gw9NTruYoK2qQURM_RrbyY3p1PtuBOI1xttKWimeP58F_Ri-eylDzzG2pGCjQ4yltkksaPGhPJruCSQWkItyGZVZcSU4qK3p1CE8BuKAEIo-G6MyHoEXF9IjTd9P
# leNVamiYtZsL8r7q6xZ4wBx6vTY4BnTtVTGU6SlE24zVsnJJyZ2iVzpqZ-Wx7Jk5mVSBNb8-b4NjGNbVIUZkwoGF5iH56krH7QlUNDEBKj8DzSx0qMRl5mF-jjBNII6UfWUWJdG_rTA


# ========== Carregando pacotes ==========

library(DescTools)
library(tidyverse)
library(tidymodels)
library(reactable)
library(janitor)
library(highcharter)
library(sparkline)
library(corrplot)
library(patchwork)

# ========== Carregando os dados ==========

# carregando dataset
dados = tibble(read_csv("~/Icaro/Churn_Telecom/WA_Fn-UseC_-Telco-Customer-Churn.csv"))

# resumo do dataset
glimpse(dados)

# padronizando os nomes da vars, excluir a coluna com o ID
dados = 
  dados %>% select(-customerID) %>% clean_names()

# formatando o campo senior citizen
dados$senior_citizen = ifelse(dados$senior_citizen == 0, 'No', 'Yes')

# resumo do dataset
glimpse(dados)


# Sumário - Variáveis Categóricas
sumario_character <- dados %>%
  select(where(is.character)) %>%
  as.list() %>%
  enframe(name = "variavel", value = "valores") %>%
  mutate(
    n_missing = map_dbl(valores, ~sum(is.na(.x))),
    complete_rate = 1 - n_missing/map_dbl(valores, ~length(.x)),
    min = map_dbl(valores, ~min(str_length(.x))),
    max = map_dbl(valores, ~max(str_length(.x))),
    # empty = map_dbl(valores, ~sum(str_detect("^$", .x))),
    n_unique = map_dbl(valores, ~n_distinct(.x)),
    whitespace = map_dbl(valores, ~sum(str_detect("^[:blank:]+$", .x)))
  ) %>%
  arrange(variavel != "churn") 

sumario_character %>%
  reactable(
    wrap = FALSE, 
    resizable = TRUE,
    fullWidth = TRUE,
    defaultColDef = colDef(width = 60),
    columns = list(
      valores = colDef(show = FALSE),
      variavel = colDef("Variável", minWidth = 230, width = 230)
    ),
    details = function(index) {
      variavel_chr <- sumario_character[index, "variavel", drop = TRUE]
      dados %>%
        tabyl(!!sym(variavel_chr)) %>% arrange(desc(n)) %>%
        reactable(columns = list(percent = colDef("%", format = colFormat(percent = TRUE, digits = 1))), width = 500)
    }
  )



# Sumário - Variáveis Numéricas
sumario_numeric <- dados %>%
  select(where(is.numeric)) %>%
  as.list() %>%
  enframe(name = "variavel", value = "valores") %>%
  mutate(
    n_missing = map_dbl(valores, ~sum(is.na(.x))),
    complete_rate = 1 - n_missing/map_dbl(valores, ~length(.x)),
    mean = map_dbl(valores, ~mean(.x)),
    sd = map_dbl(valores, ~sd(.x)),
    min = map_dbl(valores, ~min(.x)),
    median = map_dbl(valores, ~median(.x)),
    max = map_dbl(valores, ~max(.x))
  ) %>%
  mutate(across(where(is.numeric), round, digits = 1))

sumario_numeric %>%
  reactable(
    wrap = FALSE, 
    resizable = TRUE,
    fullWidth = TRUE,
    defaultColDef = colDef(width = 60),
    columns = list(
      valores = colDef(cell = function(values) {sparkline(table(cut(values, min(n_distinct(values), 15))), type = "bar")}),
      variavel = colDef("Variável", minWidth = 230, width = 230)
    ),
    details = function(index) {
      hchart(sumario_numeric[index, "valores"][[1]][[1]])
    }
  )


## Relação entre as explicativas


### Correlações entre as numéricas
dados %>% 
  select(where(is.numeric)) %>% 
  filter(!is.na(dados$total_charges)) %>%  
  cor() %>% 
  corrplot(method = "color", 
           order = "hclust", 
           type = "upper", 
           addCoef.col = "black")


### Sankey das explicativas categóricas
dados %>%
  select(where(is.character), -churn) %>%
  data_to_sankey() %>%
  hchart("sankey")



## Relação com a variável resposta



# base
churn_numeric_long <- dados %>%
  select(where(is.numeric), churn) %>%
  pivot_longer(-churn, names_to = "variavel", values_to = "valor")

### Variáveis numéricas
# plot base
plot_base <- churn_numeric_long %>%
  ggplot(aes(x = valor)) +
  facet_wrap(~variavel, scale = "free", ncol = 1) +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

#### Histogramas
p1 <- plot_base + geom_histogram(aes(fill = churn), position = "identity")

#### Densidades
p2  <- plot_base + geom_density(aes(fill = churn), alpha = 0.4, show.legend = FALSE)

#### Boxplots
p3 <- plot_base + geom_boxplot(aes(fill = churn), show.legend = FALSE)

#### KS
p4 <- plot_base + stat_ecdf(aes(colour = churn), show.legend = FALSE)

(p4 + p3 + p2 + p1) + plot_layout(nrow = 1)


### Variáveis categóricas


# gráfico - balanceamento das classes
dados %>% 
  ggplot(aes(x = churn)) + 
  geom_bar(fill = "#175217") + 
  labs(
    x = "Churn",
    y = "Frequencia", 
    title = "Cancelamento de assinatura",
    subtitle = 'Balanceamento de classes',
    caption = "Fonte: Medium") +
  theme_gray() + coord_flip() +
  theme(
    plot.title = element_text(size = 15L, face = "bold", colour = "#175217"),
    plot.subtitle = element_text(colour = "#175217"),
    axis.title.y = element_text(colour = "#175217"), 
    axis.title.x = element_text(colour = "#175217"))


# tranformando em fator as variaveis categoricas
dados_transf = 
  dados %>% 
  mutate(
    across(
      c(gender:dependents, 
        phone_service, multiple_lines,
        online_security:streaming_movies,
        paperless_billing, churn), 
      factor))

# dados faltantes por variavel
DescTools::PlotMiss(dados_transf)

# resumo
glimpse(dados_transf)


# ========== Modelagem ==========

# marcando a base em teste e treino
set.seed(1232)
churn_split = 
  initial_split(data = dados_transf, strata = churn, prop = 0.75)

# separando a base em treino
training_df = training(churn_split)
# seprando a base em teste
testing_df = testing(churn_split)


# avaliação dos modelos com cross-validation
folds = 
  vfold_cv(training_df, v = 10, strata = churn)


# pré-processamento dos dados

# aqui defino o que é variavel resposta e variavel explicativa
churn_rec = recipe(churn ~ ., data = training_df)

churn_rec$var_info

# preparando os dados 
churn_rec = 
  recipe(churn ~ ., data = training_df) %>% 
  step_corr(all_numeric()) %>% # correlação entre explicativas
  step_normalize(all_numeric()) %>% # normalizar vars
  step_rm(total_charges) %>% # Tratamento de missings
  step_impute_median(all_numeric()) %>% # Substituir os missings com a mediana 
  step_dummy(all_nominal(), -churn) # Transformando as variaveis nominais em dummy 


# especificando o modelo
lr_mod = 
  logistic_reg() %>% 
  set_engine('glm')


# workflow
lr_workflow = 
  workflow() %>% 
  add_recipe(churn_rec) %>% 
  add_model(lr_mod)


# fit ou ajuste
lr_fit = 
  lr_workflow %>% 
  fit_resamples(folds, 
                metrics = metric_set(accuracy, sens, spec, roc_auc),
                control = control_resamples(save_pred = TRUE))

lr_fit

collect_metrics(lr_fit)


# curva ROC, avaliação dos modelos
lr_roc = 
  collect_predictions(lr_fit) %>% 
  roc_curve(truth = churn, .pred_Yes)

lr_roc %>% autoplot() + ggtitle("Curva ROC")


# comparando modelos

# modelo de randow forest (árvore aleatória)
rf_mod = 
  rand_forest() %>%
  set_mode('classification') %>% 
  set_engine('ranger', seed = 123)


rf_workflow = 
  lr_workflow %>% 
  update_model(rf_mod)


rf_fit = 
  rf_workflow %>% 
  fit_resamples(folds, 
                metrics = metric_set(accuracy, sens, spec, roc_auc),
                control = control_resamples(save_pred = TRUE))


# comparando as métricas dos modelos
collect_metrics(lr_fit)
collect_metrics(rf_fit)


# Curva ROC
rf_roc = 
  rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(truth = churn, .pred_Yes)

# Gráfico das curvas ROC
bind_rows(
  lr_roc %>% mutate(model = "Regressão Logística"),
  rf_roc %>% mutate(model = "Random Forest")) %>% 
    ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.2) + 
  geom_abline(lty = 3) + 
  coord_equal() + 
  labs(title = "Curva ROC", col = NULL) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "top")


# modelo final


# escolher o workflow do melhor modelo
final_fit = 
  rf_workflow %>% 
  fit(training_df)


predict(final_fit, testing_df, type = "prob")

# avaliação do modelo
pred = 
  bind_cols(
    testing_df %>% select(churn),
    predict(final_fit, testing_df, type = "prob"),
    predict(final_fit, testing_df))

class_metrics = metric_set(accuracy, roc_auc, sens, spec)

class_metrics(pred, truth = churn, estimate = .pred_class, .pred_Yes)







