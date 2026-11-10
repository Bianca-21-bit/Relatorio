# -------------------------------------
# Bibliotecas e Configurações
# -------------------------------------

library(tidyverse)

theme_set(theme_minimal(base_size = 12))

dir.create("graficos", showWarnings = FALSE)

# Função auxiliar para salvar cada gráfico logo depois de exibi-lo
salvar <- function(grafico, nome, largura = 10, altura = 6) {
  print(grafico)
  ggsave(file.path("graficos", paste0(nome, ".png")), grafico,
         width = largura, height = altura, dpi = 150)
}


# ------------------------------------
# Importação
# ------------------------------------

dados <- read.csv("sleep_debt_screen_time.csv") # Lendo o arquivo

# ---------------------------------
# Conhecendo a base
# ----------------------------------

head(dados)
glimpse(dados)
summary(dados)
cat("Linhas: ", nrow(dados), "| Colunas: ", ncol(dados), "\n")


# ------------------------------------------
# Ajuste de Tipos
# ------------------------------------------

dados <- dados %>%
  mutate(
    bedtime_hour = if_else(bedtime_hour < 12, bedtime_hour + 24, bedtime_hour),
    blue_light_filter_on = case_when(
      tolower(as.character(blue_light_filter_on)) %in% c("true", "1", "yes", "sim") ~ "Sim",
      tolower(as.character(blue_light_filter_on)) %in% c("false", "0", "no", "não", "nao") ~ "Não",
      TRUE ~ NA_character_
    ),
    blue_light_filter_on = factor(blue_light_filter_on, levels = c("Não", "Sim"))
  )


# Separação das variáveis numéricas das categóricas

numericas <- dados %>%
  select(where(is.numeric)) %>%
  select(-user_id)

categoricas <- dados %>%
  select(where(~ is.factor(.) | is.character(.)))

names(numericas) # Ver os nomes de todas as variáveis numéricas
names(categoricas) # Ver os nomes de todas as variáveis categóricas


# -------------------------------------
# Qualidade dos Dados
# -------------------------------------

# Valores ausentes (contagem e percentual)

ausentes <- dados %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to =  "variavel", values_to =  "n_ausentes") %>%
  mutate(pct_ausentes = round(100 * n_ausentes / nrow(dados), 2)) %>%
  arrange(desc(n_ausentes))
print(ausentes, n = Inf)

# Linhas duplicadas
cat("\nLinhas duplicadas: ", sum(duplicated(dados)), "\n")
if ("user_id" %in% names(dados)) {
  cat("user_id repetidos: ", sum(duplicated(dados$user_id)), "\n")
}

## Valores impossíveis

# Valores negativos em qualquer variável numérica
negativos <- numericas %>%
  summarise(across(everything(), ~ sum(. < 0, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "n_negativos") %>%
  filter(n_negativos > 0)
cat("\nVariáveis com valores negativos: \n")
print(if (nrow(negativos) == 0) "nenhuma" else negativos)

# Percentuais (colunas terminadas em _pct) fora do intervalo 0-100
fora_pct <- numericas %>%
  select(ends_with("_pct")) %>%
  summarise(across(everything(), ~ sum(. < 0 | . > 100, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "n_fora_0_100") %>%
  filter(n_fora_0_100 > 0)
cat("\nPercentuais fora de 0-100:\n")
print(if (nrow(fora_pct) == 0) "nenhum" else fora_pct)

#Horas de sono fora de 0-24
cat("\ntotal_sleep_hours fora de 0-24:",
    sum(dados$total_sleep_hours < 0 | dados$total_sleep_hours > 24, na.rm = TRUE), "\n")


# -----------------------------------
# Análise Univariada: Variáveis Numéricas
# -----------------------------------


# Estatísticas descritivas completas
resumo_num <- numericas %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "valor") %>%
  group_by(variavel) %>%
  summarise(
    n = sum(!is.na(valor)),
    media = mean(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    dp = sd(valor, na.rm = TRUE),
    min = min(valor, na.rm = TRUE),
    q1 = quantile(valor, 0.25, na.rm = TRUE),
    q3 = quantile(valor, 0.75, na.rm = TRUE),
    max = max(valor, na.rm = TRUE),
    iqr = IQR(valor, na.rm = TRUE),
    # Assimetria de Pearson: > 0 cauda à direita, < 0 cauda à esquerda
    assimetria = ifelse(dp == 0, NA,
                        3 * (media - mediana) / dp),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))
print(resumo_num, n = Inf)
  
# Histogramas

p_hist <- numericas %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "valor") %>%
  ggplot(aes(x = valor)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variavel, scales = "free") +
  labs(title = "Distribuição das Variáveis Numéricas",
       x = NULL, y = "Frequência")
salvar(p_hist, "01_histogramas", 14, 10)
  
# Boxplots (um painel por variável, cada um na sua escala)

p_box <- numericas %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "valor") %>%
  ggplot(aes(x = "", y = valor)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.alpha = 0.5) +
  facet_wrap(~ variavel, scales = "free") +
  labs(title = "Boxplots das Variáveis Numéricas (outliers em vermelho)",
       x = NULL, y = NULL)
salvar(p_box, "02_boxplots", 14, 10)

# Contagem de outliers (redgra de Tukey: 1,5 x IQR)

contar_outliers <- function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  lim <- 1.5 * (q[2] - q[1])
  sum(x < q[1] - lim | x> q[2] + lim, na.rm = TRUE)
}

outliers <- numericas %>%
  summarise(across(everything(), contar_outliers)) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "n_outliers") %>%
  mutate(pct = round(100 * n_outliers / nrow(dados), 2)) %>%
  arrange(desc(n_outliers))
print(outliers, n = Inf)

# Normalidade (indicativa)

normalidade <- map_dfr(names(numericas), function(v){
  
  x <- na.omit(numericas[[v]])
  
  if(length(x) > 5000)
    x <- sample(x, 5000)
  
  tibble(
    variavel = v,
    n = length(x),
    p_shapiro = signif(shapiro.test(x)$p.value, 3)
  )
  
}) %>%
  mutate(
    parece_normal = p_shapiro > 0.05
  )

##OBS.: O teste de Shapiro-Wilk ele parte da hipótese de que a variável segue uma distribuição normal. 
# Se 'p_shapiro' for menor que 0,05, rejeita-se essa hipótese, e a coluna 'parece_normal' aparece 'FALSE'.
# Se for maior, não há evidência contra a normalidade, e a coluna aparece como 'TRUE'. Esse 'não rejeitar'
# não prova que a variável é normal.
# Para amostras grandes o Shapiro fica extremamente sensível e tende a rejeitar a normalidade mesmo para desvios pequenos.

## Em relação ao resultado: As variáveis são "aproximadamente simétricas" ou "assimétricas à direita",
# conforme o uso de métodos não paramétricos. O p-valor do Shapiro-Wilk sozinho não sustenta nenhuma dessas afirmações.


# -----------------------------------------------------------
# Análise Univariada: Variáveis Categóricas
# -----------------------------------------------------------

# Distribuição das categorias de dívida de sono

p_divida <- dados %>%
  count(sleep_debt_category) %>%
  ggplot(aes(x = reorder(sleep_debt_category, n), y = n,
             fill = sleep_debt_category)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Distribuição das Categorias de DÍvida de Sono",
       x = "Categoria", y = "Frequência")
salvar(p_divida, "03_divida_sono")

# Frequência de todas as demais categóricas

p_cat <- categoricas %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "categoria") %>%
  count(variavel, categoria) %>%
  ggplot(aes(x = reorder(categoria, n), y = n)) +
  geom_col(fill = "red") +
  coord_flip() +
  facet_wrap(~ variavel, scales = "free") +
  labs(title = "Frequência das Categorias", x = NULL, y = "Contagem")
salvar(p_cat, "04_categoricas", 14, 9)
  
# -----------------------------------------------------
# Corrrelação entre Variávies Numéricas
# -----------------------------------------------------

## OBS.: Spearman é mais robusta a outliers e a relações não lineares que Pearson.

corr_matrix <- cor(numericas, use = "pairwise.complete.obs", method = "spearman")

## Heatmap apenas com o triângulo superior (sem diagonal nem repetição)

corr_df <- as.data.frame(as.table(corr_matrix)) %>%
  filter(match(Var1, colnames(corr_matrix)) <
           match(Var2, colnames(corr_matrix)))

p_corr <- ggplot(corr_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-1,1), name = "Spearman") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Matriz de Correlação (Spearman)", x = NULL, y = NULL)
salvar(p_corr, "05_correlacao", 11, 10)

# Ranking das correlações com as variáveis de interesse
top_cor <- function(alvo) {
  tibble(variavel = names(corr_matrix[, alvo]),
         correlacao = round(corr_matrix[, alvo], 3)) %>%
    filter(variavel != alvo) %>%
    arrange(desc(abs(correlacao)))
}

cat("\nCorrelações com next_day_fatigue_score:\n")
print(top_cor("next_day_fatigue_score"), n = Inf)
cat("\nCorrelações com sleep_efficiency_pct:\n")
print(top_cor("sleep_efficiency_pct"), n = Inf)

# ----------------------------------------------------
# Relações entre Pares de Variáveis
# ----------------------------------------------------

# Função que desenha a dispersão com duas tendências (linear em vermelho e
# loess em azul) e mostra o coeficiente de Spearman no subtítulo. Se as duas
# curvas divergem, a relação não é linear.

grafico_dispersao <- function(dados, x, y, titulo, xlab, ylab) {
  rho <- cor(dados[[x]], dados[[y]], use = "complete.obs", method = "spearman")
  ggplot(dados, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +
    geom_smooth(method = "loess", formula = y ~ x, color = "blue", se = TRUE) +
    labs(title = titulo,
         subtitle = paste0("Spearman rho = ", round(rho, 3),
                           "  |  vermelho: linear, azul: loess"),
         x = xlab, y = ylab)
}


# Tempo de tela x Fadiga

salvar(grafico_dispersao(dados, "screen_time_before_bed_min", "next_day_fatigue_score",
                         "Tempo de Tela Antes de Dormir x Fadiga",
                         "Tempo de Tela (min)", "Fadiga no Dia Seguinte"),
       "06_tela_fadiga")

# Cafeína x Latência do sono

salvar(grafico_dispersao(dados, "caffeine_intake_mg", "sleep_onset_latency_min",
                         "Cafeína x Latência do Sono",
                         "Cafeína (mg)", "Tempo para Adormecer (min)"),
       "07_cafeina_latencia")

# Horas de sono x Fadiga

salvar(grafico_dispersao(dados, "total_sleep_hours", "next_day_fatigue_score",
                         "Horas de Sono x Fadiga",
                         "Horas de Sono", "Fadiga"),
       "08_sono_fadiga")


# -----------------------------------------------------------
# Comparações entre Grupos
# ------------------------------------------------------------

# Filtro de luz azul x Eficiência do sono

p_filtro <- ggplot(dados %>% filter(!is.na(blue_light_filter_on)),
                   aes(x = blue_light_filter_on, y = sleep_efficiency_pct,
                       fill = blue_light_filter_on)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Filtro de Luz Azul e Eficiência do Sono",
       x = "Filtro Ativado", y = "Eficiência do Sono (%)")
salvar(p_filtro, "09_filtro_luz_azul")

# Resumo com n e desvio, não só a média

dados %>%
  group_by(blue_light_filter_on) %>%
  summarise(
    n = n(),
    media_eficiencia = mean(sleep_efficiency_pct, na.rm = TRUE),
    dp_eficiencia    = sd(sleep_efficiency_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()


# Teste: a diferença entre os grupos é maior do que o acaso explicaria?

print(wilcox.test(sleep_efficiency_pct ~ blue_light_filter_on, data = dados))


# Cronotipo x Horário de dormir

p_cron <- ggplot(dados, aes(x = chronotype, y = bedtime_hour, fill = chronotype)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Cronotipo x Horário de Dormir",
       x = "Cronotipo", y = "Horário de Dormir (h; 24 = meia-noite, 25 = 1h)")
salvar(p_cron, "10_cronotipo_horario")
print(kruskal.test(bedtime_hour ~ chronotype, data = dados))

# Categoria do aplicativo x Eficiência do sono 

p_app <- ggplot(dados, aes(x = primary_app_category, y = sleep_efficiency_pct,
                           fill = primary_app_category)) +
  geom_boxplot(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Categoria do Aplicativo x Eficiência do Sono",
       x = NULL, y = "Eficiência do Sono (%)")
salvar(p_app, "11_app_eficiencia")
print(kruskal.test(sleep_efficiency_pct ~ primary_app_category, data = dados))

# ---------------------------------------------------------------
# Resumo por Categoria de dívida de Sono
# ---------------------------------------------------------------

# Atenção: se sleep_debt_category foi calculada a partir de total_sleep_hours
# (ou outra variável abaixo), a comparação dessa variável entre as categorias
# é circular e não traz informação nova.
resumo_divida <- dados %>%
  group_by(sleep_debt_category) %>%
  summarise(
    n = n(),
    media_sono       = mean(total_sleep_hours, na.rm = TRUE),
    min_sono         = min(total_sleep_hours, na.rm = TRUE),
    max_sono         = max(total_sleep_hours, na.rm = TRUE),
    media_fadiga     = mean(next_day_fatigue_score, na.rm = TRUE),
    dp_fadiga        = sd(next_day_fatigue_score, na.rm = TRUE),
    media_tela       = mean(screen_time_before_bed_min, na.rm = TRUE),
    media_eficiencia = mean(sleep_efficiency_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))
print(resumo_divida)

print(kruskal.test(next_day_fatigue_score ~ sleep_debt_category, data = dados))


