# ============================================================
# Momento 4 - Modelação Estatística (Regressão Linear)
# Seminário de Sistemas e Aplicações - Preços de Combustíveis em Portugal
#
# Objetivo:
#   Modelar o preço por litro de cada tipo de combustível em função do
#   preço do petróleo Brent (EUR/litro) e da carga fiscal (ISP total, EUR/litro).
#
#   Modelo: preco_eur_l ~ brent_eur_litro + isp_total_eur_l
#   (um modelo por tipo de combustível)
#
# Pré-requisito: correr a partir da raiz do repositório
#   (ou definir setwd() para a pasta do projeto)
# ============================================================

library(tidyverse)
library(broom)   # tidy() / glance() para extrair resultados dos modelos
library(scales)

# ------------------------------------------------------------
# 1. Importar dados
# ------------------------------------------------------------
dados <- read_csv("data/processed/dataset_consolidado.csv", show_col_types = FALSE) |>
  mutate(data = as.Date(data))

glimpse(dados)

combustiveis <- unique(dados$combustivel)
cat("Tipos de combustível:", paste(combustiveis, collapse = ", "), "\n")

# ------------------------------------------------------------
# 2. Ajustar um modelo de regressão linear por combustível
# ------------------------------------------------------------
ajustar_modelo <- function(tipo) {
  df <- dados |> filter(combustivel == tipo)
  modelo <- lm(preco_eur_l ~ brent_eur_litro + isp_total_eur_l, data = df)
  list(tipo = tipo, dados = df, modelo = modelo)
}

modelos <- map(combustiveis, ajustar_modelo) |> set_names(combustiveis)

# ------------------------------------------------------------
# 3. Resumo dos modelos (coeficientes, R², p-values)
# ------------------------------------------------------------
resumo_coef <- map_dfr(modelos, function(m) {
  tidy(m$modelo) |> mutate(combustivel = m$tipo)
}, .id = NULL) |>
  relocate(combustivel)

resumo_qualidade <- map_dfr(modelos, function(m) {
  glance(m$modelo) |> mutate(combustivel = m$tipo)
}) |>
  relocate(combustivel) |>
  select(combustivel, r.squared, adj.r.squared, sigma, statistic, p.value, df.residual)

print(resumo_coef, n = Inf)
print(resumo_qualidade, n = Inf)

write_csv(resumo_coef, "analysis/m4_coeficientes_modelos.csv")
write_csv(resumo_qualidade, "analysis/m4_qualidade_modelos.csv")

# ------------------------------------------------------------
# 4. Diagnóstico de resíduos (valores previstos vs. resíduos)
# ------------------------------------------------------------
dir.create("analysis/figures_m4", showWarnings = FALSE, recursive = TRUE)

diagnostico_df <- map_dfr(modelos, function(m) {
  augment(m$modelo) |> mutate(combustivel = m$tipo)
})

# 4.1 Resíduos vs. valores ajustados (homocedasticidade)
p_resid <- ggplot(diagnostico_df, aes(.fitted, .resid)) +
  geom_point(alpha = 0.25, size = 0.8, color = "steelblue") +
  geom_hline(yintercept = 0, color = "firebrick", linetype = "dashed") +
  facet_wrap(~combustivel, scales = "free") +
  labs(
    title = "Diagnóstico do modelo: resíduos vs. valores ajustados",
    x = "Valor ajustado (preço previsto, €/l)",
    y = "Resíduo (€/l)"
  ) +
  theme_minimal(base_size = 11)

ggsave("analysis/figures_m4/residuos_vs_ajustados.png", p_resid,
       width = 9, height = 6, dpi = 150)

# 4.2 QQ-plot dos resíduos (normalidade)
p_qq <- ggplot(diagnostico_df, aes(sample = .resid)) +
  stat_qq(alpha = 0.3, size = 0.6, color = "steelblue") +
  stat_qq_line(color = "firebrick") +
  facet_wrap(~combustivel, scales = "free") +
  labs(title = "QQ-plot dos resíduos", x = "Quantis teóricos", y = "Quantis amostrais") +
  theme_minimal(base_size = 11)

ggsave("analysis/figures_m4/qqplot_residuos.png", p_qq,
       width = 9, height = 6, dpi = 150)

# ------------------------------------------------------------
# 5. Preço real vs. preço previsto (ajuste global do modelo)
# ------------------------------------------------------------
p_ajuste <- ggplot(diagnostico_df, aes(preco_eur_l, .fitted)) +
  geom_point(alpha = 0.25, size = 0.8, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "firebrick", linetype = "dashed") +
  facet_wrap(~combustivel, scales = "free") +
  labs(
    title = "Preço real vs. preço previsto pelo modelo",
    x = "Preço real (€/l)",
    y = "Preço previsto (€/l)"
  ) +
  theme_minimal(base_size = 11)

ggsave("analysis/figures_m4/real_vs_previsto.png", p_ajuste,
       width = 9, height = 6, dpi = 150)

# ------------------------------------------------------------
# 6. Tabela-resumo "pronta a colar" na apresentação (R², coef.)
# ------------------------------------------------------------
tabela_final <- resumo_qualidade |>
  left_join(
    resumo_coef |> filter(term == "brent_eur_litro") |>
      select(combustivel, coef_brent = estimate),
    by = "combustivel"
  ) |>
  left_join(
    resumo_coef |> filter(term == "isp_total_eur_l") |>
      select(combustivel, coef_isp = estimate),
    by = "combustivel"
  ) |>
  select(combustivel, r.squared, coef_brent, coef_isp) |>
  mutate(across(where(is.numeric), ~round(.x, 4)))

print(tabela_final)
write_csv(tabela_final, "analysis/m4_tabela_resumo.csv")

cat("\n==> Concluído. Outputs em analysis/figures_m4/ e analysis/m4_*.csv\n")
