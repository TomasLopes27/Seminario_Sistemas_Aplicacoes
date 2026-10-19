# =============================================================================
# Momento 3 -- Analise Exploratoria de Dados (EDA)
# Precos de Combustiveis em Portugal: Padroes, Correlacoes e Fatores Explicativos
# =============================================================================

library(tidyverse)
library(lubridate)
library(scales)

# -----------------------------------------------------------------------
# 1. Importacao dos dados
# -----------------------------------------------------------------------

dados <- read_csv(
  "data/processed/dataset_consolidado.csv",
  col_types = cols(
    data = col_date(format = "%Y-%m-%d"),
    combustivel = col_character(),
    preco_eur_l = col_double(),
    isp_produtos_petroliferos_eur_l = col_double(),
    contribuicao_servico_rodoviario_eur_l = col_double(),
    taxa_carbono_eur_l = col_double(),
    isp_total_eur_l = col_double(),
    brent_usd_barril = col_double(),
    brent_eur_barril = col_double(),
    brent_eur_litro = col_double()
  )
)

dados <- dados %>%
  mutate(
    tipo = case_when(
      str_detect(combustivel, "^gasoleo") ~ "Gasoleo",
      str_detect(combustivel, "^gasolina") ~ "Gasolina 95"
    ),
    variante = case_when(
      str_detect(combustivel, "especial") ~ "Especial",
      str_detect(combustivel, "simples") ~ "Simples"
    ),
    peso_isp_pct = isp_total_eur_l / preco_eur_l * 100,
    ano = year(data)
  )

cat("Periodo:", as.character(min(dados$data)), "a", as.character(max(dados$data)), "\n")
cat("Numero de observacoes:", nrow(dados), "\n")
cat("Combustiveis:", paste(unique(dados$combustivel), collapse = ", "), "\n\n")

# -----------------------------------------------------------------------
# 2. Estatisticas descritivas por combustivel
# -----------------------------------------------------------------------

estatisticas <- dados %>%
  group_by(combustivel) %>%
  summarise(
    n = n(),
    preco_min = min(preco_eur_l),
    preco_max = max(preco_eur_l),
    preco_medio = mean(preco_eur_l),
    preco_mediana = median(preco_eur_l),
    preco_dp = sd(preco_eur_l),
    isp_medio = mean(isp_total_eur_l),
    peso_isp_medio_pct = mean(peso_isp_pct),
    .groups = "drop"
  )

print(estatisticas)
write_csv(estatisticas, "analysis/estatisticas_descritivas.csv")

estatisticas_brent <- dados %>%
  distinct(data, brent_eur_litro) %>%
  summarise(
    brent_min = min(brent_eur_litro),
    brent_max = max(brent_eur_litro),
    brent_medio = mean(brent_eur_litro),
    brent_dp = sd(brent_eur_litro)
  )
print(estatisticas_brent)

# -----------------------------------------------------------------------
# 3. Evolucao do preco ao longo do tempo (por combustivel)
# -----------------------------------------------------------------------

p1 <- ggplot(dados, aes(x = data, y = preco_eur_l, color = combustivel)) +
  geom_line(linewidth = 0.4) +
  labs(
    title = "Evolucao do preco dos combustiveis em Portugal (2008-2026)",
    x = "Data", y = "Preco (EUR/litro)", color = "Combustivel"
  ) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("analysis/figures/01_evolucao_precos.png", p1, width = 10, height = 6, dpi = 150)

# -----------------------------------------------------------------------
# 4. Comparacao Brent vs preco final (dupla escala / facet)
# -----------------------------------------------------------------------

brent_serie <- dados %>% distinct(data, brent_eur_litro)

p2 <- ggplot() +
  geom_line(data = dados, aes(x = data, y = preco_eur_l, color = combustivel), linewidth = 0.3) +
  geom_line(data = brent_serie, aes(x = data, y = brent_eur_litro), color = "black", linewidth = 0.5, linetype = "dashed") +
  labs(
    title = "Preco dos combustiveis vs. Brent (convertido para EUR/litro)",
    subtitle = "Linha preta tracejada = Brent (EUR/litro)",
    x = "Data", y = "EUR/litro", color = "Combustivel"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("analysis/figures/02_preco_vs_brent.png", p2, width = 10, height = 6, dpi = 150)

# -----------------------------------------------------------------------
# 5. Correlacao Brent vs preco final, por combustivel
# -----------------------------------------------------------------------

correlacoes <- dados %>%
  group_by(combustivel) %>%
  summarise(
    correlacao_brent_preco = cor(brent_eur_litro, preco_eur_l, use = "complete.obs"),
    .groups = "drop"
  )
print(correlacoes)
write_csv(correlacoes, "analysis/correlacoes_brent_preco.csv")

p3 <- ggplot(dados, aes(x = brent_eur_litro, y = preco_eur_l, color = combustivel)) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~combustivel, scales = "free") +
  labs(
    title = "Relacao entre o preco do Brent (EUR/litro) e o preco final ao consumidor",
    x = "Brent (EUR/litro)", y = "Preco final (EUR/litro)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

ggsave("analysis/figures/03_correlacao_brent_preco.png", p3, width = 10, height = 7, dpi = 150)

# -----------------------------------------------------------------------
# 6. Peso do ISP no preco final ao longo do tempo
# -----------------------------------------------------------------------

p4 <- ggplot(dados, aes(x = data, y = peso_isp_pct, color = combustivel)) +
  geom_line(linewidth = 0.4) +
  labs(
    title = "Peso do ISP no preco final ao consumidor (%)",
    x = "Data", y = "ISP / Preco final (%)", color = "Combustivel"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("analysis/figures/04_peso_isp.png", p4, width = 10, height = 6, dpi = 150)

# -----------------------------------------------------------------------
# 7. Distribuicao dos precos (boxplot por combustivel e por ano)
# -----------------------------------------------------------------------

p5 <- ggplot(dados, aes(x = combustivel, y = preco_eur_l, fill = combustivel)) +
  geom_boxplot() +
  labs(
    title = "Distribuicao dos precos por combustivel (2008-2026)",
    x = NULL, y = "Preco (EUR/litro)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("analysis/figures/05_boxplot_precos.png", p5, width = 8, height = 6, dpi = 150)

p6 <- ggplot(dados, aes(x = factor(ano), y = preco_eur_l)) +
  geom_boxplot(outlier.size = 0.4) +
  facet_wrap(~combustivel, ncol = 2) +
  labs(
    title = "Distribuicao anual dos precos por combustivel",
    x = "Ano", y = "Preco (EUR/litro)"
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave("analysis/figures/06_boxplot_anual.png", p6, width = 12, height = 7, dpi = 150)

cat("\nEDA concluida. Figuras guardadas em analysis/figures/\n")
