# ==============================================================================
# EXPLORANDO OS PINGUINS DA ANTÁRTICA COM POSITRON (VERSÃO RESUMIDA)
# ==============================================================================
# Curso: Positron IDE - R-Ladies Goiânia
# Dataset: Palmer Penguins

# HISTÓRIA: Entre 2007 e 2009, a Dra. Kristen Gorman e sua equipe estudaram
# três espécies de pinguins em ilhas da Antártica.

# PERGUNTA CENTRAL: Será que conseguimos distinguir as espécies apenas 
# observando suas medidas corporais?
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PREPARAÇÃO DO AMBIENTE
# ------------------------------------------------------------------------------

# Instalar pacotes (rode apenas uma vez!)
install.packages("tidyverse")


# Carregar pacotes
library(tidyverse)

# ------------------------------------------------------------------------------
# 2. CONHECENDO OS DADOS
# ------------------------------------------------------------------------------
# Importar dados do pacote palmerpenguins
penguins <- read.csv("dados/penguins.csv")

# Visualizar no Data Explorer do Positron (DESTAQUE!)
View(penguins)

# No Data Explorer:
# - Clique nos cabeçalhos para ordenar
# - Use a barra de filtros
# - Veja estatísticas no painel lateral

# Filtro inicial

library(dplyr)

filtro_inicial<- penguins |>
  filter(
    grepl("Adelie", species, fixed = TRUE),
    grepl("Biscoe", island, fixed = TRUE),
    bill_length_mm >= 32 & bill_length_mm <= 42,
    year == 2007)

# Visão geral
glimpse(penguins)

# 344 pinguins, 3 espécies (Adelie, Chinstrap, Gentoo)
# Medidas: bico, nadadeira, peso, sexo, ilha, ano

# ------------------------------------------------------------------------------
# 3. QUAL ESPÉCIE É MAIOR?
# ------------------------------------------------------------------------------

# Peso médio por espécie

peso_por_especie <- penguins |>
  group_by(species) |> 
  summarise(
    peso_medio = mean(body_mass_g, na.rm = TRUE),
    n_pinguins = n()) |> 
  arrange(desc(peso_medio))

peso_por_especie

# RESULTADO: Gentoo são os mais pesados (~5000g)!

# ------------------------------------------------------------------------------
# 4. VISUALIZAÇÃO 1: NADADEIRA x PESO
# ------------------------------------------------------------------------------

# Relação entre tamanho da nadadeira e peso corporal

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(
    title = "Relação entre tamanho da nadadeira e peso corporal",
    x = "Comprimento da nadadeira (mm)",
    y = "Peso corporal (g)",
    color = "Espécie", 
    caption = "R-Ladies Goiânia, 2025.") +
  scale_color_manual(values = c("#88398a", "#224573", "#333333")) +
  theme_classic()

# Correlação positiva - nadadeira maior = pinguim mais pesado

# ------------------------------------------------------------------------------
# 5. VISUALIZAÇÃO 2: FORMATO DO BICO (RESPONDE A PERGUNTA!)
# ------------------------------------------------------------------------------

# Morfologia do bico por espécie

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, 
                     color = species, shape = species)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "Morfologia do bico por espécie",
    x = "Comprimento do bico (mm)",
    y = "Profundidade do bico (mm)",
    color = "Espécie",
    caption = "R-Ladies Goiânia, 2025.") +
  scale_color_manual(values = c("#88398a", "#224573", "#333333")) +
  theme_classic()

# RESPOSTA: SIM! As espécies formam clusters distintos
# - Adelie: bico curto e profundo
# - Chinstrap: bico longo e profundo
# - Gentoo: bico longo e raso

# ------------------------------------------------------------------------------
# 6. VISUALIZAÇÃO 3: MACHOS x FÊMEAS
# ------------------------------------------------------------------------------

# Dimorfismo sexual

penguins |> 
  filter(!is.na(sex)) |> 
  ggplot(aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Dimorfismo sexual",
    subtitle = "Machos são mais pesados em todas as espécies",
    x = "Espécie",
    y = "Peso corporal (g)",
    fill = "Sexo", 
    caption = "R-Ladies Goiânia, 2025.") +
  scale_fill_manual(
    values = c("female" = "#88398a", "male" = "#224573"),
    labels = c("Fêmea", "Macho")) +
  theme_classic()


# ------------------------------------------------------------------------------
# 7. EXPORTAR DADOS (para usar no Quarto!)
# ------------------------------------------------------------------------------

# Salvar análise resumida
write.csv(peso_por_especie, "outputs/peso_por_especie.csv", row.names = FALSE)

# Salvar dataset completo
write.csv(penguins, "outputs/penguins_teste.csv", row.names = FALSE)

# ==============================================================================
# PRÓXIMO PASSO: Criar relatório Quarto com estes resultados!
# ==============================================================================

