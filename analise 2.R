vendas <- read.csv("vendas.csv")
library(tidyverse)

limpos <- vendas %>%
  filter(!is.na(Price) & !is.na(Brand))

ggplot(limpos) +
  aes(x = Marca, y = Preço) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Marca", y = "Preço") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")
