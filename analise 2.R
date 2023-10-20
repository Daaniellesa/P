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

quadro_resumo <- limpos %>% 
  group_by(Marca) %>% 
  summarize(Média = round(mean(Preço),2),
            `Desvio Padrão` = round(sd(Preço),2),
            `Variância` = round(var(Preço),2),
            `Mínimo` = round(min(Preço),2),
            `1º Quartil` = round(quantile(Preço, probs = .25),2),
            Mediana = round(quantile(Preço, probs = .5),2),
            `3º Quartil` = round(quantile(Preço, probs = .75),2),
            `Máximo` = round(max(Preço),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",","))
quadro_resumo  

