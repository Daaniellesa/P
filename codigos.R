vendas <- read.csv("vendas.csv")
library(tidyverse)

cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}




ggplot(vendas, aes(y = Price , x = Brand)) +
  geom_boxplot()

ggplot(vendas) +
  aes(x = Brand, y = Price) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Marca", y = "Preço") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")



ggplot(dados_limpos) +
  aes(x = Brand, y = Category) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Marca",
    y = "Categoria"
  ) +
  theme_estat()

vendas %>%
  filter(!is.na(Category) & !is.na(Brand))

ggplot(vendas, aes(x = Category, fill = Brand)) +
  geom_bar(position = "dodge")

categoria <- vendas[!(vendas$Category == "Kids'Fashion" | is.na(vendas$Category)), ]

dados_limpos <- vendas %>%
  filter(!(Category == "Kids' Fashion" | is.na(Category)))


ggplot(dados_limpos, aes(x = Category, fill = Brand)) +
  geom_bar()


ggplot(dados_limpos) +
  aes(x = Brand, y = Category) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Categoria",
    y = "Marca"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")

na <- 

resultados <- chisq.test(dados_limpos$Category, dados_limpos$Brand)
# Certifique-se de que o teste qui-quadrado tenha sido realizado
coef_cramer <- sqrt(resultados$statistic / sum(resultados$observed))
coef_cramer


# Suponha que você tenha um conjunto de dados chamado "seus_dados" com as colunas "TipoTransmissao" e "TipoTracao"

relação <- dados_limpos %>%
  mutate(trans = case_when(
    str_detect(TipoTransmissao, "auto") ~ "auto",
    str_detect(TipoTransmissao, "manual") ~ "manual"
  )) %>%
  group_by(trans, TipoTracao) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = scales::percent(freq / sum(freq))
  )



# Suponha que você tenha um conjunto de dados chamado "vendas" com as colunas "categoria" e "marca"

# Carregue a biblioteca dplyr se ainda não estiver carregada
library(dplyr)

# Crie um novo conjunto de dados com as frequências relativas
analise_vendas <- dados_limpos %>%
  group_by(Category, Brand) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = scales::percent(freq / sum(freq)))

# Visualize o novo conjunto de dados
head(analise_vendas)

# Suponha que você tenha um conjunto de dados chamado "seus_dados" com as colunas "contagem" e "porcentagem"

porcentagens <- str_c(analise_vendas$Brand, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(analise_vendas$Category, " (", porcentagens, ")"))

ggplot(analise_vendas) +
  aes(
    x = fct_reorder(Category, freq, .desc = T), y = freq,
    fill = Brand, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Transmissão", y = "Frequência") +
  theme_estat()
ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")


ggplot(vendas) +
  aes(x = Price, y = Rating) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Preço",
    y = "Avaliação"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")








 

 freq <- # Suponha que você tenha um conjunto de dados chamado "seus_dados" com colunas "Categoria" e "Marca"
   freq <- table(dados_limpos$Category, dados_limpos$Brand)
freq <- dados_limpos %>%
  group_by(Category, Brand) %>%
  summarise(Frequencia = n()
            
            frequencia <- dados_limpos %>%
              group_by(Category, Brand) %>%
              summarise(Frequencia = n())

           
            
            
            
            
           
            
            ggplot(frequencia, aes(x = Category, y = Frequencia, fill = Brand, label = scales::percent(Frequencia / sum(Frequencia)))) +
              geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
              geom_text(
                position = position_dodge(width = 0.9),
                vjust = -0.5,
                size = 3
              ) +
              labs( x = "Categoria", y = "Porcentagem") +
              theme_estat()
            ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")
            
            
            
            
            vendas1 <- vendas %>%
              filter(!is.na(Price) & !is.na(Category))            
            
            
            
           
            
            
            
            
            vendas1$Price <- as.numeric(vendas1$Price)
            rm(faturamento_anual2)
            faturamento_anual <- vendas1 %>%
              group_by(Category) %>%
              summarise(Faturamento_Total = sum(Price))
            
            faturamento <- faturamento_anual %>%
              mutate(Prop = (Faturamento_Total / sum(Faturamento_Total)) * 100)
            rm(dados_limpos)
            
            faturamento$Prop <- round(faturamento$Prop, 2)
            
            
            faturamento <- faturamento %>%
              arrange(desc(Prop)) %>%
              mutate(Posicao = prop - 0.1 * Prop)
            
            
            
            
            
            
            
            
            
            
            faturamento$Posicao <- 1:nrow(faturamento)
            print(faturamento)
            
            
           
            
            ggplot(faturamento, aes(x = 1, y = Prop, fill = Category, label = paste0(Prop, "%"))) +
              geom_bar(width = 1, stat = "identity") +
              geom_text(aes(x = 1.8, label = paste0(Prop, "%")), position = position_stack(vjust = 0.5)) +
              coord_polar(theta = "y") +
              theme_void() +
              theme(legend.position = "top") +
              scale_fill_manual(values = cores_estat, name = 'categoria')
            ggsave("setor.pdf", width = 158, height = 93, units = "mm")
            
           j <- sum(vendas$Price, na.rm = TRUE)
           j
            
           limpos <- limpos %>%
             rename(Marca = Brand)
           limpos <- limpos %>%
             mutate(Categoria = ifelse(Categoria == "Men's Fashion", "Moda Masculins", Categoria))
           limpos <- limpos %>%
             mutate(Categoria = ifelse(Categoria == "Women's Fashion", "Moda Feminina", Categoria))
           limpos <- limpos %>%
             mutate(Categoria = ifelse(Categoria == "Kids' Fashion", "Moda Infantil", Categoria))
           
           
           
           
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
           