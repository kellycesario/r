# Instalação de pacotes

install.packages("ggplot2")
install.packages("dplyr")
install.packages("binom")
install.packages("lme4")
install.packages("Matrix")
install.packages("lmerTest")
install.packages("sjPlot")
install.packages("readr")

# Inicialização de pacotes

library(ggplot2)
library(dplyr)
library(binom)
library(lme4)
library(Matrix)
library(lmerTest)
library (sjPlot)
library(readr)

data <- read_csv("data.csv")
View(data)

data<- data %>% mutate_if(sapply(data, is.character), as.factor)
data$Escolha = as.factor(data$Escolha)
data$Participante = as.factor(data$Participante)
data$Perfil = as.factor(data$Perfil)
data$VLT = as.factor(data$VLT)
str(data)

data = data%>%
  filter(Escolha == "1" | Escolha == "2")%>%
  droplevels

data %>%
  filter(VLT == 2) %>%
  group_by(Condicao, Escolha) %>%
  dplyr::summarise(quantidade = n()) %>%
  ggplot(., aes(x = Condicao, y = quantidade, fill = Escolha)) + 
  ylab(" ") +
  xlab(" ") +
  theme_minimal(base_size = 11, base_family = "Garamond") +
  geom_col() +
  scale_fill_manual(values = c("#101419", "#476C9B"),  breaks = c(1, 2),  labels = c("Correct", "Incorrect")) +
  labs(fill = " ")


# GLM

modelo <- glm(Escolha ~ VLT, data, family = binomial)
tab_model(modelo)

# Frequência, contingência, qui quadrado
data_filtered <- filter(data, (VLT == 2))
frequencia_sim <- sum(data_filtered$Escolha == "1")
frequencia_nao <- sum(data_filtered$Escolha == "2")
proporcao_sim_nao <- frequencia_sim / frequencia_nao

tabela_contingencia <- table(data_filtered$Perfil, data_filtered$Escolha)
resultado_teste <- chisq.test(tabela_contingencia)
print(resultado_teste)

