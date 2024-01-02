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

# Gráfico bilíngues (apenas VLT5)

dataB <- read_csv("dataB.csv")
View(dataB)

dataB<- dataB %>% mutate_if(sapply(dataB, is.character), as.factor)
dataB$Resposta = as.factor(dataB$Resposta)
dataB$Participante = as.factor(dataB$Participante)
dataB$ID_Maze = as.factor(dataB$ID_Maze)
str(dataB)

dataB = dataB%>%
  filter(Resposta == "1" | Resposta == "2")%>%
  droplevels

dataB %>%
  filter(VLT == 5) %>%
  group_by(tipo_De_Frase, Resposta) %>%
  dplyr::summarise(quantidade = n()) %>%
  ggplot(., aes(x = tipo_De_Frase, y = quantidade, fill = Resposta)) + 
  ylab(" ") +
  xlab(" ") +
  theme_minimal(base_size = 11, base_family = "Garamond") +
  geom_col() +
  scale_fill_manual(values = c("#101419", "#476C9B"),  breaks = c(1, 2),  labels = c("Correct", "Incorrect")) +
  labs(fill = " ")

dataB %>%
  group_by(ID_Maze, Resposta) %>%
  dplyr::summarise(quantidade = n()) %>%
  ggplot(., aes(x = ID_Maze, y = quantidade, fill = Resposta)) + 
  ylab(" ") +
  xlab(" ") +
  theme_minimal(base_size = 11, base_family = "Garamond") +
  geom_col() +
  scale_fill_manual(values = c("#101419", "#476C9B"),  breaks = c(1, 2),  labels = c("Correct", "Incorrect")) +
  labs(fill = " ")

#exported picture: 947x1017

# Gráfico nativos

dataN <- read_csv("dataN.csv")
View(dataN)

dataN<- dataN %>% mutate_if(sapply(dataN, is.character), as.factor)
dataN$Resposta = as.factor(dataN$Resposta)
dataN$Participante = as.factor(dataN$Participante)
dataN$ID_Maze = as.factor(dataN$ID_Maze)

dataN = dataN%>%
  filter(Resposta == "1" | Resposta == "2")%>%
  droplevels

dataN %>%
  group_by(tipo_De_Frase, Resposta) %>%
  dplyr::summarise(quantidade = n()) %>%
  ggplot(., aes(x = tipo_De_Frase, y = quantidade, fill = Resposta)) + 
  ylab(" ") +
  xlab(" ") +
  theme_minimal(base_size = 11, base_family = "Garamond") +
  geom_col() +
  scale_fill_manual(values = c("#101419", "#476C9B"),  breaks = c(1, 2),  labels = c("Correct", "Incorrect")) +
  labs(fill = " ")

dataN %>%
  group_by(ID_Maze, Resposta) %>%
  dplyr::summarise(quantidade = n()) %>%
  ggplot(., aes(x = ID_Maze, y = quantidade, fill = Resposta)) + 
  ylab(" ") +
  xlab(" ") +
  theme_minimal(base_size = 11, base_family = "Garamond") +
  geom_col() +
  scale_fill_manual(values = c("#101419", "#476C9B"),  breaks = c(1, 2),  labels = c("Correct", "Incorrect")) +
  labs(fill = " ")

# GLM

dataAll <- read_csv("dataAll.csv")
View(dataAll)

dataAll<- dataAll %>% mutate_if(sapply(dataAll, is.character), as.factor)
dataAll$Resposta = as.factor(dataAll$Resposta)
dataAll$Participante = as.factor(dataAll$Participante)
dataAll$ID_Maze = as.factor(dataAll$ID_Maze)
str(dataAll)

dataAll = dataAll%>%
  filter(Resposta == "1" | Resposta == "2")%>%
  droplevels()

data_filtered <- filter(dataAll, (Perfil == 'Nativo') | (VLT == 5 & Perfil == 'Bilingue'))
modelo <- glm(Resposta ~ tipo_De_Frase * Perfil, data_filtered, family = binomial)
tab_model(modelo)

# Frequência, contingência, qui quadrado
frequencia_sim <- sum(data_filtered$Resposta == "1")
frequencia_nao <- sum(data_filtered$Resposta == "2")
proporcao_sim_nao <- frequencia_sim / frequencia_nao

tabela_contingencia <- table(data_filtered$Perfil, data_filtered$Resposta)
print(tabela_contingencia)
resultado_teste <- chisq.test(tabela_contingencia)
print(resultado_teste)

summary(frequencia_sim)
summary(frequencia_nao)
summary(tabela_contingencia)
