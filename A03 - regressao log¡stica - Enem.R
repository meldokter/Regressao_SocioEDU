# A03 - Regressão logística

# Variável dependente (a ser explicada): binária com valores 0 e 1

# 1. Setup ---------------------------------------------------------------------

# Remove notação científica
options(scipen = 999)

# Limpando a memória
rm(list=ls()) # Remove all objects - remove objetos
gc()          # Garbage Clean - limpeza de lixos

# Caminho simplificado para carregar pacotes (PACMAN)
#install.packages(pacman)
library(pacman)
p_load(tidyverse,ggplot2,plotly,rio,
       pROC,ROCR,stargazer,caret)

# 2. Importar base de dados ----------------------------------------------------

# Saeb 2019
enem_2021_sp <- import("0_data/ENEM/mydata_enem_2021_sp.csv")

# 3. Manuseio dos dados --------------------------------------------------------

# Já fizemos

mydata <- mutate(.data = enem_2021_sp,
                 
                 educMae_sd = scale(educMae),
                 rendFamP_sd = scale(rendFamP),
                 depAdmin2 = case_when(tpEscola == "Publica" ~ 0,
                                       tpEscola == "Privada" ~ 1),
                 faixas_rendFamP = case_when(
                   rendFamP < 1500 ~ "1. Baixa",
                   rendFamP >= 1500 & rendFamP < 5000 ~ "2. Média",
                   rendFamP > 5000 ~ "3. Alta"
                 )
)

# Evento: nota para ingresso ser maior que o mínimo necessário
# para ingresso na USP (corte arbitrário = 700)

corte = 700

mydata$NPI <- ifelse(mydata$media > corte,1,0)

# Variável dependente: Nota Para Ingresso (NPI)
# 1 = "Sim"
# 0 = "Não"

# Variáveis independentes:
# 1. escolaridade da mãe
# 2. renda familiar per capta
# 3. tipo de escola (dep Admin)

# Análise exploratória ---------------------------------------------------------

# Como se comportam nossas variáveis?

# PARTE CONCEITUAL

#Estabelecendo uma função para a probabilidade de ocorrência de um evento
prob <- function(z){
  prob = 1 / (1 + exp(-z))
}

#Plotando a curva sigmóide teórica de ocorrência de um evento
#para um logito z entre -5 e +5
data.frame(z = -5:5) %>%
  ggplot() +
  stat_function(aes(x = z, color = "Prob. Evento"),
                fun = prob,
                size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "Logito z",
       y = "Probabilidade") +
  theme_bw()

# Histograma da média de desempenho com o corte da nota
mydata %>%
  ggplot(aes(x = media)) +
  theme_bw()+
  geom_histogram(bins = 50,fill="steelblue",alpha=.7)+
  geom_vline(xintercept = corte,lty=2,size=.7,color="red")+
  geom_text(aes(y=1600,x=760,label="NPI = 700"),color="red")+
  labs(x="Média de desempenho",y="Frequência")

# Quantidade de 0 e 1
mydata %>% mutate(NPI = factor(NPI)) %>% drop_na(NPI) %>% 
  ggplot(aes(x = NPI)) +
  theme_bw()+
  geom_bar(alpha=.8)+
  labs(fill="")+
  labs(x="NPI",y="Frequência")

# Regressão logística ----------------------------------------------------------

mydata.2 <- mydata %>% select(NPI,educMae,rendFamP,faixas_rendFamP,
                            educMae_sd,rendFamP_sd,
                            depAdmin2,tpEscola) %>% na.exclude()

# Agora, ao invés de usar a função para os modelos lineares, mobilizaremos
# a função para os modelos lineares generalizados:

# Generalized Linear Models (GLM): função para regressão logística

reg <- glm(formula = NPI ~ educMae + rendFamP + depAdmin2, 
                      data = mydata.2, 
                      family = "binomial")
summary(reg)

# Outras maneiras de apresentar os outputs do modelo
# Função 'stargazer' do pacote 'stargazer'

stargazer(reg, nobs = T, type = "text") # mostra o valor de Log-Likelihood

# Manipulando o objeto de regressão --------------------------------------------

# Podemos extrair informações de interesse:

# Probabilidades preditas pelo modelo
mydata.2$phat <- reg$fitted.values

# Vamos analisar as probabilidades preditas antes de avançar

# Distribuição das probabilidades preditas pelo modelo
mydata.2 %>%
  ggplot(aes(x = phat))+
  theme_bw()+
  geom_histogram(bins = 50)+
  scale_y_continuous(expand = c(0.01,0))+
  scale_x_continuous(expand = c(0.01,0))+
  labs(x="Probabilidade predita pelo modelo",y="Frequência")

# Por que, em geral, a probabilidade predita é baixa? 
# [lembrar discriminação da nota de corte]

# Distrib. de probabilidade por tipo de escola
mydata.2 %>%
ggplot(aes(x = phat))+
  theme_bw()+
  geom_density(aes(fill=tpEscola),bw=0.02,alpha=.7)+
  scale_y_continuous(expand = c(0.01,0))+
  scale_x_continuous(expand = c(0.01,0))+
  labs(fill="Escola",x="Probabilidade predita")

mydata.2 %>%
  ggplot(aes(x = phat))+
  theme_bw()+
  geom_density(aes(fill=faixas_rendFamP),bw=0.02,alpha=.7)+
  scale_y_continuous(expand = c(0.01,0))+
  scale_x_continuous(expand = c(0.01,0))+
  labs(fill="Renda Familiar \n per capta",x="Probabilidade predita")

# Voltando a manipular o objeto de regressão -----------------------------------

# Podemos extrair informações de interesse:

# Coeficientes
reg$coefficients

# Modelo
modelo <- reg$model

# Probabilidades preditas
modelo$phat <- reg$fitted.values

modelo <- relocate(.data = modelo,
                   NPI,phat,
                   .after = depAdmin2)
view(modelo)

# valor de Log-Likelihood (LL)
logLik(reg)

# Fazendo predições para o modelo

#Exemplo: qual a probabilidade média de ter nota para ingressar na Usp 
# se a mãe tiver escolaridade 16 e a renda ser 10000
educMae = c(2,12,16,17)
rendFamP = c(500,2500,5000,30000) 
depAdmin2 = c(0,1,1,1)

dados_predicao <- data.frame(educMae,rendFamP,depAdmin2)

dados_predicao$prob_predita = predict(object = reg, 
                                      newdata = dados_predicao, 
                                      type = "response")

view(dados_predicao)

#Matriz de confusão para cutoff = 0.10
# Interpretação: para casos em que a predição com o modelo é de probabilidade
# maior que 0.10, considere que o indivíduo tem NPI == 1

predito <- predict(reg, type = "response") >= 0.10

observado <- mydata.2$NPI == 1

m_confus <- table(predito,observado)[2:1, 2:1]
m_confus

data.frame(predito,observado) %>% head()

#(função confusionMatrix do pacote caret)
confusionMatrix(m_confus)

#função roc do pacote pROC
ROC <- roc(response = mydata.2$NPI, 
           predictor = reg$fitted.values)

plot(ROC)
