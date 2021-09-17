## Pacotes
library(jsonlite)
library(tidyverse)
library(plyr)
library(ggplot2)
library(ggthemes)
library(forecast)
library(seasonal)
library('sidrar')
library(reshape2)
library(dplyr)

#-- Importando os dados
IPCA <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados?formato=json") #433 - ?ndice nacional de pre?os ao consumidor-amplo (IPCA) - Var. % mensal
IPCA_MS <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.4466/dados?formato=json") #?ndice nacional de pre?os ao consumidor-Amplo (IPCA) - N?cleo m?dias aparadas com suaviza??o
IPCA_EX0 <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.11427/dados?formato=json") #?ndice nacional de pre?os ao consumidor - Amplo (IPCA) - N?cleo por exclus?o - EX0
IPCA_EX1 <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.16121/dados?formato=json") #?ndice nacional de pre?os ao consumidor - Amplo (IPCA) - N?cleo por exclus?o - EX1
IPCA_DP <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.16122/dados?formato=json") #?ndice nacional de pre?os ao consumidor - Amplo (IPCA) - N?cleo de dupla pondera??o - DP
SELIC <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.11/dados?formato=json") 
SELIC_META <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.432/dados?formato=json") 
IPCA_DIFUSAO <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.21379/dados?formato=json") #433 - ?ndice nacional de pre?os ao consumidor-amplo (IPCA) - Var. % mensal
IPCA_MON <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.4449/dados?formato=json") #433 - ?ndice nacional de pre?os ao consumidor-amplo (IPCA) - Var. % mensal
IPCA_LIVRE <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.11428/dados?formato=json") #433 - ?ndice nacional de pre?os ao consumidor-amplo (IPCA) - Var. % mensal

#PIB
PIB <- get_sidra(api = '/t/1620/n1/all/v/all/p/all/c11255/90707/d/v583%202') #PIB a preço de mercado, trimestral. Tabela 1620 - Série encadeada do índice de volume trimestral (Base: média 1995 = 100)
PIB_desazonalizado <- get_sidra(api = "/t/1621/n1/all/v/all/p/all/c11255/90707/d/v584%202")#PIB a preço de mercado, trimestral. Tabela 1621 - Série encadeada do índice de volume trimestral com ajuste sazonal (Base: média 1995 = 100)

#Taxa de Juros
SELIC <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.11/dados?formato=json") #Taxa de juros - Selic. (%a.d)
SELIC_META <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.432/dados?formato=json") #Taxa de juros - Meta Selic definida pelo Copom (%a.a)
SELIC_252 <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.4189/dados?formato=json") #	Taxa de juros - Selic acumulada no mês anualizada base 252 (%a.a)

#Saldo de Crédito
SDC <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.20622/dados?formato=json") # Saldo da carteira de crédito em relação ao PIB - %
SDC_PJ <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.20623/dados?formato=json") # Saldo da carteira de crédito a pessoas jurídicas em relação ao PIB - %
SDC_PF <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.20624/dados?formato=json") # Saldo da carteira de crédito a pessoas fisicas em relação ao PIB - %
SDC_recursoslivres <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.20625/dados?formato=json") # Saldo da carteira de crédito com recursos livres em relação ao PIB - %
SDC_recursosdirecionados <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.20628/dados?formato=json")  # Saldo da carteira de crédito com recursos direcionados em relação ao PIB - %

#Inadimplencia do credito
INAD <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.21082/dados?formato=json") #Inadimplência da carteira de crédito - Total - %
INAD_PJ <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.21083/dados?formato=json") #Inadimplência da carteira de crédito - Pessoas jurídicas - Total - %
INAD_PF <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.21084/dados?formato=json") #Inadimplência da carteira de crédito - Pessoas físicas - Total - %
INAD_recursoslivres <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.21085/dados?formato=json") #Inadimplência da carteira de crédito com recursos livres - Total - %
INAD_recursoslivresPJ <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.21086/dados?formato=json") #Inadimplência da carteira de crédito com recursos livres - Pessoas jurídicas - Total - %
INAD_recursoslivresPF <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.21112/dados?formato=json") #  Inadimplência da carteira de crédito com recursos livres - Pessoas físicas - Total - %
INAD_recursosdirecionados <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.21132/dados?formato=json") #Inadimplência da carteira de crédito com recursos direcionados - Total - %
INAD_recursosdirecionadosPJ <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.21133/dados?formato=json") #Inadimplência da carteira de crédito com recursos direcionados - Pessoas jurídicas - Total - %
INAD_recursosdirecionadosPF <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.21134/dados?formato=json") # Inadimplência da carteira de crédito com recursos direcionados - Pessoas físicas - Total - %

#Endividamento
Endividamento1 <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.19881/dados?formato=json") # Comprometimento de renda das famílias com o serviço da dívida com o Sistema Financeiro Nacional - Com ajuste sazonal - % 
Endividamento2 <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.19882/dados?formato=json") # Endividamento das famílias com o Sistema Financeiro Nacional em relação à renda acumulada dos últimos doze meses - %

#Renda Média Mensal
Renda <- get_sidra(api = '/t/5439/n1/all/v/5932/p/all/c12029/99383') #Total no Brasil. Tabela 5439 - Rendimento médio real, habitualmente recebido por mês e efetivamente recebido no mês de referência, do trabalho principal, por posição na ocupação no trabalho principal

#-- Moldando os dados ===============

colnames(IPCA) <- c("data", "IPCA")
colnames(IPCA_MS) <- c("data", "IPCA_MS")
colnames(IPCA_EX0) <- c("data", "IPCA_EX0")
colnames(IPCA_EX1) <- c("data", "IPCA_EX1")
colnames(IPCA_DP) <- c("data", "IPCA_DP")
colnames(SELIC) <- c("data", "SELIC")
colnames(SELIC_META) <- c("data","SELIC_META")
colnames(IPCA_DIFUSAO) <- c("data", "IPCA_DIFUSAO")
colnames(IPCA_MON) <- c("data", "IPCA_MON")
colnames(IPCA_LIVRE) <- c("data", "IPCA_LIVRE")



IPCA$IPCA <- as.numeric(IPCA$IPCA)
IPCA_MS$IPCA_MS <- as.numeric(IPCA_MS$IPCA_MS)
IPCA_EX0$IPCA_EX0 <- as.numeric(IPCA_EX0$IPCA_EX0)
IPCA_EX1$IPCA_EX1 <- as.numeric(IPCA_EX1$IPCA_EX1)
IPCA_DP$IPCA_DP <- as.numeric(IPCA_DP$IPCA_DP)
SELIC$SELIC <- as.numeric(SELIC$SELIC)
SELIC_META$SELIC_META <- as.numeric(SELIC_META$SELIC_META)
IPCA_DIFUSAO$IPCA_DIFUSAO <- as.numeric(IPCA_DIFUSAO$IPCA_DIFUSAO)
IPCA_MON$IPCA_MON <- as.numeric(IPCA_MON$IPCA_MON)
IPCA_LIVRE$IPCA_LIVRE <- as.numeric(IPCA_LIVRE$IPCA_LIVRE)



IPCA$data <- as.Date(IPCA$data,"%d/%m/%Y")
IPCA_MS$data <- as.Date(IPCA_MS$data,"%d/%m/%Y" )
IPCA_EX0$data <- as.Date(IPCA_EX0$data,"%d/%m/%Y" )
IPCA_EX1$data <- as.Date(IPCA_EX1$data,"%d/%m/%Y" )
IPCA_DP$data <- as.Date(IPCA_DP$data,"%d/%m/%Y" )
SELIC$data <- as.Date(SELIC$data, "%d/%m/%Y" )
SELIC_META$data <- as.Date(SELIC_META$data, "%d/%m/%Y" )
IPCA_DIFUSAO$data <- as.Date(IPCA_DIFUSAO$data,"%d/%m/%Y")
IPCA_MON$data <- as.Date(IPCA_MON$data,"%d/%m/%Y")
IPCA_LIVRE$data <- as.Date(IPCA_LIVRE$data,"%d/%m/%Y")


df1 <- merge(IPCA,IPCA_MS,by="data", all = T)
df1 <- merge(df1,IPCA_EX0,by="data", all = T)
df1 <- merge(df1,IPCA_EX1,by="data", all = T)
df1 <- merge(df1,IPCA_DP,by="data", all = T)
df1 <- merge(df1,IPCA_DIFUSAO,by="data", all = T)
df1 <- merge(df1,IPCA_MON,by="data", all = T)
df1 <- merge(df1,IPCA_LIVRE,by="data", all = T)


df1$IPCA[is.na(df1$IPCA)] <- c(0.43,0.75,0.57,0.13,0.01,0.19) #!!!!!!!!!!!! Corrigindo os valores devido ao problema do API para a s?rie 433. Verifique se h? 469 elementos na variavel IPCA antes de aplicar essa formula
#-- Obtendo resultados atrav?s dos dados obtidos ------------------------------------


#Gerando a variavel base (fator multiplicacao) para variacao percentual acumulada em 12 meses

#-------------------------- Fator Multiplicacao ------------------------------------
fator <- df1%>%
  mutate(IPCA_fator = 1+(IPCA/100))%>% 

  mutate(IPCA_MS_fator = (1+(IPCA_MS/100)))%>% 

  mutate(IPCA_EX0_fator = (1+(IPCA_EX0/100)  ))%>% 

  mutate(IPCA_EX1_fator = ( 1+(IPCA_EX1/100)  ))%>% 

  mutate(IPCA_DP_fator = ( 1+(IPCA_DP/100) )) %>%
  
  mutate(IPCA_LIVRE_fator = ( 1+(IPCA_LIVRE/100) )) %>%
  
  mutate(IPCA_MON_fator = ( 1+(IPCA_MON/100) ))



#--- Gerando a variavel Variacao Percentual Acumulada em 12 meses

#-------------------------- Var.%.Ac. 12 Meses ------------------------------------------

#IPCA
IPCA_ac <- 1:(length(fator$IPCA_fator)-12) #Varia??o Percentual Acumulada em 12 meses, atrav?s do Fator Multiplicacao
for(i in 1:(length(fator$IPCA_fator)-12)){
  IPCA_ac[i] <- (last(cumprod(fator$IPCA_fator[(i+1):(i+12)]))-1)*100
}
IPCA_ac <- append(rep(0,12),IPCA_ac)

tail(IPCA_ac,50)

#IPCA_MS
IPCA_MS_ac <- 1:(length(fator$IPCA_MS_fator)-12) #Varia??o Percentual Acumulada em 12 meses, atrav?s do Fator Multiplicacao
for(i in 1:(length(fator$IPCA_MS_fator)-12)){
  IPCA_MS_ac[i] <- (last(cumprod(fator$IPCA_MS_fator[(i+1):(i+12)]))-1)*100
}
IPCA_MS_ac <- append(rep(0,12),IPCA_MS_ac)

tail(IPCA_MS_ac,50)
#IPCA_EX0

IPCA_EX0_ac <- 1:(length(fator$IPCA_EX0_fator)-12) #Varia??o Percentual Acumulada em 12 meses, atrav?s do Fator Multiplicacao
for(i in 1:(length(fator$IPCA_EX0_fator)-12)){
  IPCA_EX0_ac[i] <- (last(cumprod(fator$IPCA_EX0_fator[(i+1):(i+12)]))-1)*100
}
IPCA_EX0_ac <- append(rep(0,12),IPCA_EX0_ac)

tail(IPCA_EX0_ac,50)

#IPCA_EX1

IPCA_EX1_ac <- 1:(length(fator$IPCA_EX1_fator)-12) #Varia??o Percentual Acumulada em 12 meses, atrav?s do Fator Multiplicacao
for(i in 1:(length(fator$IPCA_EX1_fator)-12)){
  IPCA_EX1_ac[i] <- (last(cumprod(fator$IPCA_EX1_fator[(i+1):(i+12)]))-1)*100
}
IPCA_EX1_ac <- append(rep(0,12),IPCA_EX1_ac)

tail(IPCA_EX1_ac,50)

#IPCA_DP

IPCA_DP_ac <- 1:(length(fator$IPCA_DP_fator)-12) #Varia??o Percentual Acumulada em 12 meses, atrav?s do Fator Multiplicacao
for(i in 1:(length(fator$IPCA_DP_fator)-12)){
  IPCA_DP_ac[i] <- (last(cumprod(fator$IPCA_DP_fator[(i+1):(i+12)]))-1)*100
}
IPCA_DP_ac <- append(rep(0,12),IPCA_DP_ac)

tail(IPCA_DP_ac,50)

#IPCA_LIVRE

IPCA_LIVRE_ac <- 1:(length(fator$IPCA_LIVRE_fator)-12) #Varia??o Percentual Acumulada em 12 meses, atrav?s do Fator Multiplicacao
for(i in 1:(length(fator$IPCA_LIVRE_fator)-12)){
  IPCA_LIVRE_ac[i] <- (last(cumprod(fator$IPCA_LIVRE_fator[(i+1):(i+12)]))-1)*100
}
IPCA_LIVRE_ac <- append(rep(0,12),IPCA_LIVRE_ac)

tail(IPCA_LIVRE_ac,50)

#IPCA_MON

IPCA_MON_ac <- 1:(length(fator$IPCA_MON_fator)-12) #Varia??o Percentual Acumulada em 12 meses, atrav?s do Fator Multiplicacao
for(i in 1:(length(fator$IPCA_MON_fator)-12)){
  IPCA_MON_ac[i] <- (last(cumprod(fator$IPCA_MON_fator[(i+1):(i+12)]))-1)*100
}
IPCA_MON_ac <- append(rep(0,12),IPCA_MON_ac)

tail(IPCA_MON_ac,50)


#Criando o Data Frame para variacao acumulada em 12 meses

ac_12meses <- df1%>%
  mutate(IPCA_ac = IPCA_ac)%>%
  mutate(IPCA_EX0_ac = IPCA_EX0_ac)%>%
  mutate(IPCA_EX1_ac = IPCA_EX1_ac)%>%
  mutate(IPCA_DP_ac = IPCA_DP_ac)%>%
  mutate(IPCA_MS_ac = IPCA_MS_ac)%>%
  mutate(IPCA_LIVRE_ac = IPCA_LIVRE_ac)%>%
  mutate(IPCA_MON_ac = IPCA_MON_ac)
   

teste <- ac_12meses%>%
  mutate(a = rep(0,478))%>%
  select(IPCA_EX0_ac:a)
  
teste <- ac_12meses%>%
  mutate(a = rep(0,478))%>%
  select(IPCA_EX0_ac:a)%>%
  mutate( a=  rowMeans(teste, na.rm = FALSE, dim = 1L) )



#Gráfico 1) -----------------------------------------------------------------------

#Ajeitando os numeros....

q1 <- ac_12meses%>%
  mutate( media = teste$a)%>%
  mutate( metas = c(rep(4.5,nrow(ac_12meses)))) %>%
  subset( data >="2000-01-01" & data <= "2019-07-01")


#Fa?a um gr?fico para verificar o comportamento da infla??o e dos nucleos em todo o per?odo:
ggplot(q1, aes(x = data, y = IPCA_ac))+
  geom_line(aes(y = q1$IPCA_ac, colour = "IPCA")) + 
  geom_line(aes(y = q1$metas ,colour = "Meta de Infla??o")) +
  geom_line(aes(y = q1$media ,colour = "M?dia dos N?cleos")) +
  
  theme(legend.position=c(0.9, 0.9))+
  
  geom_vline(xintercept = as.Date("2003-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2007-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2011-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2016-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2016-08-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2019-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  
  annotate("text", x = as.Date("2003-01-01"), y = 18, label = "Lula 1")+
  annotate("text", x = as.Date("2007-01-01"), y = 18, label = "Lula 2")+
  annotate("text", x = as.Date("2011-01-01"), y = 18, label = "Dilma 1")+
  annotate("text", x = as.Date("2016-01-01"), y = 18, label = "Dilma 2")+
  annotate("text", x = as.Date("2016-08-01"), y = 16, label = "Temer")+
  annotate("text", x = as.Date("2019-01-01"), y = 18, label = "Bolsonaro")+
  
  
  
  
    labs(title ="Gr?fico I - Infla??o no Brasil",
       y = "?ndice acumulado em 12 meses (%)",
        x ="Anos",
       subtitle = "De 2000 at? 2019 ",
       caption = "Fonte: BCB") +
  
  theme_economist() +scale_colour_economist()
  
  
  
  
  
#---------- questao 2

#Grafico 2
ggplot(q1, aes(x = data))+
  geom_line(aes( y = ))+
  labs(title ="Gr?fico II - IPCA em 2019  ",
       y = "?ndice acumulado em 12 meses (%)",
       x ="Meses",
       subtitle = "De Janeiro at? Julho",
       caption = "Fonte: BCB") +
  
  theme_economist() +scale_colour_economist()

ggplot(q1, aes(x = data, y = IPCA_EX1_ac))+
  geom_line()+
  labs(title ="Gr?fico III - N?cleo por exclus?o (1) IPCA",
       y = "IPCA-EX1 acumulado em 12 meses(%)",
       x ="Anos",
       subtitle = "De 2000 at? 2019 ",
       caption = "Fonte: BCB") +
  
  theme_economist() +scale_colour_economist()


ggplot(q1, aes(x = data, y = IPCA_DP_ac))+
  geom_line()+
  labs(title ="Gr?fico IV - N?cleo de Dupla Pondera??o IPCA",
       y = "IPCA-DP acumulado em 12 meses(%)",
       x ="Anos",
       subtitle = "De 2000 at? 2019 ",
       caption = "Fonte: BCB") +
  
  theme_economist() +scale_colour_economist()


max(q1$IPCA_ac, na.rm = TRUE)
min(q1$IPCA_ac, na.rm = TRUE)
#------------- questao 1.2 -------------------
#Para o per?odo mais recente

q2 <- q1%>%
  subset( data >="2019-01-01" & data <= "2019-07-01")

ggplot(q2, aes(x = data))+
  geom_line(aes(y = IPCA_ac, colour = "IPCA" ))+
  geom_line(aes(y = media, colour = "M?dia dos N?cleos" ))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m")+
  expand_limits(y=c(2.75,5.75,6))+
  geom_hline(yintercept = 5.75, colour = "red")+
  geom_hline(yintercept = 2.75, colour = "blue")+
  labs(title ="Gr?fico II - ?ndice IPCA",
       y = "IPCA acumulado em 12 meses (%)",
       x ="meses",
       subtitle = "De Janeiro at? Julho ",
       caption = "Fonte: BCB") +
  
  theme_economist() +scale_colour_economist()
#-- Gráfico 2-----------------------

ggplot(q1, aes(x = data))+
  geom_line(aes(y = q1$IPCA_EX0_ac), colour = "red")+
  geom_line(aes(y = q1$IPCA_EX1_ac), colour = "green")+
  geom_line(aes(y = q1$IPCA_DP_ac), colour = "yellow")+
  geom_line(aes(y=q1$IPCA_ac), colour = "black")+
  labs(title ="Grafico II - Núcleos do IPCA",
       y = "Indice acumulado em 12 meses (%)",
       x =" ",
       subtitle = "De 01/2000 até 07/2019 ",
       caption = "Fonte: BCB") +
  
  theme_economist() +scale_colour_economist()

  
  
  

#-- Gráfico 3-----------------------

#Modelando os dados da SELIC

df3 <- merge(SELIC_META,SELIC, by = "data")

df3 <- df3 %>%
  mutate( SELIC_a.a = (((1+(df3$SELIC/100))^252)-1)*100)%>%
  mutate(diferenca = ((SELIC_META/SELIC_a.a)-1)*100)%>%
  subset( data >="2000-01-01" & data <= "2019-07-01")

ggplot(df3, aes(x = df3$data)) + 
  geom_line(aes(y = df3$SELIC_META, colour = "Selic Meta")) + 
  geom_line(aes(y = df3$SELIC_a.a ,colour = "Selic Over")) +
  theme(legend.position=c(0.9, 0.9))+
  labs(title ="Gr?fico 3 - Taxa Selic",
       y = " a.a(%)",
       x ="Anos",
       subtitle = "de 2000 at? 2019",
       caption = "Fonte: BCB") +
  
  geom_vline(xintercept = as.Date("2003-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2007-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2011-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2016-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2016-08-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2019-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  
  
  
  annotate("text", x = as.Date("2003-01-01"), y = 16, label = "Lula 1")+
  annotate("text", x = as.Date("2007-01-01"), y = 18, label = "Lula 2")+
  annotate("text", x = as.Date("2011-01-01"), y = 18, label = "Dilma 1")+
  annotate("text", x = as.Date("2016-01-01"), y = 18, label = "Dilma 2")+
  annotate("text", x = as.Date("2016-08-01"), y = 16, label = "Temer")+
  annotate("text", x = as.Date("2019-01-01"), y = 18, label = "Bolsonaro")+
  
  
  theme_economist() +scale_colour_economist()


#Questao 4 ------------------------------------

#OBSERVAÇAO: Percebi que se acumular a série os dados ficam mais nítidos. Ao inves de variaçao mensal estao acumulados em 12 meses
q4 <- ac_12meses%>%
  subset(data >="2000-01-01" & data <= "2019-07-01")
  
ggplot(q4, aes(x =   data  ))+
  geom_line(aes(y = IPCA_LIVRE_ac, colour = "Itens Livres"))+
  geom_line(aes(y = IPCA_MON_ac, colour= "Preços Monitorados"))+
  labs(title ="Gr?fico 4 - Preços",
       y = "Acumulado em 12 meses(%)",
       x =" ",
       subtitle = "de 2000 at? 2019",
       caption = "Fonte: BCB") +
  geom_vline(xintercept = as.Date("2003-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2007-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2011-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2016-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2016-08-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2019-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  
  
  
  annotate("text", x = as.Date("2003-01-01"), y = 45, label = "Lula 1")+
  annotate("text", x = as.Date("2007-01-01"), y = 45, label = "Lula 2")+
  annotate("text", x = as.Date("2011-01-01"), y = 45, label = "Dilma 1")+
  annotate("text", x = as.Date("2016-01-01"), y = 45, label = "Dilma 2")+
  annotate("text", x = as.Date("2016-08-01"), y = 40, label = "Temer")+
  annotate("text", x = as.Date("2019-01-01"), y = 45, label = "Bolsonaro")+
  
  
  theme_economist() +scale_colour_economist()

#QUESTAO 4.1 -----------------------------------------------
#Dado Original

#Criando o Dataframe
h <- IPCA_MON
j <- IPCA_LIVRE

df5 <- merge(h,j, by ="data")

df5 <- df5 %>%
  subset( data >="2000-01-01" & data <= "2019-07-01")


#Plotando o grafico

ggplot(df5, aes(x = df5$data))+
  geom_line(aes(y = df5$IPCA_MON, colour = "Pre?os Monitorados" ))+
  geom_line(aes(y = df5$IPCA_LIVRE, colour = "Itens Livres"))+
  
  labs(title =" Gr?fico IV - Indice  ",
       y = "Varia??o Mensal (%)",
       x ="Anos",
       subtitle = "de 2000 at? 2019",
       caption = "Fonte: BCB") +
  geom_vline(xintercept = as.Date("2003-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2007-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2011-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2016-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2016-08-01"), color = "black", linetype="dotted", size = 0.7)  +
  geom_vline(xintercept = as.Date("2019-01-01"), color = "black", linetype="dotted", size = 0.7)  +
  
  
  
  annotate("text", x = as.Date("2003-01-01"), y = 5, label = "Lula 1")+
  annotate("text", x = as.Date("2007-01-01"), y = 5, label = "Lula 2")+
  annotate("text", x = as.Date("2011-01-01"), y = 5, label = "Dilma 1")+
  annotate("text", x = as.Date("2016-01-01"), y = 5, label = "Dilma 2")+
  annotate("text", x = as.Date("2016-08-01"), y = 3, label = "Temer")+
  annotate("text", x = as.Date("2019-01-01"), y = 5, label = "Bolsonaro")+
  
  
  theme_economist() +scale_colour_economist()

#Gráficos do 5 até 10 ------------------

#Passos para executar os graficos

#Pressione CTRL + f e digite "IPCA" em Find
#Coloque o nome da Variável de interesse (PIB, PIB_desazonalizado, renda...) em replace
#Selecione todo comando abaixo e use " in selection", depois utilize All


colnames(IPCA) <- c("data", "IPCA")
IPCA$IPCA <- as.numeric(IPCA$IPCA)
IPCA$data <- as.Date(IPCA$data,"%d/%m/%Y")


a <- IPCA %>% subset( data >="2000-01-01" & data <= "2019-11-20")

ggplot(a, aes(x = a$data   ))+
  geom_line(aes( y = a$IPCA), color = "black", size = 0.7, show.legend = TRUE)+
  labs(title ="Gráfico II - IPCA em 2019",
       y = "IPCA acumulado em 12 meses (%)",
       x ="meses",
       subtitle = "De Janeiro até Julho ",
       caption = "Fonte: BCB") +
  
  theme_economist() +scale_colour_economist()

#Situacao Fiscal e Divida Bruta do Governo ---------------------------
#Feito por Wilson Freitas

library(BETS)
library(ggplot2)
library(forecast)
library(gridExtra)

nfsp <- window(BETS.get(5793),start=c(2002,11))
dbgg <- window(BETS.get(13762), start=c(2006,12))



g1 <- autoplot(nfsp)+
  geom_line(size=.8)+
  scale_x_discrete(limits=c(2003:2017))+
  labs(title='',
       subtitle='Necessidade (Primária) de Financiamento do Setor Público (% PIB)')+
  ylab('% PIB')+xlab('')

g2 <- autoplot(dbgg)+
  geom_line(size=.8)+
  scale_x_discrete(limits=c(2007:2017))+
  labs(title='',
       subtitle='Dívida Bruta do Governo Geral (% PIB)',
       caption='Fonte: analisemacro.com.br com dados do BCB.')+
  ylab('% PIB')+xlab('')


grid.arrange(g1, g2,
             top = "Por que aumentar imposto?",
             layout_matrix = matrix(c(1,2), 
                                    ncol=1, byrow=TRUE))


#Emprego e Desemprego ---------

library(magrittr)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(forecast)
library(vars)
library(aod)
### Pacote Seasonal
library(seasonal)
Sys.setenv(X13_PATH = "C:/Séries Temporais/R/Pacotes/seas/x13ashtml")

data = read_csv2('clt.csv',
                 col_types = list(col_date(format='%d/%m/%Y'),
                                  col_double(), col_double(),
                                  col_double())) %>%
  mutate(razao = po_carteira/po*100)

desemprego_sa = ts(data$desemprego, start=c(2002,03), freq=12)
desemprego_sa = final(seas(desemprego_sa))

data = mutate(data, desemprego_sa=desemprego_sa)

#Cambio e Incerteza --------------------
library(BETS)
library(ggplot2)


incerteza = BETS.get('ST_100.0')
cambio = BETS.get(3696, from='2000-01-01')

data = ts.intersect(incerteza, cambio)

df = data.frame(incerteza=data[,1], cambio=data[,2])

ggplot(df, aes(incerteza, cambio))+
  geom_point(stat='identity')+
  geom_smooth(method='lm')+
  xlab('Incerteza Econômica')+ylab('Taxa de Câmbio R$/US$')+
  labs(title='Incerteza Econômica vs. Taxa de Câmbio')+
  
#Operacoes Compromissadas e Reservas -------------------------

#Serie 1839
OP <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.1839/dados?formato=json")
colnames(OP) <- c("data","op")
OP$data <- as.Date(OP$data, format =  "%d/%m/%Y" )
OP$op <- as.numeric(OP$op)

OP <- OP%>%
  filter(data > "2000-01-01")

OP%>%
  ggplot(aes(y = op/1000000, x = as.Date(data)))+
  geom_line()+
  labs(title="Operações Compromissadas/Selic (saldo em final de período)", 
       subtitle="Série 1839", 
       caption="Fonte: BCB", 
       y="U.M.C (Bilhão)",
       x = "")+
  geom_vline(xintercept = as.Date("2006-12-01"), color = "black", linetype="dotted", size = 0.7)  +
  scale_x_date(labels = c(seq(2000,2019)), breaks = c(  seq(as.Date("2000-01-01"), as.Date("2019-09-01"), by = "year")  ))+
  theme_economist(base_size = 8) + scale_fill_economist()

#Serie 1787

reserva <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.1787/dados?formato=json")
colnames(reserva) <- c("data","reserva")
reserva$data <- as.Date(reserva$data, format =  "%d/%m/%Y" )
reserva$reserva <- as.numeric(reserva$reserva)

reserva <- reserva%>%
  filter(data > "2000-01-01")

reserva%>%
  ggplot(aes(y = reserva/1000000, x = as.Date(data)))+
  geom_line()+
  labs(title="Reservas bancárias (saldo em final de período)", 
       subtitle="Série 1787", 
       caption="Fonte: BCB", 
       y="U.M.C (Bilhão)",
       x = "")+
  geom_vline(xintercept = as.Date("2006-12-01"), color = "black", linetype="dotted", size = 0.7)  +
  scale_x_date(labels = c(seq(2000,2019)), breaks = c(  seq(as.Date("2000-01-01"), as.Date("2019-09-01"), by = "year")  ))+
  theme_economist(base_size = 8) + scale_fill_economist()

theme_economist

#O Unico gráfico que não elaborei foi sobre a Balança COmercial, porque esses dados são pagos.

#link: https://analisemacro.com.br/economia/setor-externo/grafico-do-dia-balanca-comercial-desagregada/
#Database : http://www.funcexdata.com.br/


  


  
  