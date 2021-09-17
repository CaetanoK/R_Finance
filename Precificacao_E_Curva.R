#Reprodução de Artigo

#Link: http://wilsonfreitas.github.io/posts/pricing-brazilian-government-bonds-lft.html
#Autor: Wilson Freitas
#
setwd("C:/Users/Caetano/Desktop/Economia/Financas/Clube de Financas/Nucleo Macro/Titulos Publicos/dados/API")

library(bizdays)
library(ipeadatar)
library(calendar)
library(dplyr)
library(tidyr)
library(jsonlite)
library(readxl)
library(ggplot2)
library(purrr)
library(tidyverse)
library(readxl)
library(PerformanceAnalytics)
library(rBMF)
library(GetTDData)
library(ggthemes)
library(fuzzyjoin)
library(sidrar)
library(tidyverse)

#Taxa DI ----------

#Importando o dado do CDI
#Taxa DI = Taxa Selic Over*

DI <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.12/dados?formato=json") #Taxa de juros - Selic. (%a.d)
colnames(DI) <- c("ref.date","DI")
DI$DI <- as.numeric(DI$DI)
DI$ref.date <- as.Date(DI$ref.date,"%d/%m/%Y")
DI <- DI %>% mutate(anual.DI = (((1+(DI/100))^252)-1)*100) #Gerando o valor anualizado da taxa

#---------------- Curva de Juros da BM&F ---------------------------
#Link: http://wilsonfreitas.github.io/posts/empenada-na-curva-de-juros-de-di1.html
#A curva de juros DI1, de depósitos interbancários de 1 dia, a curva mais negociada em nosso mercado, sofreu uma metamorfose nos últimos meses. Saímos de uma expectativa de juros de longo prazo de 12% para quase 16%. Quando falo juros de longo prazo me refiro aos contratos com vencimento a partir de 2020, com aproximadamente 5 anos de duration.


#Curva de Juros da BMF

library(XML)
library(bizdays)
library(stringr)

bizdays.options$set(default.calendar="Brazil/ANBIMA")

str_supplant <- function (string, repl) {
  result <- str_match_all(string, "\\{([^{}]*)\\}")
  if (length(result[[1]]) == 0)
    return(string)
  result <- result[[1]]
  for (i in seq_len(dim(result)[1])) {
    x <- result[i,]
    pattern <- x[1]
    key <- x[2]
    if (!is.null(repl[[key]]))
      string <- gsub(pattern, repl[[key]], string, perl=TRUE)
  }
  string
}

.get_curve_url <- function(refdate, ticker) {
  url <- 'http://www2.bmf.com.br/pages/portal/bmfbovespa/boletim1/TxRef1.asp'
  query <- str_supplant('?Data={refdate}&Data1={sysdate}&slcTaxa={ticker}',
                        list(refdate=format(as.Date(refdate), '%d/%m/%Y'),
                             sysdate=format(Sys.Date(), '%Y%m%d'),
                             ticker=ticker))
  paste0(url, query)
}

get_curve <- function (refdate, ticker='PRE') {
  refdate <- as.Date(refdate)
  url <- .get_curve_url(refdate, ticker)
  doc <- htmlTreeParse(url, useInternalNodes=TRUE)
  num <- xpathSApply(doc, "//td[contains(@class, 'tabelaConteudo')]", 
                     function(x) gsub('[\r\n \t]+', '', xmlValue(x)))
  num <- sapply(num, function(x) as.numeric(gsub(',', '.', x)), USE.NAMES=FALSE)
  
  colspan <- as.integer(xpathApply(doc, "//td[contains(@class, 'tabelaTitulo')]",  xmlAttrs )[[2]][3])
  if (colspan == 1) {
    terms <- num[c(TRUE, FALSE)]
    rates <- num[c(FALSE, TRUE)]/100
    log_pu <- log(1 + rates*terms/360)
    rate <- function(pu, term) (pu - 1)*(360/term)
  } else {
    terms <- bizdayse(refdate, num[c(TRUE, FALSE, FALSE)])
    rates <- num[c(FALSE, TRUE, FALSE)]/100
    log_pu <- log((1 + rates)^(terms/252))
    rate <- function(pu, term) pu^(252/term) - 1
  }
  
  log_price_interpolator <- approxfun(terms, log_pu, method='linear')
  function (term) {
    pu <- exp(log_price_interpolator(term))
    rate(pu, term)*100
  }
}
#Uma vez que este código foi sourced basta executar a função get_curve passando a data e a curva de interesse, no nosso caso a curva de interesse é a de DI1 que tem código PRE (de juros pré-fixados).

irbrl <- get_curve('2015-01-02', 'PRE')
irbrl(seq(21, 252, by=21))
##  [1] 11.80000 12.04666 12.28111 12.46142 12.58328 12.66676 12.72591
##  [8] 12.77506 12.81516 12.85459 12.88258 12.91228


#-------- Grafico --------------


seq(21, 2520, by=21) -> terms

dates <- seq(as.Date('2015-01-01'), as.Date('2015-09-01'), by='2 months')
dates <- adjust.next(dates)
curves <- lapply(dates, function(d) {
  get_curve(d, 'PRE')(terms)
})
names(curves) <- dates

ir_df <- stack(curves)
ir_df$date <- terms + Sys.Date()

library(ggplot2)
ggplot(data=ir_df, aes(x=date, y=values, colour=ind)) + geom_point() + geom_line() +
  xlab('Datas') + ylab('Taxas') + theme(legend.title=element_blank(), legend.position='top') + ggtitle('Curvas de DI1')

#Observe que de julho para setembro as taxas de longo prazo se elevaram em 2 pontos percentuais (200bps), o que para um prazo de 10 anos é uma variação significativa.

#Empenada na curva de Juros -----------------------
#Link: http://wilsonfreitas.github.io/posts/nada-e-tao-ruim-que-nao-possa-piorar-empenada-na-curva-de-juros-de-di1-parte-2.html

#O mês de setembro de 2015 não está sendo fácil para o mercado financeiro. Alta de dólar (rompeu a barreira de R$ 4,00 e segue sem rumo), disparada de juros, pacote bomba do governo, pacote fiscal nebuloso, volta da CPMF e por aí vai. Mas no dia 23 de setembro de 2015 aconteceu algo interessante, após o nome do deputado Eduardo Cunha aparecer na lista de acusados da Operação Lava Jato, a curva de juros apresentou forte elevação. Essa empenada é reflexo da incerteza que ronda a aprovação de medidas importantes para a economia e para o país, incluindo o corte de gastos e o ajuste fiscal. Olhe no gráfico abaixo a curva de juros de DI1 para a semana do dia 21 de setembro.

seq(21, 1260, by=21) -> terms

dates <- seq(as.Date('2015-09-21'), as.Date('2015-09-25'), by='days')
curves <- lapply(dates, function(d) {
  get_curve(d, 'PRE')(terms)
})
names(curves) <- dates

ir_df <- stack(curves)
ir_df$date <- terms + Sys.Date()

library(ggplot2)
ggplot(data=ir_df, aes(x=date, y=values, colour=ind)) + geom_point() + geom_line() +
  xlab('Datas') + ylab('Taxas') + theme(legend.title=element_blank(), legend.position='top') + ggtitle('Curvas de DI1')

ir_df%>%
  filter(ir_df$date == 	as.Date("2023-07-07"))

15.83-16.81
#Isso mesmo, em 3 dias a curva deu uma empenada de 1 ponto percentual (100 basis points (bps)) e depois retornou ao nível inicial no último dia da semana. Esses movimentos do mercado, na minha opinião são muito difíceis de se explicar. Em geral os analistas se prendem a algum fato, como as notícias do governo, ou criam um factóide, mas a verdade é que pode ser um grande player manipulando o mercado de forma indireta.




#Veja para outras datas
#DOl - Dollar
#PRE - Pre
#EU - EUR
#DIC - IPCA
#DIM - IGP-M

seq(21, 2520, by=21) -> terms

dates <- seq(as.Date("2020-03-02"), as.Date("2020-03-06"), by='2 day')

dates <- adjust.next(dates)


curves <- lapply(dates, function(d) {
  get_curve(d, 'PRE')(terms)
})


names(curves) <- dates

ir_df <- stack(curves)
ir_df$date <- terms + Sys.Date()

ggplot(data=ir_df, aes(x=date, y=values, colour=ind)) + geom_point() + geom_line() +
  xlab('Maturidade') + ylab('Taxas') + theme(legend.title=element_blank(), legend.position='top') + ggtitle('Curvas de DI1 - ANBIMA')



asset.codes <- c("LTN")
download.TD.data(asset.codes = asset.codes)
LFT <- read.TD.files(asset.codes = c("LTN"))

LFT %>%
  subset( LFT$ref.date >= "2019-09-21" & LFT$ref.date <= "2019-10-21")%>%
    ggplot(aes(x = as.Date(ref.date), y = yield.bid, color = asset.code)) +
  geom_line() + labs(x = "Datas", y = "Taxas(%)", colour = NULL)+
  labs(title="LFT Precos", 
       subtitle="De janeiro de 2014 atÃ© outubro de 2019", 
       caption="Fonte: Tesouro Nacional", 
       y="Taxa",
       x = "")+
  theme_economist() + scale_fill_economist()




#Tesouro Prefixado ---------------------------


#Segundo o Tesouro Nacional do Brasil, LTN é um título com rentabilidade definida no momento da compra, com o resgate do valor do título na data do vencimento. Cada título é adquirido com deságio e possui o valor de resgate de R$ 1.000,00, no vencimento.

#Dado uma LTN em que:
#Data do negócio: 30/12/2015
#Data de maturação: 2017/01/01

#Exemplo - Encontre PU:

cal <- create.calendar("Brazil/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"))

VN = 1000
n = 252 #Ano base em dias uteis 252. Pode ser substituida por 360 ou 365 dependendo do exercicio.
dias.uteis = bizdays(from = as.Date("2015-12-30"), to = ("2017-01-01"),cal ) #Dias uteis entre as duas datas
i <- 0.157698

PU <- function(VN, n, dias.uteis , i){
  K <- VN/((1+i)^(dias.uteis/n))
  K
}

PU(VN,n,dias.uteis,i)

#Exemplo - encontre a taxa:

VN = 1000
n = 252
dias.uteis = bizdays(from = as.Date("2015-12-30"), to = ("2017-01-01"),cal ) #Dias uteis entre as duas datas
pu <- 863.7831

i <- function(fv,vp,n){
  for(N in 1:length(n))
    K <- sum((((fv[1:n]/vp[1:n])^(252/n[1:n]))-1),na.rm = TRUE)
  K
}

i(fv = VN, vp = PU, n = dias.uteis)

#As taxas que definirão o preço das LTNs vêm do derivativo chamado Futuro de Taxa Média de Depósitos Interfinanceiros de Um Dia da BM&FBovespa (B3), o famoso Futuro de DI –


#Exemplo: Calcule agio e desagio ------------------

#no dia 30/12/2014 efetuamos a compra de uma LTN com vencimento em
#04/01/2016 com a Taxa de 13,0202 a.a e consequentemente ao Preço de 884,797584, há
#exatos 252 dias úteis do vencimento do título.

VN = 1000
n = 252
dias.uteis = bizdays(from = as.Date("2014-12-30"), to = ("2016-01-04"),cal ) #Dias uteis entre as duas datas
pu_compra <- 884.797584
i_compra <- 0.130202

#No ponto 1 da figura 3 já passaram 130 dias (09/07/2015) desde da compra e nossa LTN que vem sendo corrigida pela Taxa de compra (13,0202 a.a) e está valendo R$ 942,46. No entanto, a taxa de mercado (linha tracejada) encontrava-se 13,32 a.a e logo o Preço de mercado foi R$ 941,25 para esse dia.
ponto1 <-bizdays(from = as.Date("2014-12-30"), to = ("2016-01-04"),cal ) - bizdays(from = as.Date("2014-12-30"), to = ("2015-07-09"),cal ) #Veja que temos de realizar a diferenca entra [Inicio da negociacao do titulo até maturacao] - [Negociacao do titulo ate a maturacao]


#Preco diario para taxas de mercado (Curvatura sinuosa em torno da Linha Reta)
i_ponto1 <- 0.1332
preco_mercado <- PU(VN = 1000, n = 252, dias.uteis = ponto1, i = i_ponto1)
preco_mercado

#Preco diaro para taxa de compra (Linha reta)
preco_compra <- PU(VN = 1000, n = 252, dias.uteis = ponto1, i = i_compra) #Mude apenas a taxa, pois passaram-se os dias e temos de fazer a comparacao com a mesma taxa da realizacao da compra
preco_compra



#Observem que a taxa desde a nossa compra mudou, subindo de 13,02 para 13,32, e assim, o Preço que o mercado está disposto a pagar na nossa LTN, neste dia, será diferente daquele referente a nossa Taxa de compra, ou seja, o Preço que estamos carregando em nossa carteira de ativos.
#Se quisermos nos desfazer da LTN deveremos estar dispostos a receber um valor menor do que aquele que ele vale para nós. Ou seja, 941,25 – 942,46 = R$ -1,21.

#--- Desagio/agio ---
#Como o Preço diario para taxa de compra é maior do que o preco diaro para taxas de mercado, significa que a taxa de compra é menor que a taxa de mercado. Concluimos:

#Taxa de mercado > taxa de compra ::::: Desagio
#Taxa de mercado < Taxe de compra ::::: Agio

# Preço diario para taxa de mercado > Preco diaro para taxas de compra ::::: Agio
# Preço diario para taxa de mercado < Preco diaro para taxas de compra ::::: Desagio

#Valor monetario
preco_mercado - preco_compra
#Houve uma perda de - R$ 1.20 em relacao ao preco de compra pela mesma taxa

#Rentabilidade (Anualizada)
rent1 <- (((preco_mercado/pu_compra)^(252/ponto1))-1)*100
rent2 <- (((preco_compra/pu_compra)^(252/ponto1))-1)*100
rent2-rent1
#Sua rentabilidade caiu em 0.30% a.a

#A principio nao perdeu dinheiro, pois vendeu a um valor mais alto. Entretanto, se mantivesse rendendo naquela taxa ou numa menor estaria recebendo mais.
preco_mercado-pu_compra

#Negociando LTN --------------------

#Demonstracao ----------------

#O mercado chama a quantidade de dias da data atual até o vencimento de títulos
#prefixados do Tesouro Nacional de “Duration” que no caso da LTN corresponde exatamente
#ao número de dias úteis entre esses dias

#Vamos buscar as LTN de maturacao em 01/01/2019, 01/01/2021 e 01/01/2023.

asset.codes <- c("LTN")
download.TD.data(asset.codes = asset.codes)
LTN <- read.TD.files(asset.codes = c("LTN"),
                     maturity = c("010119", "010121","010123"))


LTN <- LTN %>% subset(ref.date > "2016-01-16" & ref.date < "2017-03-16")


LTN %>% 
  ggplot(aes(x = as.Date(ref.date), y = yield.bid*100, color = asset.code)) +
  geom_line() + labs(x = "Datas", y = "Taxas(%)", colour = NULL)+
  labs(title="LTN de CP,MP,LP", 
       subtitle="Entre 01/2016 à 03/2017", 
       caption="Fonte: Tesouro Nacional", 
       y="Taxa",
       x = "")+
  theme_economist()

bizdays(from = LTN[,1], to = LTN[,3], cal)


#Suponha que eu comprei um titulo no dia 26-01-2016 comprei um titulo que vence em 01-01-2019.
#Vamos verificar a rentabilidade, dado pu de compra 644.85 e taxa de compra 16.28%
pu_compra <- 644.85
i_compra <- 0.1628



teste <- filter(LTN, matur.date == "2019-01-01")
teste <- teste %>% mutate(taxa.compra = PU(VN = 1000, n = 252, dias.uteis = bizdays(from = teste[,1], to = teste[,5], cal)  , i = i_compra  ))
teste <-teste %>% mutate(spread = teste$price.bid - teste$taxa.compra  ) #Preço diario para taxa de mercado > Preco diaro para taxas de compra ::::: Agio
teste <- teste %>% mutate(tx.operacao = ((price.bid/taxa.compra)-1)*100 ) #Porcentagem

teste%>% 
  na.omit()%>% 
  select(ref.date, taxa.compra, price.bid)%>% 
  pivot_longer(-ref.date)%>%
  ggplot(aes(x = ref.date, y = value, color = name)) +
  geom_line() 


teste%>% 
  na.omit()%>% 
  select(ref.date, yield.bid)%>% 
  pivot_longer(-ref.date)%>%
  ggplot(aes(x = ref.date, y = value, color = name)) +
  geom_line() 

#Modelo de Negociacao (VER NA APRESENTACAO) -----------------

asset.codes <- c("LTN")
download.TD.data(asset.codes = asset.codes)
LTN_papeis <- read.TD.files(asset.codes = c("LTN"))


#Inputs
data.compra = "2019-01-01"
data.venda = "2020-09-25"
vencimento = "2025-01-01" #Escolha apenas o titulo que negociaras, 

#Resultados
LTN <- LTN_papeis %>% subset(ref.date >= data.compra & ref.date <= data.venda)
head(LTN)
tail(LTN)

LTN %>% 
  ggplot(aes(x = as.Date(ref.date), y = yield.bid*100, color = asset.code)) +
  geom_line() + labs(x = "Datas", y = "Taxas(%)", colour = NULL)+
  labs(title="LTN", 
       subtitle = "T?tulo P?blico Pr?-fixado",
       caption="Fonte: Tesouro Nacional", 
       y="Taxa (%) ",
       x = "")+
  theme_economist() +   theme(plot.background = element_rect(fill = "white"),
                              panel.background = element_rect(fill = NA),
                              panel.grid.major = element_line(colour = "grey", size = 0.5, linetype = "dotted"),
                              panel.grid.major.x = element_blank(),
                              axis.line = element_line(size = 1, colour = "grey"),
                              title = element_text(colour = "deepskyblue4"),
                              text = element_text(size = 12, color = "deepskyblue4"),
                              axis.text.x  = element_text(angle = 90),
                              legend.title = element_text(size = )
  )



#Moldando os dados em um Dataframe

negocio_LTN <- filter(LTN, matur.date == vencimento)

pu_compra <- negocio_LTN[1,3] #Preco do primeiro dia
i_compra <- negocio_LTN[1,2] #Taxa do primeiro dia
entredias <-   bizdays(negocio_LTN[,1], to = vencimento , cal )

negocio_LTN <- negocio_LTN %>% mutate(taxa.compra = PU(VN = 1000, n = 252, dias.uteis = entredias  , i = i_compra  ))
negocio_LTN <-negocio_LTN %>% mutate(spread = negocio_LTN$price.bid - negocio_LTN$taxa.compra  ) #Preço diario para taxa de mercado > Preco diaro para taxas de compra ::::: Agio
negocio_LTN <- negocio_LTN %>% mutate(tx.operacao = ((price.bid/taxa.compra)-1)*100 ) #Porcentagem
negocio_LTN <- negocio_LTN %>% mutate(taxa.anual = (1+(tx.operacao/100))^(entredias/252) ) #Porcentagem

head(negocio_LTN)
tail(negocio_LTN)

negocio_LTN%>% 
  na.omit()%>% 
  select(ref.date, taxa.compra, price.bid)%>% 
  pivot_longer(-ref.date)%>%
  ggplot(aes(x = ref.date, y = value, color = name)) +
  geom_line() + 
  labs(title="Volatilidade da Taxa", 
       subtitle = "Marca??o a mercado e na curva",
       caption="Fonte: Tesouro Nacional", 
       y="R$ ",
       x = "")+
  theme(plot.background = element_rect(fill = "white"),
                        panel.background = element_rect(fill = NA),
                        panel.grid.major = element_line(colour = "grey", size = 0.5, linetype = "dotted"),
                        panel.grid.major.x = element_blank(),
                        axis.line = element_line(size = 1, colour = "grey"),
                        title = element_text(colour = "deepskyblue4"),
                        text = element_text(size = 12, color = "deepskyblue4"),
                        axis.text.x  = element_text(angle = 90),
                        
                        legend.position = "none"
                        
  )



#--- Suponha que eu compre em um dia e venda no outro, desconsiderando os impostos. Qual a rentabilidade?

rentabilidade <- negocio_LTN %>% mutate(rent =  ((price.bid/lag(price.bid,1))-1)*100 )
summary(rentabilidade$rent)
sd(rentabilidade$rent, na.rm = TRUE)
max(rentabilidade$rent, na.rm = TRUE)-min(rentabilidade$rent, na.rm = TRUE)

rentabilidade%>% 
  na.omit()%>% 
  select(ref.date, rent)%>% 
  pivot_longer(-ref.date)%>%
  ggplot(aes(x = ref.date, y = value, color = name)) +
  geom_line() 

#Rentabilidade acumulada ----------
#Positiva
rent.p <- rentabilidade$rent[rentabilidade$rent>=0]
ac_positivo <- (cumprod( (na.omit((1+(rent.p/100))^(1/length(rent.p)))) )-1)*100 #Inserir o 272, pois depende do numero da amostra. Ocorre devido aos numeros serem muito semelhantes. Consertar isso!
ac_positivo #0.75% no final do periodo

#Negativa
rent.n <- rentabilidade$rent[rentabilidade$rent<=0]
ac_negativo <- (cumprod( (na.omit((1+(rent.n/100))^(1/length(rent.n)))) )-1)*100 #Inserir o 272, pois depende do numero da amostra. Ocorre devido aos numeros serem muito semelhantes. Consertar isso!
ac_negativo #-0.59% no final do periodo

#Total
ac <- (cumprod( (na.omit((1+(rentabilidade$rent/100))^(1/length(rentabilidade$rent)))) )-1)*100 #Inserir o 272, pois depende do numero da amostra. Ocorre devido aos numeros serem muito semelhantes. Consertar isso!
ac #0.17% no final do periodo

#OBS: No artigo, ele descobre uma rentabilidade bruta no periodo de 65.49% para uma LTN negociada no dia 26/01/2019 e vendida no dia 30/03/2017 com maturacao em 01/01/2023. Necessita verificar os valores de rentabilidade acumulada, os outros estão dando certo

#Comparacao com DI ----------
#Além da rentabilidade absoluta levaremos em consideração a rentabilidade relativa que compara a rentabilidade absoluta com a rentabilidade do Custo Oportunidade que no nosso caso usaremos um ativo remunerado a 90% CDI (aquele DI que explicamos anteriormente).

#Verifique os valores de negociacao do papel

#O valor de tx.opDI esta expressa em %
rentabilidade <- merge(DI, rentabilidade, by = "ref.date", all.y = TRUE)
rentabilidade <- rentabilidade %>% mutate(DI_periodo = ((((( 1+(rentabilidade$anual.DI/100))^( seq(1,nrow(rentabilidade),by = 1) /252))  ))-1)*100 )
rentabilidade <- rentabilidade %>% mutate(tx.opDI = ((((1+(rentabilidade$tx.operacao/100))  /(( 1+(rentabilidade$anual.DI/100))^( seq(1,nrow(rentabilidade),by = 1) /252))  ))-1)*100 )

rentabilidade%>% 
  na.omit()%>% 
  select(ref.date, tx.operacao,    DI_periodo    )%>% 
  pivot_longer(-ref.date)%>%
  ggplot(aes(x = ref.date, y = value, color = name)) +
  geom_line() +
  labs(title="Rentabilidade", 
       subtitle = "Vermelho - Selic / Azul - LTN",
       caption="Fonte: Tesouro Nacional", 
       y="Taxa %a.a ",
       x = "")+
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey", size = 0.5, linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(size = 1, colour = "grey"),
        title = element_text(colour = "deepskyblue4"),
        text = element_text(size = 12, color = "deepskyblue4"),
        axis.text.x  = element_text(angle = 90),
        
        legend.position = "none"
        
  )



#Duration e medidas de risco ----------------
#Veja a relacao Preco e Taxa, observe a inclinacao quando a taxa varia de -1% até +1% em 20 pb
pb = 0.002
precotaxa = data.frame(taxas = rentabilidade[1,4]+  seq(-0.01,0.01, by = pb) )
precotaxa = precotaxa %>% mutate(pu = PU(VN = 1000, n = 252, dias.uteis = entredias[1], i = precotaxa$taxas ) ) #Preco original fica na linha 11


duration =  (precotaxa[round(nrow(precotaxa)/2,0)-1,2]-precotaxa[round(nrow(precotaxa)/2,0)+1,2])/(2*precotaxa[round(nrow(precotaxa)/2,0),2]*pb)
duration

#Duration is interpreted as the approximate percentage change in price for a
#100 basis point change in rates. Consequently, a duration of 3.12  means that the
#approximate change in price for this bond is 3.12% for a 100 basis point change
#in rates. Veja o exemplo

((756/733)-1)*100 #Proximo a duration, onde 759 é o preco em +100pb na taxa, e 733 é o preco original.

#Approximating the Percentage Price Change Using Duration
-duration*pb*100
((733/738)-1)*100
#Vemos que nossa estimativa para o duration foi boa, porque 738 é o novo preco numa queda de 20pb nas taxas.
#E se a taxa modificar em +200pb, qual sera a nova variacao de preco?
-duration*0.02*100
733.6191*(1-0.0624114)
PU(VN = 1000, n = 252, dias.uteis = entredias[1], i = 0.1149 )





#NTN-B ----------------------

#Seguiremos o exemplo do Tesouro nacional http://www.tesouro.fazenda.gov.br/documents/10180/410323/NTN-B%20principal_novidades.pdf
#Dados para o papel

#Data de compra: 02/01/2012 (liquidacao no dia 03/01/2012)
#VNA ate dezembro/2011: 2.097.58
#Data de Vencimento: 15/05/2015
#Dias uteis: 846
#taxa pactuada: 5.17%a.a
#Preco no dia da compra: R$ 1776,77
#IPCA projetado para dezembro 2011: 0.53%


IPCA <- fromJSON("https://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados?formato=json") #433 - ?ndice nacional de pre?os ao consumidor-amplo (IPCA) - Var. % mensal
colnames(IPCA) <- c("data", "IPCA")
IPCA$IPCA <- as.numeric(IPCA$IPCA)
IPCA$data <- as.Date(IPCA$data,"%d/%m/%Y")



inflacao_VNA <- get_sidra(api = '/t/1737/n1/all/v/2266/p/all/d/v2266%2013')
inflacao_VNA <- inflacao_VNA[,11]
inflacao_VNA <- cbind(inflacao_VNA, data =  seq(as.Date("1979-12-01"),length.out = nrow(inflacao_VNA), by = "month"))


fator <- function(x){
a <- inflacao_VNA %>% filter(data == "2000-06-01") #IPCA de Junho
b <- inflacao_VNA %>% filter(data == x) #IPCA do mes anterior ao requisitado
c = b$Valor/a$Valor
c
}

VNA = 1000*fator("2011-11-01")
IPCA%>% filter(data =="2011-11-01" ) #Aqui usaremos 0.53% que era o projetado pelo mercado


#O expoente da equação “x” nada mais é do que a razão entre o número de dias corridos entre a data de
#liquidação e o dia 15 do mês em questão e o número de dias corridos entre o dia 15 do mês seguinte e o dia 15
#do mês em questão. Ou seja:
  
x = as.numeric((as.Date("2012-01-03")-as.Date("2011-12-15")))/as.numeric((as.Date("2012-01-15")-as.Date("2011-12-15")))
VNA = VNA*((1+0.0053)^x)

#Agora deve-se obter a cotação do papel, que reflete o ágio ou o deságio do título, dada pela seguinte relação:

DU <- bizdays(from = "2012-01-03",to = "2015-05-15", cal)
cotacao <- PU(VN = 100, n = 252, dias.uteis = DU, i = 0.0517)

#Retomando a equação original do preço do papel, tem-se:
preco <- VNA*cotacao/100
preco



#Modelo de Precificacao NTN-B Principal

#Suponha
#Data de compra: 02/01/2018 (liquidacao no dia 03/01/2012)
#Data de Vencimento: 15/08/2024
#Taxa = 0.0506 negociado na data de compra

#Veja os titulos
asset.codes <- c("NTN-B Principal")
download.TD.data(asset.codes = asset.codes)
NTN <- read.TD.files(asset.codes = c("NTN-B Principal"))

NTN %>% filter(ref.date >= data.compra) %>% 
  ggplot(aes(x = as.Date(ref.date), y = yield.bid*100, color = asset.code)) +
  geom_line() + labs(x = "Datas", y = "Taxas(%)", colour = NULL)+
  labs(title="NTN-B Principal", 
       caption="Fonte: Tesouro Nacional", 
       y="Taxa",
       x = "")+
  theme_economist() + scale_fill_economist()




#Input
data.compra = "2012-01-02"
data.liquidacao = "2012-01-03" #Lembre-se que é D+1
mes.anterior = "2011-12-01" #Preencha com dia 1 o mes anterior ao da data de compra
mes.VNA = "2011-11-01" #Para calculo do fator VNA, sendo 2 meses anterior a data de compra

dia15.mescompra = "2012-01-15"
dia15.anterior = "2011-12-15"

vencimento = "2015-05-15"

#Montando o dataframe
negocio_NTN <- filter(NTN, matur.date == vencimento)
juros <- filter(negocio_NTN, ref.date == data.compra)
juros <- juros[,2]

#1º Encontrando o VNA
x = as.numeric((as.Date(data.liquidacao)-as.Date(dia15.anterior)))/as.numeric((as.Date(dia15.mescompra)-as.Date(dia15.anterior)))
VNA = 1000*fator(mes.VNA)
taxa = IPCA%>% filter(data == mes.VNA ) #O problema esta nesta taxa. O VNA eh baseado na projeçao
VNA = VNA*((1+(taxa[,2]/100))^x)

#2º Encontrando PU e preco
DU = bizdays(from = data.liquidacao, to = vencimento, cal)
cotacao = PU(VN = 100, n = 252, dias.uteis = DU, i = juros)

preco = cotacao*VNA/100
preco

#O preco foi de 2204.47, ja o bid foi de 	2193.14. Houve realmente um desagio do mercado?
#O preco nunca sera igual o bid, até porque o VNA projetado conta com uma taxa de inflacao em projeção, não a atual.



DI %>% 
  subset( ref.date >="2019-01-01" & ref.date <= "2020-02-28")%>%
  na.omit() %>% 
  select(ref.date, anual.DI) %>% 
  pivot_longer(-ref.date)%>%
  
  ggplot(aes(x = ref.date, y = value, color = name)) +
  geom_line() +
  labs(title = " Taxa SELIC ",
       subtitle = " ",
       caption = "Fonte - BACEN",
       x = " ",
       y = " %a.a ") +
  guides(color = guide_legend(title = "")) +
  scale_x_date(breaks = "1 month", date_labels = "%m/%Y")+
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey", size = 0.5, linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(size = 1, colour = "grey"),
        title = element_text(colour = "deepskyblue4"),
        text = element_text(size = 12, color = "deepskyblue4"),
        axis.text.x  = element_text(angle = 90),
        
        legend.position = "none"
        
  )



