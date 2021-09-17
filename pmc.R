#Proposta 1

#Autor: Wilson Freitas
#Link: http://wilsonfreitas.github.io/posts/empenada-na-curva-de-juros-de-di1.html

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
cal <- create.calendar("Brazil/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"))

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
#A curva de juros DI1, de depÃ³sitos interbancÃ¡rios de 1 dia, a curva mais negociada em nosso mercado, sofreu uma metamorfose nos Ãºltimos meses. SaÃ­mos de uma expectativa de juros de longo prazo de 12% para quase 16%. Quando falo juros de longo prazo me refiro aos contratos com vencimento a partir de 2020, com aproximadamente 5 anos de duration.


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
#Uma vez que este cÃ³digo foi sourced basta executar a funÃ§Ã£o get_curve passando a data e a curva de interesse, no nosso caso a curva de interesse Ã© a de DI1 que tem cÃ³digo PRE (de juros prÃ©-fixados).

irbrl <- get_curve('2015-01-02', 'PRE')
irbrl(seq(21, 252, by=21))
##  [1] 11.80000 12.04666 12.28111 12.46142 12.58328 12.66676 12.72591
##  [8] 12.77506 12.81516 12.85459 12.88258 12.91228


#-------- Grafico --------------


seq(21, 2520, by=21) -> terms

dates <- seq(as.Date('2020-01-01'), as.Date('2020-09-01'), by='4 months')
dates <- adjust.next(dates)
curves <- lapply(dates, function(d) {
  get_curve(d, 'PRE')(terms)
})
names(curves) <- dates

ir_df <- stack(curves)
ir_df$date <- terms + Sys.Date()

library(ggplot2)
ggplot(data=ir_df, aes(x=date, y=values, colour=ind)) + geom_point() + geom_line() +
  xlab('Vencimentos') + ylab('Taxas') + theme(legend.title=element_blank(), legend.position='top') + ggtitle('Curva de Juros')


#--------- Negociacao de Titulos ---------------

#Funcoes
cal <- create.calendar("Brazil/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"))

PU <- function(VN, n, dias.uteis , i){
  K <- VN/((1+i)^(dias.uteis/n))
  K
}

i <- function(fv,vp,n){
  for(N in 1:length(n))
    K <- sum((((fv[1:n]/vp[1:n])^(252/n[1:n]))-1),na.rm = TRUE)
  K
}

#Valores

#O mercado chama a quantidade de dias da data atual atÃ© o vencimento de tÃ­tulos
#prefixados do Tesouro Nacional de âDurationâ que no caso da LTN corresponde exatamente
#ao nÃºmero de dias Ãºteis entre esses dias

#Vamos buscar as LTN de maturacao em 01/01/2019, 01/01/2021 e 01/01/2023.

asset.codes <- c("LTN")
download.TD.data(asset.codes = asset.codes)
LTN_papeis <- read.TD.files(asset.codes = c("LTN"))

#Inputs
data.compra = "2020-01-01"
data.venda = "2020-04-01"
vencimento = "2025-01-01" #Escolha apenas o titulo que negociaras, 

#Resultados
LTN <- LTN_papeis %>% subset(ref.date >= data.compra & ref.date <= data.venda)
head(LTN,1)
tail(LTN)


LTN %>% 
  ggplot(aes(x = as.Date(ref.date), y = yield.bid*100, color = asset.code)) +
  geom_line() + labs(x = "Datas", y = "Taxas(%)", colour = NULL)+
  labs(title="LTN", 
       subtitle = "TÃ­tulo PÃºblico prÃ©-fixado",
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

negocio_LTN <- filter(LTN, matur.date == as.Date(vencimento))
head(negocio_LTN)
tail(negocio_LTN)


pu_compra <- negocio_LTN[1,3] #Preco do primeiro dia
i_compra <- negocio_LTN[1,2] #Taxa do primeiro dia
entredias <-   bizdays(negocio_LTN[,1], to = vencimento , cal )

negocio_LTN <- negocio_LTN %>% mutate(taxa.compra = PU(VN = 1000, n = 252, dias.uteis = entredias  , i = i_compra  ))
negocio_LTN <-negocio_LTN %>% mutate(spread = negocio_LTN$price.bid - negocio_LTN$taxa.compra  ) #PreÃ§o diario para taxa de mercado > Preco diaro para taxas de compra ::::: Agio
negocio_LTN <- negocio_LTN %>% mutate(tx.operacao = ((price.bid/taxa.compra)-1)*100 ) #Porcentagem
negocio_LTN <- negocio_LTN %>% mutate(taxa.periodo = (( (1+(tx.operacao/100))^(entredias/252))-1)*100 ) #Porcentagem

head(negocio_LTN)
tail(negocio_LTN)

negocio_LTN%>% 
  na.omit()%>% 
  select(ref.date, taxa.compra, price.bid)%>% 
  pivot_longer(-ref.date)%>%
  ggplot(aes(x = ref.date, y = value, color = name)) +
  geom_line() + 
  labs(title="EvoluÃ§Ã£o da LTN", 
       subtitle = "MarcaÃ§Ã£o na Curva e a Mercado",
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



#Duration e medidas de risco ----------------
#Veja a relacao Preco e Taxa, observe a inclinacao quando a taxa varia de -1% atÃ© +1% em 20 pb
pb = 0.002
precotaxa = data.frame(taxas = negocio_LTN[1,2]+  seq(-0.01,0.01, by = pb) )
precotaxa = precotaxa %>% mutate(pu = PU(VN = 1000, n = 252, dias.uteis = entredias[1], i = precotaxa$taxas ) ) #Preco original fica na linha 11


duration =  (precotaxa[round(nrow(precotaxa)/2,0)-1,2]-precotaxa[round(nrow(precotaxa)/2,0)+1,2])/(2*precotaxa[round(nrow(precotaxa)/2,0),2]*pb)
duration


convexidade = (precotaxa[7,2]+precotaxa[5,2]-2*precotaxa[6,2])/(2*precotaxa[6,2]*0.01^2)
convexidade
#Duration is interpreted as the approximate percentage change in price for a
#100 basis point change in rates. Consequently, a duration of 3.12  means that the
#approximate change in price for this bond is 3.12% for a 100 basis point change
#in rates. Veja o exemplo


#ACoes ===================


library(quantmod)
library(ggplot2)
library(tidyverse)


getSymbols("^GSPC", from = "2008-01-01", to = Sys.Date())
sp <- data.frame(as.xts(GSPC))
sp$data <- as.Date(row.names(sp))
sp <- sp%>%
  mutate(SP500_2020 = ((sp$GSPC.Close/sp[which(sp$data == as.Date("2020-01-02")),4])-1) )%>%
  mutate(SP500_2008 = ((sp$GSPC.Close/sp[which(sp$data == as.Date("2008-01-02")),4])-1) )





getSymbols("^BVSP", from = "2008-01-01", to = Sys.Date())
ibov <- data.frame(as.xts(BVSP))
ibov$data <- as.Date(row.names(ibov))
ibov <- ibov%>%
  mutate(ibov_2020 = (ibov$BVSP.Close/(ibov[which(ibov$data == as.Date("2020-01-03")),4])-1) )%>%
  mutate(ibov_2008 = (ibov$BVSP.Close/(ibov[which(ibov$data == as.Date("2008-01-03")),4])-1) )



getSymbols("^STOXX50E", from = "2008-01-01", to = Sys.Date())
eu <- data.frame(as.xts(STOXX50E))
eu$data <- as.Date(row.names(eu))
eu <- eu%>%
  mutate(eu_2020 = (eu$STOXX50E.Close/(eu[which(eu$data == as.Date("2020-01-03")),4])-1) )%>%
  mutate(eu_2008 = (eu$STOXX50E.Close/(eu[which(eu$data == as.Date("2008-01-03")),4])-1) )


getSymbols("^FTSE", from = "2008-01-01", to = Sys.Date())
FTSE <- data.frame(as.xts(FTSE))
FTSE$data <- as.Date(row.names(FTSE))
FTSE <- FTSE%>%
  mutate(FTSE_2020 = (FTSE$FTSE.Close/(FTSE[which(FTSE$data == as.Date("2020-01-02")),4])-1) )%>%
  mutate(FTSE_2008 = (FTSE$FTSE.Close/(FTSE[which(FTSE$data == as.Date("2008-01-02")),4])-1) )


getSymbols("^N225", from = "2008-01-01", to = Sys.Date())
N225 <- data.frame(as.xts(N225))
N225$data <- as.Date(row.names(N225))
N225 <- N225%>%
  mutate(N225_2020 = (N225$N225.Close/(N225[which(N225$data == as.Date("2020-01-07")),4])-1) )%>%
  mutate(N225_2008 = (N225$N225.Close/(N225[which(N225$data == as.Date("2008-01-07")),4])-1) )




df <- merge(eu,ibov, by = "data")
df <- merge(sp, df, by = "data")
df <- merge(FTSE, df, by = "data")
df <- merge(N225, df, by = "data")

df <- df[,round(c(1,8,9,16,17,24,25,40,41),2)]

teste <- sapply(df[,-1], FUN = function(x){round(x,2)})

teste <- round(df[,-1], 2)


df%>%
  ggplot( aes(x = data, y = ibov_2020))+
  geom_line()


df %>% 
  subset( data >="2020-01-01" & data <= "2020-04-01")%>%
  na.omit() %>% 
  select(data, "IBOV" = ibov_2020,"SP 500" = SP500_2020, "FTSE" = FTSE_2020, "Nikkei" = N225_2020) %>% 
  pivot_longer(-data)%>%
  
  ggplot(aes(x = data, y = value*100, color = name)) +
  geom_line(size = 1.2) +
  labs(title = "Desempenho dos Ã­ndices de aÃ§Ãµes",
       subtitle = " ",
       caption = "Fonte - Yahoo Finance",
       x = " 2020 ",
       y = " % ") +
  
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey", size = 0.5, linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        title = element_text(colour = "deepskyblue4"),
        text = element_text(size = 12, color = "deepskyblue4"),
        legend.position = "top",
        legend.title = element_blank()
  )




