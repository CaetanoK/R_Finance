library(quantmod)
library(ggplot2)
library(plotly)
library(ipeadatar)
library(fredr)
library(Quandl)
library(tidyverse)

#SETUP Indices-------------

getSymbols("^GSPC", from = "2008-01-01", to = Sys.Date())
sp <- data.frame(as.xts(GSPC))
sp$data <- as.Date(row.names(sp))
sp <- sp%>%
  mutate(SP500_2020 = round(((sp$GSPC.Close/sp[which(sp$data == as.Date("2020-01-02")),4])-1)*100,2)  )%>%
  mutate(SP500_2008 = round(((sp$GSPC.Close/sp[which(sp$data == as.Date("2008-01-02")),4])-1)*100,2)   )





getSymbols("^BVSP", from = "2008-01-01", to = Sys.Date())
ibov <- data.frame(as.xts(BVSP))
ibov$data <- as.Date(row.names(ibov))
ibov <- ibov%>%
  mutate(ibov_2020 = round((ibov$BVSP.Close/(ibov[which(ibov$data == as.Date("2020-01-03")),4])-1)*100,2) )%>%
  mutate(ibov_2008 = round((ibov$BVSP.Close/(ibov[which(ibov$data == as.Date("2008-01-03")),4])-1)*100,2))
  


getSymbols("^STOXX50E", from = "2008-01-01", to = Sys.Date())
eu <- data.frame(as.xts(STOXX50E))
eu$data <- as.Date(row.names(eu))
eu <- eu%>%
  mutate(eu_2020 = round((eu$STOXX50E.Close/(eu[which(eu$data == as.Date("2020-01-03")),4])-1)*100,2) )%>%
  mutate(eu_2008 = round((eu$STOXX50E.Close/(eu[which(eu$data == as.Date("2008-01-03")),4])-1)*100,2) )
  
  
getSymbols("^FTSE", from = "2008-01-01", to = Sys.Date())
FTSE <- data.frame(as.xts(FTSE))
FTSE$data <- as.Date(row.names(FTSE))
FTSE <- FTSE%>%
  mutate(FTSE_2020 = round((FTSE$FTSE.Close/(FTSE[which(FTSE$data == as.Date("2020-01-02")),4])-1)*100,2) )%>%
  mutate(FTSE_2008 = round((FTSE$FTSE.Close/(FTSE[which(FTSE$data == as.Date("2008-01-02")),4])-1)*100,2) )
  

getSymbols("^N225", from = "2008-01-01", to = Sys.Date())
N225 <- data.frame(as.xts(N225))
N225$data <- as.Date(row.names(N225))
N225 <- N225%>%
  mutate(N225_2020 = round((N225$N225.Close/(N225[which(N225$data == as.Date("2020-01-07")),4])-1)*100,2) )%>%
  mutate(N225_2008 = round((N225$N225.Close/(N225[which(N225$data == as.Date("2008-01-07")),4])-1)*100,2) )




df <- merge(eu,ibov, by = "data")
df <- merge(sp, df, by = "data")
df <- merge(FTSE, df, by = "data")
df <- merge(N225, df, by = "data")

df <- df[,c(1,8,9,16,17,24,25,40,41)]


#GRAFICOS Indices ----------- 

df%>%
  ggplot( aes(x = data, y = ibov_2020))+
  geom_line()
  



df %>% 
  subset( data >="2020-01-01")%>%
  na.omit() %>% 
  select(data, "IBOV" = ibov_2020,"SP 500" = SP500_2020, "FTSE" = FTSE_2020, "Nikkei" = N225_2020) %>% 
  pivot_longer(-data)%>%
  
  ggplot(aes(x = data, y = value, color = name)) +
  geom_line(size = 1.2) +
  labs(title = "Desempenho dos índices de ações",
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



df %>% 
  subset( data >="2008-01-01" & data <="2010-01-01")%>%
  na.omit() %>% 
  select(data, "IBOV" = ibov_2008,"SP 500" = SP500_2008, "FTSE" = FTSE_2008, "Nikkei" = N225_2008) %>% 
  pivot_longer(-data)%>%
  
  ggplot(aes(x = data, y = value, color = name)) +
  geom_line() +
  labs(title = "Desempenho dos índices de ações",
       subtitle = " ",
       caption = "Fonte - Yahoo Finance",
       x = " 2008 - 2010 ",
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

#Curva de Juros Brasil -------------

setwd("C:/Users/Caetano/Desktop/Economia/Financas/Clube de Financas/Nucleo Macro/Titulos Publicos/dados/API")

library(XML)
library(bizdays)
library(stringr)
library(ggplot2)


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


#Modelo

#Veja outras curvas
#DOl - Dollar
#PRE - Pre
#EU - EUR
#DIC - IPCA
#DIM - IGP-M


seq(21, 2520, by=21) -> terms

dates <- seq(as.Date('2020-03-01'), as.Date('2020-04-01'), by='1 week')
dates <- adjust.next(dates)
curves <- lapply(dates, function(d) {
  get_curve(d, 'PRE')(terms)
})
names(curves) <- dates

ir_df <- stack(curves)
ir_df$date <- terms + Sys.Date()
ir_df$values <- round(ir_df$values,2)

ggplot(data=ir_df, aes(x=date, y=values, colour=ind)) + geom_point() + geom_line() +
  xlab('Datas') + ylab('Taxas') + theme(legend.title=element_blank(), legend.position='top') + ggtitle('Curvas de DI1')

ggplot(data=ir_df, aes(x=date, y=values, colour=ind)) + geom_point() + geom_line() +
  theme(legend.title=element_blank(), legend.position='top')  +
  labs(title = "Curva de Juros DI1 ",
       subtitle = " ",
       caption = "Fonte - BM&F",
       x = " 2020 ",
       y = " %a.a ") +
  
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey", size = 0.5, linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        title = element_text(colour = "deepskyblue4"),
        text = element_text(size = 12, color = "deepskyblue4"),
        legend.position = "top",
        legend.title = element_blank()
  )




#CDS -------------
library(ipeadatar)
risco <- ipeadata("JPM366_EMBI366",language = "en")

risco <- risco[,c(2,3)]
colnames(risco) = c("data","EMBI")

risco %>% 
  subset( data >="2020-01-01")%>%
  na.omit() %>% 
  select(data, EMBI) %>% 
  pivot_longer(-data)%>%
  
  ggplot(aes(x = data, y = value, color = name)) +
  geom_line(size = 1.2) +
  labs(title = "EMBI+",
       subtitle = " Risco Brasil ",
       caption = "Fonte - IPEA",
       x = " 2020 ",
       y = "  ") +
  
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey", size = 0.5, linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        title = element_text(colour = "deepskyblue4"),
        text = element_text(size = 12, color = "deepskyblue4"),
        legend.position = "none",
        legend.title = element_blank()
  )

teste <- ipeadatar::search_series(terms = "ima")

#yield americano --------

teste <- fredr_series(series_id = "Treasury")
fredr_set_key

treasury = Quandl("FRED/T10Y3M", start_date="2004-01-03",end_date="2020-04-01",type="raw")


treasury %>% 
  na.omit() %>% 
  select(Date, Value) %>% 
  pivot_longer(-Date)%>%
  
  ggplot(aes(x = Date, y = value, color = name)) +
  geom_line(size = 1) +
  labs(title = "Diferença entre Treasury de 10 anos e 3 meses",
       subtitle = " ",
       caption = "Fonte - IPEA",
       x = "  ",
       y = " % a.a") +
  
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey", size = 0.5, linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        title = element_text(colour = "deepskyblue4"),
        text = element_text(size = 12, color = "deepskyblue4"),
        legend.position = "none",
        legend.title = element_blank(),
        
        
        
  )

