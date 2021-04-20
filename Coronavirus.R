# Coronavirus Moving Average

# Média movel do Nº de casos covid-19

library (ggplot2)
library (zoo)
library (dplyr)
library (lubridate)

setwd('C:/Users/Vinicius Borges/Repos/R-Moving.average-Covid19')


dados <- read.csv('dados/brazil_covid19.csv') %>%
  mutate(date=ymd(dados$date)) %>%
  dplyr::select(date,cases) %>% #Selecionando colunas data e nº de casos
  mutate(Moving.Average10=rollmean(cases,k=10,fill=NA)) #Média movel 10 dias
  agrupados <- group_by(dados,year(date),month(date)) #Agrupar os dias aos meses
  summarise(agrupados,sum(cases))
  g <- summarise(agrupados,sum(cases))
  

ggplot(dados,aes(x=date,y=cases + #Definindo os valores dos dados
  geom_line(aes(col='Cases',lty='Cases'),lwd=0.8) +
  geom_line(aes(y=Moving.Average10,col= 'Moving Average 10 days',
                lty='Moving Average 10 days'),lwd=0.8) + #Media movel
  scale_color_manual(values = c('#000080','#FF0000')) +
  scale_linetype_manual(values = c('solid','longdash')) +
  labs(x= NULL,y='Cases Affected',
       col=NULL,
       linetype=NULL) +
  theme_minimal() +
  theme(legend.position = 'top')

ggsave(filename= 'figs/moving_averagecovid.png',
       width= 9.76,
       height= 6.57,
       scale= 0.87)

  
  
  

