---
  title: "Prova 4E - Case 1"

# Limpando o workspace
rm(list= ls())

# Definindo o diret�rio
# Obs: colocar seu pr�prio destino
setwd('C:\\Users\\55119\\Downloads')

#Pacote com fun��es para tratamento de dados
install.packages("dplyr")
library("dplyr")
# Pacote para gr�ficos
install.packages('ggplot2')
library("ggplot2")
# Pacote para forecast
install.packages('forecast')
library('forecast')

TFP = read.csv('C:\\Users\\55119\\Downloads\\TFP.csv')

##### 
# An�lise explorat�ria
#####
str(TFP)

# Gerando uma tabela com estat�sticas descritivas b�sicas
explore = group_by(TFP, isocode) %>% summarise(media = mean(rtfpna),
                                               mediana = median(rtfpna),
                                               Q25 = quantile(rtfpna,.25),
                                               Q75 = quantile(rtfpna,.75),
                                               desvpad = sqrt(var(rtfpna)),
                                               media_crescimento = mean(log(rtfpna[2:62])-log(rtfpna[1:61]))*100,
                                               mediana_crescimento = median(log(rtfpna[2:62])-log(rtfpna[1:61]))*100,
                                               Q25_crescimento = quantile(log(rtfpna[2:62])-log(rtfpna[1:61]),.25)*100,
                                               Q75_crescimento = quantile(log(rtfpna[2:62])-log(rtfpna[1:61]),.75)*100,
                                               desvpad_crescimento = sqrt(var((log(rtfpna[2:62])-log(rtfpna[1:61])*100))))
print(explore)

# Gr�ficos
ggplot()+
  geom_line(aes(year,rtfpna), data = TFP %>% dplyr::filter(isocode == 'USA'), col= 'blue')+
  geom_line(aes(year,rtfpna), data = TFP %>% dplyr::filter(isocode == 'MEX'), col= 'green')+
  geom_line(aes(year,rtfpna), data = TFP %>% dplyr::filter(isocode == 'CAN'), col= 'red')+
  xlab('') + ylab('') +
  theme_minimal()


#####
# Forecast
#####                                          

#MEX
MEX = TFP %>% dplyr:: filter(isocode=='MEX')

arimex = auto.arima(MEX$rtfpna ,d=1, D=0, max.p = 4, max.q = 4, max.P = 0, max.Q = 0)

forecast_mex = forecast(arimex, h = 10)

proje�ao_do_mexico =  c(rep(NA,62+10)) 

proje�ao_do_mexico[1:62] = forecast_mex$x
proje�ao_do_mexico[63:72] = forecast_mex$mean

ggplot()+
  geom_line(aes(1950:2021,proje�ao_do_mexico), col = 'green')+
  xlab('')+ ylab('')+
  theme_minimal()

#USA

USA = TFP %>% dplyr:: filter(isocode=='USA')

arimex = auto.arima(USA$rtfpna ,d=1, D=0, max.p = 4, max.q = 4, max.P = 0, max.Q = 0)

forecast_usa = forecast(arimex, h = 10)

proje�ao_usa =  c(rep(NA,62+10)) 

proje�ao_usa[1:62] = forecast_usa$x
proje�ao_usa[63:72] = forecast_usa$mean

ggplot()+
  geom_line(aes(1950:2021,proje�ao_usa), col = 'blue')+
  xlab('')+ ylab('')+
  theme_minimal()

#CAN

CAN = TFP %>% dplyr:: filter(isocode=='CAN')

arimex = auto.arima(CAN$rtfpna ,d=1, D=0, max.p = 4, max.q = 4, max.P = 0, max.Q = 0)

forecast_can = forecast(arimex, h = 10)

proje�ao_can =  c(rep(NA,62+10)) 

proje�ao_can[1:62] = forecast_can$x
proje�ao_can[63:72] = forecast_can$mean

ggplot()+
  geom_line(aes(1950:2021,proje�ao_can), col = 'red')+
  xlab('')+ ylab('')+
  theme_minimal()


#####
# Pergunta 3
#####

# Poderiam ajudar a explicar a TFP: o n�vel do produto, o estoque de capital e o indice de capital humano de cada pa�s:
# - rgdpna Real GDP at constant 2005 national prices (in million 2005 USD)
# - rkna Capital stock at constant 2005 national prices (in million 2005 USD).
# - hc Index of human capital per person, based on years of schooling (Barro and Lee 2013) and returns to education (Psacharopoulos 1994).




