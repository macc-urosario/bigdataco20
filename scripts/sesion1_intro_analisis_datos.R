# Este codigo presenta elemtos básicos para cargas y analisis estructuras de datos


#### Curso de R ----
# Para quienes estar iniciando en programación, puede usar el paquete 
# swirl para aprender sobre las herramientas básicas

install.packages("swirl") # Instalación de paquetes
library("swirl")          # Cargar librerias

swirl()                   # Activar curso 
                          # Buscar cursos de regresión
                          # https://swirlstats.com/scn/title.html
bye()                     # Terminar curso

# Primer paso consiste en establecer el directorio de trabajo
setwd("D:\\Google Drive\\U Rosario\\Conferences\\Big Data CO\\bigdataco2020_programa\\datasets")


#### Analisis de datos de corte transversal ----
# Real estate valuation data set Data Set
# https://archive.ics.uci.edu/ml/datasets/Real+estate+valuation+data+set
install.packages("readxl")
library("readxl")

real.estate <- read_excel("Real estate valuation data set.xlsx")
names(real.estate)
names(real.estate) <- c("n", "date", "age", "dis_MRT", "stores", "lat",
                        "lon", "price")
real.estate <- as.data.frame(real.estate)
str(real.estate)


cor(real.estate[, c("age", "dis_MRT", "stores", "price")])

plot(real.estate$dis_MRT, real.estate$price, main = "price vs distance to transport",
     xlab = "Distance", ylab = "Price",
     pch = 19, frame = FALSE)
abline(lm(price ~ dis_MRT, data = real.estate), col = "blue")


#### Datos geolocalizados ----
install.packages("ggmap")
library (ggmap)
qmplot(lon, lat, data = real.estate, colour = I('red'), size = I(3), darken = .3)

### Series de tiempo ----
library(fpp2)
air.qual <- read.csv("PRSA_data_2010.1.1-2014.12.31.csv")

# Frqs: https://robjhyndman.com/hyndsight/seasonal-periods/
ts.air <- ts(air.qual$pm2.5,start=c(2010),frequency=8766)
# Datos por hora con ciclos anuales

autoplot(ts.air)
ggseasonplot(ts.air, year.labels = T)

# Sub-data
ts.air.sub <- ts(air.qual$pm2.5[1:12000],start=c(2010,1,1),frequency=8766)

autoplot(ts.air.sub)
ggseasonplot(ts.air.sub, year.labels = T)

#### Scrapping data y mapa ----
# Dats de expectativa de vida
library(rvest)
library(ggplot2)
library(dplyr)
library(scales)

le <-  read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_by_life_expectancy")
class(le)
# Extrae la tabla
le <- le %>%
  html_nodes("table") %>%
  .[[2]]%>%
  html_table(fill=T)

# Cambio de formato
class(le)
str(le)
names(le)[2] <- "le_2018"
le$region = tolower(le$State)

states <-  map_data("state")
class(states)
str(states)

# Integrar bases
states <-  merge(states, le, by="region", all.x=T)

ggplot(states, aes(x = long, y = lat, group = group, fill = le_2018)) + 
  geom_polygon(color = "white")


### Datos provenientes de Google Trends ----
library(gtrendsR) 
# definir parametros
# pzifer, vacuna
palabra.clave <- c( "salario minimo")
pais <- c("CO")
periodo=c("2010-01-01 2020-12-3")
channel <- 'web'

trends <-  gtrends(palabra.clave, gprop =channel, 
                   geo=pais, time = periodo )

#select only interst over time 
tendencia <- trends$interest_over_time
head(tendencia)

library(ggplot2)
plot<-ggplot(data=tendencia, aes(x=date, y=hits))+
  geom_line()+xlab('Time')+ylab('Relative Interest')+ theme_bw()+
  theme(legend.title = element_blank(),legend.position="bottom",legend.text=element_text(size=12))+ggtitle("Google Search Volume")
plot


