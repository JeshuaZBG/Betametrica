
#----- Gráfico de barras usando ggplot2 #-----
#cuando la variable fecha no es fecha si no caracter
#el flujo es el siguiente:

#data,estéticas,geometrías,complementos

#Cargando la información

library(openxlsx)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)



datos <- read.xlsx("F:\\BETAMETRICA\\CURSOS\\VIRTUALES\\AUTOMATIZACION DE REPORTES\\importacionesperu.xlsx",
                   sheet = "Mensuales",detectDates = T)

str(datos)

# Agrupando un poco los datos para poder analizarlos

datos$periodo <-  seq(as.Date("2014/08/1"), as.Date("2019/05/1"), by = "month")






datos <- datos %>%
  mutate(anio=year(datos$periodo),
         mes=month(datos$periodo)) %>%
  select(anio,mes, everything())  


datosagrupados1 <- datos %>%
  filter(mes<=tail(datos$mes,n=1))%>%
  select(-periodo,-mes,-Total)%>%
  mutate(anio=as.numeric(anio))%>%
  group_by(anio)%>%
  summarise_each(list(sum))

datosagrupados1 <- round(datosagrupados1,2)



#para barras stacked
#x, y, fill
#si hay varias variables, es probable que se deba usar la función melt
# de la librería reshape2


melt1 <- melt(datosagrupados1,id.vars = "anio")


ggplot(melt1, aes(x = anio, y = value, fill = variable)) + 
  geom_bar(stat = "identity")+
  geom_text(aes(y=value, label=value),
            position = position_stack(vjust = 0.5),
            size=4)+
  labs(title="Evolución de las importaciones del Perú",
       subtitle = "En millones de pesos, acumulados de Enero a (fecha de corte)",
       caption = "Fuente: BCP\n Elaboración:autor",
       x="periodo acumulado",y="millones de soles")
last_plot()
ggsave("importacionesstack.png", width = 8, height = 8)  


#basta colocar un coor_flip para voltear las barras

ggplot(melt1, aes(x = anio, y = value, fill = variable)) + 
  geom_bar(stat = "identity")+coord_flip()+
  geom_text(aes(y=value, label=value),
            position = position_stack(vjust = 0.5),
            size=4)+
  labs(title="Evolución de las importaciones del Perú",
       subtitle = "En millones de pesos, acumulados de Enero a (fecha de corte)",
       caption = "Fuente: BCP\n Elaboración:autor",
       x="periodo acumulado",y="millones de soles")+
  scale_fill_brewer(palette = "Spectral")+theme_minimal()
last_plot()
ggsave("importacionesstackflip.png", width = 8, height = 8) 










