
#----- Gráfico de barras usando ggplot2 #-----

#el flujo es el siguiente:

#data,estéticas,geometrías,complementos

#Cargando la información

library(openxlsx)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)



datos <- read.xlsx("F:\\BETAMETRICA\\CURSOS\\VIRTUALES\\AUTOMATIZACION DE REPORTES\\BASE DE DATOS.xlsx",
                   sheet = "Hoja1",detectDates = T)

#Estructura de ggplot2 (paso a paso)

#data
#estéticas
#geometrías (x, y, alpha, color, group, linetype, size)
#complementos (titulos, subtitulos, escalas, temas, lineas de intercepción,
#colores, estadísticas, regresiones, etc)




ggplot(data=datos)+
  aes(x=PERIODO)+
  aes(y=EXPORTACIONES)+
  geom_bar(position = "dodge",stat="identity")
  

# Agrupando un poco los datos para poder analizarlos

datos <- datos %>%
         mutate(anio=year(datos$PERIODO),
                mes=month(datos$PERIODO)) %>%
         select(anio,mes, everything())  


datosagrupados <- datos %>%
                  filter(mes<=tail(datos$mes,n=1))%>%
                  select(-PERIODO)%>%
                  mutate(anio=as.numeric(anio))%>%
                  group_by(anio)%>%
                  summarise_each(list(sum))

datosagrupados <- round(datosagrupados,2)
  

ggplot(data=datosagrupados)+
  aes(x=anio)+
  aes(y=EXPORTACIONES)+
  geom_bar(position = "dodge",stat="identity")+
  geom_text(aes(label=EXPORTACIONES), vjust=0, color="black", size=5)+
  labs(title="Evolución de las exportaciones del Ecuador",
       subtitle = "En miles de millones",
       caption = "Fuente: BCE\n Elaboración:autor")
  last_plot()
ggsave("exportacionesbarra.png", width = 8, height = 5)



# Barras verticales
ggplot(data=datosagrupados)+
  aes(x=anio)+
  aes(y=EXPORTACIONES)+
  geom_bar(position = "dodge",stat="identity")+coord_flip()+
  geom_text(aes(label=EXPORTACIONES), vjust=0, color="black", size=4,
            position = position_dodge(width = 1),
            angle=270)+
  labs(title="Evolución de las exportaciones del Ecuador",
       subtitle = "En miles de millones",
       caption = "Fuente: BCE\n Elaboración:autor")
last_plot()
ggsave("exportacionesbarraflip.png", width = 8, height = 8)



