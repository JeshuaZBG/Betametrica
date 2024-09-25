
#----- Gr?fico de lineas usando ggplot2 #-----

#el flujo es el siguiente:

#data,est?ticas,geometr?as,complementos

#Cargando la informaci?n

library(openxlsx)
library(ggplot2)
library(reshape2)
library(scales)


file.choose()
setwd("C:\\Users\\Lenovo T440\\Documents")
datos <- read.xlsx("Libros_cursos_Estadística\\Betamétrica\\M2_AnálisisExporatorio_AutomatizaciónReportes\\data\\BASE DE DATOS.xlsx",
                   sheet = "Hoja1",detectDates = T)
View(datos)

#Estructura de ggplot2 (paso a paso)

#data
#estéticas
#geometrías (x, y, alpha, color, group, linetype, size)
#complementos (titulos, subtitulos, escalas, temas, lineas de intercepción,
#colores, estadísticas, regresiones, etc)




ggplot(data=datos)+
  aes(x=PERIODO)+
  aes(y=EXPORTACIONES)+
  geom_line()+
  geom_point(color="blue",alpha=0.5,size=1.5)+
  geom_hline(yintercept = mean(datos$EXPORTACIONES), col = "grey")+
  geom_vline(xintercept = as.numeric(
    as.Date(tail(datos$PERIODO,n=1))), linetype=1)+
  geom_vline(xintercept = as.numeric(
    as.Date(tail(datos$PERIODO,n=13)[1])), linetype=4)+
  annotate(geom = "text",
           x = as.Date(tail(datos$PERIODO,n=1)),
           y=2000000,
           label="crisis",angle=90,size=4)+
  labs(title="Evolución de las exportaciones del Ecuador",
       subtitle = "En miles de millones",
       caption = "Fuente: BCE\n Elaboración:autor")+
  theme(text = element_text(size=14),legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust=1,size=9))+ #un poco de cambios en fechas
  scale_x_date(date_labels = "%Y %b",breaks=datos$PERIODO)
  
  #scale_x_date(date_labels = "%Y %b",breaks = scales::pretty_breaks(n = 12))

last_plot()
ggsave("exportaciones.png", width = 8, height = 5)
  

dev.off()





