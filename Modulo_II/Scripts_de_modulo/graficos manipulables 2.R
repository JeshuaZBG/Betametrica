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




datos[,2:6] <- round(datos[,2:6],2)



#para graficar varias variables es importante primero
#transformar la data a tipo panel
#con melt

#al usar melt, debemos colocar x, y, fill


melt1 <- melt(datos,id.vars = "periodo")

# hay dos formas de dividir las variables en paneles
#facet_grid: se usa generalmene para variables discretas, y genera paneles por columnas
#facet_wrap: mejor opción. permite generar los gráficos en forma rectangular
#puedes usar scales free para probrar diferencias


ggplot(data=melt1,aes(x=periodo,y=value))+
  geom_line()+facet_grid(.~ variable,scales = "free")

ggplot(data=melt1,aes(x=periodo,y=value))+
  geom_line()+facet_wrap(.~ variable,scales = "free")


#podemos cambiar la orientación del gráfico a filas
ggplot(data=melt1,aes(x=periodo,y=value))+
  geom_line()+facet_grid(variable ~. ,scales = "free")

ggplot(data=melt1,aes(x=periodo,y=value))+
  geom_line()+facet_wrap(variable ~. ,scales = "free")


#wrap también puede controlar el número de columnas

ggplot(data=melt1,aes(x=periodo,y=value))+
  geom_line()+facet_wrap(variable ~. ,scales = "free",ncol = 2)


#usualmentre trabajo con wrap.
#agregemos otras cosas adicionales



ggplot(data=melt1,aes(x=periodo,y=value))+
  geom_line()+facet_wrap(.~variable  ,scales = "free")+
  geom_smooth(se=FALSE, method="lm", formula=y~1, colour="black")  

#incorporemos cosas más técnicas

#install.packages("tidyquant")
#library(tidyquant)  
#install.packages("xml2")


#probemos con una media móvil de 3 meses

graficodinamico2 <- ggplot(data=melt1,aes(x=periodo,y=value))+
  geom_line()+facet_wrap(.~variable ,scales = "free")+
  geom_smooth(aes(color="promedio"),se=FALSE,
              method="lm", formula=y~1,show.legend = TRUE)+
  geom_ma(aes(color="MA(12)"),ma_fun = SMA,
          n = 12, size = 1,
          show.legend = TRUE)+
  scale_colour_manual(name = 'legendas', 
                      values = c('promedio' = 'black',
                                 'MA(12)' = 'red'),
                      labels = c('promedio',
                                 'media movil 12'))+
  geom_smooth(method="lm")+
  scale_x_date(date_labels = "%Y %b",breaks="6 months")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90,
                                   hjust=1,size=7))


library(plotly)
ggplotly(graficodinamico2)








