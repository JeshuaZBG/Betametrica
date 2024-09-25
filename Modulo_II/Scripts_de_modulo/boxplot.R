library(openxlsx)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)


#probemos con algo más seleccionable

datos <- read.xlsx("F:\\BETAMETRICA\\CURSOS\\VIRTUALES\\AUTOMATIZACION DE REPORTES\\importacionesperu.xlsx",
                   sheet = "Mensuales",detectDates = T)

datos$periodo <-  seq(as.Date("2014/08/1"), as.Date("2019/05/1"), by = "month")



# hagamos un boxplot por 
# hagamos un nuevo filtro


datosagrupados <- datos %>%
  mutate(anio=year(periodo),
         mes=month(periodo))%>%
  select(anio,mes,everything())%>%
  select(-Total,-periodo)%>%
  #filter(mes<=tail(mes,n=1))%>%
  group_by(anio)

magrupados <- melt(datosagrupados,id.vars = c("anio","mes"))


#probemos un primer intento

ggplot(magrupados,aes(x=anio,y=value,fill=variable))+
  geom_boxplot()+facet_wrap(~variable,scales = "free")+
  theme(legend.position = "bottom")


#cambiando el facet para analizar por anio, donde las x son las variables.
#agregemos jitter generar una aleatoriedad en los datos
#y se los pueda visualizar como dispersión.


ggplot(magrupados,aes(x=variable,y=value,fill=variable))+
  geom_boxplot()+
  geom_jitter(width=0.1,alpha=0.2)+
  facet_wrap(~anio,scales = "free")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=90,hjust = 1))



# veamos cuando las x son anios y el facet es por variable.
# es decir, generemos el boxplot por anio, separando por variable 
# en el encabezado del gráfico

ggplot(magrupados,aes(x=factor(anio),y=value))+
  geom_boxplot(aes(fill=variable))+facet_wrap(~variable,scales = "free")+
                 geom_jitter(width=0.1,alpha=0.2,aes(color=variable))+
                 theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1))
               
               # ¡Mucho mejor!
               


