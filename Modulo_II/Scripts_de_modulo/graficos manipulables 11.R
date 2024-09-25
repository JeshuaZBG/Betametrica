library(openxlsx)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)
library(plotly)


#probemos con algo más seleccionable

datos <- read.xlsx("F:\\BETAMETRICA\\CURSOS\\VIRTUALES\\AUTOMATIZACION DE REPORTES\\importacionesperu.xlsx",
                   sheet = "Mensuales",detectDates = T)

datos$periodo <-  seq(as.Date("2014/08/1"), as.Date("2019/05/1"), by = "month")



datosagrupados <- datos %>%
  mutate(anio=year(periodo),
         mes=month(periodo))%>%
  select(anio,mes,everything())%>%
  select(-Total,-periodo)%>%
  filter(mes<=tail(mes,n=1))%>%
  group_by(anio)%>%
  summarise_each(list(sum))%>%
  select(-mes)  

magrupados <- melt(datosagrupados,id.vars = "anio")


agrupados2 <- ggplot(magrupados,aes(x=anio,y=value,group=variable))+
  geom_area(aes(color=variable,fill=variable),alpha=0.2)+
  theme(legend.position = "bottom")+theme_minimal()


ggplotly(agrupados2)
