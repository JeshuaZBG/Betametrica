library(openxlsx)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)


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

#facet por anio no parece aporar 
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


#usemos la librería higcharter


library(highcharter)       


hcboxplot(x = magrupados$value, var = magrupados$anio, var2 = magrupados$variable,
          outliers = TRUE) %>% 
  hc_chart(type = "column") 






graficodinamicohc <- highchart() %>% 
  hc_xAxis(categories = datos$periodo) %>% 
  hc_add_series(name = "BienesConsumo", data = datos$BienesConsumo) %>% 
  hc_add_series(name = "MateriaPrima", data = datos$MateriaPrima) %>% 
  hc_add_series(name = "BienesCapital", data = datos$BienesCapital)%>%
  hc_add_series(name = "Total", data = datos$Tota,type="area")%>%
  hc_subtitle(text = "Evolución de las importaciones de Perú",
              align = "left",
              style = list(color = "#2b908f", fontWeight = "bold")) %>% 
  hc_credits(enabled = TRUE,
             text = "www.casaresfelix.com",
             href = "www.casaresfelix.com") %>% 
  hc_legend(align = "left") %>%
  hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
             shared = TRUE, borderWidth = 5) %>% 
  hc_exporting(enabled = TRUE)





##quizá sea una buena idea graficar en barras


#podemos pasar el gráfico anterior y transformarlo en columnas



graficodinamicohc %>%
  hc_chart(type = "column",
           options3d = list(enabled = TRUE, beta = 15, alpha = 15))



#para series cortas puede funcionar. Sin embargo, no es muy vistoso

#Agrupemos la información



datosagrupados2 <- datos %>%
  mutate(anio=year(periodo),
         mes=month(periodo))%>%
  select(anio,mes,everything())%>%
  select(-Total,-periodo)%>%
  filter(mes<=tail(mes,n=1))%>%
  group_by(anio)%>%
  summarise_each(list(sum))




#hagamos un gráfico acumulado

highchart() %>% 
  hc_xAxis(categories = datosagrupados2$anio) %>% 
  hc_add_series(name = "BienesConsumo", data = datosagrupados2$BienesConsumo,
                type="column") %>% 
  hc_add_series(name = "MateriaPrima", data = datosagrupados2$MateriaPrima,
                type="column")%>%
  hc_subtitle(text = "Evolución de las importaciones de Perú",
              align = "left",
              style = list(color = "#2b908f", fontWeight = "bold")) %>% 
  hc_credits(enabled = TRUE,
             text = "www.casaresfelix.com",
             href = "www.casaresfelix.com") %>% 
  hc_legend(align = "left") %>%
  hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
             shared = TRUE, borderWidth = 5) %>% 
  hc_exporting(enabled = TRUE)


