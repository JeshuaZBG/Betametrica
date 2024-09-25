
#Gráficos manipulables usando dygraphs para R
#Esta librería es súper útil, ya la sintaxis se parece
#a la de ggplot. Adicionalmente, produce gráficos
#manipulables, los cuales pueden ser exportados en formato html
#e incrustados en reportes con formato html y presentaciones ioslide.

#Para graficar con esta librería, las series deben ser xts, ts.
#Produce gráficos muy elegantes y con códigos sencillos de entender



#Cargando la información

library(openxlsx)
library(lubridate)
library(dplyr)
library(dygraphs)



datos <- read.xlsx("F:\\BETAMETRICA\\CURSOS\\VIRTUALES\\AUTOMATIZACION DE REPORTES\\importacionesperu.xlsx",
                   sheet = "Mensuales",detectDates = T)

datos <- datos%>%
         select(BienesConsumo,MateriaPrima,BienesCapital)  

tsdatos <- ts(datos,start = c(2014,8),frequency = 12)

dygraph(tsdatos)

#dygraphs tiene algunas opciones que las iremos viendo progresivamente:

#opciones
#realces o relieves
#rango selector
#anotaciones
#lineas y sombreados para eventos

#además la posibilidad de exportar el gráfico como un archivo.



#Veamos:

#La idea es que dygraphs, grafíca un vector o una matriz datafrme 
#con una solo función del mismo nombre
#esto es útil si se lo acompaña con las opciones que proporciona
#con dyOption

dygraph(tsdatos,main="Evolución de las Exportacioes",
        xlab ="periodo",
        ylab = "millones de soles") %>%
  dyOptions(fillGraph = T,fillAlpha = 0.04,
            drawPoints = T,pointSize = 3,
            pointShape = "triangle",gridLineColor = "blue")


#veamos los realces

  

dygraph(tsdatos,main="Evolución de las Exportacioes",
        xlab ="periodo",
        ylab = "millones de soles") %>%
  dyOptions(fillGraph = T,fillAlpha = 0.04,
            drawPoints = T,pointSize = 3,
            pointShape = "star",gridLineColor = "blue")%>%
  dyHighlight(highlightCircleSize = 8,
              highlightSeriesBackgroundAlpha = 1,hideOnMouseOut = F,
              highlightSeriesOpts = list(strokeWidth = 3))


# veamos el rango seleccionador


dygraph(tsdatos,main="Evolución de las Exportacioes",
        xlab ="periodo",
        ylab = "millones de soles") %>%
  dyOptions(fillGraph = T,fillAlpha = 0.04,
            drawPoints = T,pointSize = 3,
            pointShape = "star",gridLineColor = "blue")%>%
  dyHighlight(highlightCircleSize = 8,
              highlightSeriesBackgroundAlpha = 1,hideOnMouseOut = F,
              highlightSeriesOpts = list(strokeWidth = 3))%>%
  dyRangeSelector(dateWindow = c("2017-01-01","2018-01-01"))


#veamos las anotaciones y regiones de sombreado

graficodinamico <- dygraph(tsdatos,main="Evolución de las Exportacioes",
        xlab ="periodo",
        ylab = "millones de soles") %>%
  dyOptions(fillGraph = T,fillAlpha = 0.04,
            drawPoints = T,pointSize = 3,
            pointShape = "star",gridLineColor = "blue")%>%
  dyHighlight(highlightCircleSize = 8,
              highlightSeriesBackgroundAlpha = 1,hideOnMouseOut = F,
              highlightSeriesOpts = list(strokeWidth = 3))%>%
  dyRangeSelector()%>%
  dyAnnotation("2016-01-01",text = "IE",tooltip = "Inicio Recesivo")%>%
  dyShading(from = "2016-02-01", to = "2016-12-01",color = "#99d8c9")%>%
  dyShading(from = "2018-02-01", to = "2018-12-01",color = "#e7e1ef")%>%
  dyEvent("2016-01-01","inicio recesivo",labelLoc = "top")

#un ultima cosa: hagamos que los datos sigan según el cursor del mouse

graficodinamico <- dygraph(tsdatos,main="Evolución de las Exportacioes",
                           xlab ="periodo",
                           ylab = "millones de soles") %>%
  dyOptions(fillGraph = T,fillAlpha = 0.04,
            drawPoints = T,pointSize = 3,
            pointShape = "star",gridLineColor = "blue")%>%
  dyHighlight(highlightCircleSize = 8,
              highlightSeriesBackgroundAlpha = 1,hideOnMouseOut = F,
              highlightSeriesOpts = list(strokeWidth = 3))%>%
  dyRangeSelector()%>%
  dyAnnotation("2016-01-01",text = "IE",tooltip = "Inicio Recesivo")%>%
  dyShading(from = "2016-02-01", to = "2016-12-01",color = "#99d8c9")%>%
  dyShading(from = "2018-02-01", to = "2018-12-01",color = "#e7e1ef")%>%
  dyEvent("2016-01-01","inicio recesivo",labelLoc = "top")%>%
  dyLegend(show="follow")


#¿cómo guardo este gráfico?

library(htmlwidgets)
saveWidget(graficodinamico,
           file = "graficodinamico.html")





