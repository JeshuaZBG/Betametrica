
#Gr�ficos manipulables usando dygraphs para R
#Esta librer�a es s�per �til, ya la sintaxis se parece
#a la de ggplot. Adicionalmente, produce gr�ficos
#manipulables, los cuales pueden ser exportados en formato html
#e incrustados en reportes con formato html y presentaciones ioslide.

#Para graficar con esta librer�a, las series deben ser xts, ts.
#Produce gr�ficos muy elegantes y con c�digos sencillos de entender



#Cargando la informaci�n

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

#adem�s la posibilidad de exportar el gr�fico como un archivo.



#Veamos:

#La idea es que dygraphs, graf�ca un vector o una matriz datafrme 
#con una solo funci�n del mismo nombre
#esto es �til si se lo acompa�a con las opciones que proporciona
#con dyOption

dygraph(tsdatos,main="Evoluci�n de las Exportacioes",
        xlab ="periodo",
        ylab = "millones de soles") %>%
  dyOptions(fillGraph = T,fillAlpha = 0.04,
            drawPoints = T,pointSize = 3,
            pointShape = "triangle",gridLineColor = "blue")


#veamos los realces

  

dygraph(tsdatos,main="Evoluci�n de las Exportacioes",
        xlab ="periodo",
        ylab = "millones de soles") %>%
  dyOptions(fillGraph = T,fillAlpha = 0.04,
            drawPoints = T,pointSize = 3,
            pointShape = "star",gridLineColor = "blue")%>%
  dyHighlight(highlightCircleSize = 8,
              highlightSeriesBackgroundAlpha = 1,hideOnMouseOut = F,
              highlightSeriesOpts = list(strokeWidth = 3))


# veamos el rango seleccionador


dygraph(tsdatos,main="Evoluci�n de las Exportacioes",
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

graficodinamico <- dygraph(tsdatos,main="Evoluci�n de las Exportacioes",
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

#un ultima cosa: hagamos que los datos sigan seg�n el cursor del mouse

graficodinamico <- dygraph(tsdatos,main="Evoluci�n de las Exportacioes",
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


#�c�mo guardo este gr�fico?

library(htmlwidgets)
saveWidget(graficodinamico,
           file = "graficodinamico.html")





