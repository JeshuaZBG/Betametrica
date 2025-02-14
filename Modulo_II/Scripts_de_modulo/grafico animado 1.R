library(openxlsx)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)
library(gganimate)
library(animation)


#gganimate es la librer�a de referencia para hacer que los
#gr�ficos tomen vida propia
#se trata de incorporar 2 o 3 funciones adicionales 
#a los gr�ficos construidos en base a ggplot2
#es decir, una vez que se construyen en ggplot2,
#el resto es s�per sencillo

#esta librer�a puede ser combinada con animation, para grabar
#los gr�ficos, controlar los frames, n�mero de veces
#que se repiten


#Iniciemos desde cero

datos <- read.xlsx("F:\\BETAMETRICA\\CURSOS\\VIRTUALES\\AUTOMATIZACION DE REPORTES\\importacionesperu.xlsx",
                   sheet = "Mensuales",detectDates = T)

datos$periodo <-  seq(as.Date("2014/08/1"), as.Date("2019/05/1"), by = "month")


datosagrupados <- datos %>%
  mutate(anio=year(periodo),
         mes=month(periodo))%>%
  select(anio,mes,everything())%>%
  select(-Total,-periodo)%>%
  #filter(mes<=tail(mes,n=1))%>%
  group_by(anio)

magrupados <- melt(datosagrupados,id.vars = c("anio","mes"))


animado <- ggplot(magrupados,aes(x=factor(anio),y=value))+
  geom_boxplot(aes(fill=variable))+facet_wrap(~variable,scales = "free")+
  geom_jitter(width=0.1,alpha=0.2,aes(color=variable))+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1))


#hay varias formas de transition.

##transition_manual()*

#para controlar manualmente ciertas transiciones, y que
#exista la posibilidad de hacer gr�ficos que se acumulen
# para que las barras o puntos no desaparezcan entre 
#transici�n y transici�n

##transition_reveal()

#para generar transiciones suaves que se vayan
#degradando a trav�s de encontrar un punto 
#intermedio en cada frame
#tambi�n sirve para time serie y gr�ficas tipo linea


##transition_time()*

#Permite controlar las transiciones por estados, generalmente 
#usado para varibles que son time series


##transition_states()*

#Permite controlar las transiciones por estados, generalmente 
#usado para varibles ordinales o cualitativas, no para time series
#tambi�n puede ser combinado con  ease_aes para suavisar las transiciones
#de acuerdo a ciertas funciones, como lineal, cuadr�tica, etc.



# * significa que generalmente son los que m�s se usan


# ---- Veamos un ejemplo progresivamente # ----


animado <- animado+transition_manual(anio,cumulative = T)
animado

#Booom! #El gr�fico se mueve por cajas
#es el m�s b�sico y s�lo se necesit� transiion_manual.
#note que se us� cumulative para mantener las cajas


#Probemos con un par de configuraciones adicionales
#por ejemplo, que muestreel frame (periodo como t�tulo)



animado <- animado+transition_manual(anio,cumulative = T)+
  labs(title = "A�o: {(current_frame)}")
  
animado



animate(animado,end_pause = 4,width=1100, height=600,fps = 5)
anim_save(file="animacion1.gif",animation = last_animation())


#Estos son los principales {frames_}

#{frames} solo muestra el n�mero de frame

#{(current_frame)} cuando la variable x es un factor y se combina con transition manual
#tambi�n se pueden usar funciones como month o year {year(current_frame)}
# siempre que se defina como tal

#{frame_time} combinado con transition_time.
#{nframes} para contar el n�mero de frame y que los muestre, por ejemplo de frame a n_frame
#{closest_state} para mostrar el frame actual siempre que la variable sea factor o texto
# y que el transition sea state






#----- usuando transition_states

#dado que la variable anio es un n�mero, para que muestre los 
#anios en la transici�n, se debe usar
#{closest_state}

animado <- animado+transition_states(anio,
                   transition_length = 2,
                   state_length = 1)+
                   enter_fade() + 
                   exit_shrink()+
                   labs(title = "A�o: {closest_state}")
  


