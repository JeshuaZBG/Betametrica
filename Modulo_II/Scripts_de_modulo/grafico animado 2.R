#Hagamos un gráfico animado para una serie de tiempo


magrupados2 <- melt(datos,id.vars = c("periodo"))

animado2 <- ggplot(data=magrupados2,aes(x=periodo,y=value))+
  geom_line()+facet_wrap(.~variable  ,scales = "free")


animado2 <- animado2+transition_reveal(along = periodo)


animado2 <- animado2+geom_point()+ transition_reveal(along = periodo)+
  labs(title = "Mes de encuesta: {frame_along}")



animado2





# ¡Mucho mejor!