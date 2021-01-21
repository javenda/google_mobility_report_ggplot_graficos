########################
#todas las funciones llevan de argumento
#parametros generacion de graficos
#paises = vector con paises a seleccionar. listado en 'listado paises'
# sub_nac=FALSE vector con niveles sub_nacionales. Consultar listado con 'levels_sub_reg1('nombre_pais)'
# post=FALSE filtrar solo fechas posteriores a inicio_ven
# weekend=FALSE excluir fechas de fines de semana
# inicio=FALSE seleccion intervalo fecha inicio formato ('Y-m-d')
# fin=FALSE)seleccion intervalo fecha fin formato ('Y-m-d')
# elemento= tipo de dato del GMR q se grafica. lista en 'tipo_datos'
# hitos=añade lineas verticales para fechas guardadas en df_eventos

#1) grafico de barra
gg_barra <- function(paises, 
                     sub_nac=FALSE,
                     post=FALSE,
                     weekend=FALSE,
                     inicio=FALSE,
                     fin=FALSE,
                     elemento,
                     hitos=FALSE){
  
  df <- global_filtros(paises = paises,
                       sub_nac = sub_nac,
                       post=post,
                       weekend = weekend,
                       inicio= inicio,
                       fin=fin)
  #variables de ayuda para grafico
  fecha_f=max(global$fecha)
  fecha_i=min(global$fecha)
  posicion_leyenda=2
  if(inicio!=FALSE){fecha_i=as.Date(inicio)}
  if(fin!=FALSE){fecha_f=as.Date(fin)}
  if(mean(df[,elemento], na.rm = TRUE)<0){posicion_leyenda=-10}
  
  
  nombre_paises=paste(paste(paises,collapse = '-'),
                      if(sub_nac){paste(sub_nac,collapse = '-')})
  
  ggplot(df,aes(x=fecha, colour='indianred1'))+
    aes_string(y=elemento)+
    geom_hline(aes( yintercept=0), color="black", size=.3, alpha=.8)+
    geom_col()+
    labs(subtitle = paste('Sector ',gsub('_',' ',elemento),". Del ",format(min(df$fecha),'%d-%m-%Y'),
                          ' al',format(max(df$fecha),'%d-%m-%Y')) ,
         caption = caption,
         #tag = "Gráfico 1",
         x = "fecha",
         y = "%",
         colour = "País")+
    #scale_x_date(date_breaks='1 week',labels = date_format('%d-%m-%Y'))+
    scale_x_date(date_breaks='4 week',labels = date_format('%m-%Y'))+
    ggtitle(paste('Cambios en movilidad: ',nombre_paises))+
    theme(plot.title=element_text(size=rel(1.5),
                                  lineheight=.9,family="Times",
                                  face="bold.italic",colour="grey38"))+
    theme(legend.position = 'none')+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_vline(data=df_eventos, mapping=aes(xintercept=a), color="orange",linetype="dotted", size=.5, alpha=.5)}+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_text(data=df_eventos,aes(x=a, label=b , y = posicion_leyenda), colour="black", angle=90, size=1.8)}
}



#2 lineas en eje x comparar distintos 'tipos de datos
gg_lineas <- function(paises, 
                      sub_nac=FALSE,
                      post=FALSE,
                      weekend=FALSE,
                      inicio=FALSE,
                      fin=FALSE,
                      elemento,
                      hitos=FALSE){
  
  df <- global_filtros(paises = paises,
                       sub_nac = sub_nac,
                       post=post,
                       weekend = weekend,
                       inicio= inicio,
                       fin=fin)
  #variables de ayuda para grafico
  fecha_f=max(global$fecha)
  fecha_i=min(global$fecha)
  posicion_leyenda=2
  if(inicio!=FALSE){fecha_i=as.Date(inicio)}
  if(fin!=FALSE){fecha_f=as.Date(fin)}
  if(mean(df[,elemento[1]], na.rm = TRUE)<0){posicion_leyenda=-10}
  
  
  nombre_paises=paste(paste(paises,collapse = '-'),
                      if(sub_nac){paste(sub_nac,collapse = '-')})
  
  ggplot(df, aes(x=fecha)) +
    ylab('')+
    geom_line(aes_string(y=elemento[1],color=shQuote("one"))) +
    geom_hline(aes( yintercept=0), color="black", size=.3, alpha=.8)+
    geom_point(aes_string(y=elemento[1],color=shQuote("one"))) + #
    geom_line(aes_string(y=elemento[2],color=shQuote("two"))) + #
    geom_point(aes_string(y=elemento[2],color=shQuote("two"))) + #color=shQuote("two")
    scale_color_discrete(name = "datos", labels = c(elemento[1], elemento[2]))+
    
    
    #aes(colour= 'black'))+
    labs(subtitle = paste('Sector ',gsub('_',' ',elemento),". Del ",format(min(df$fecha),'%d-%m-%Y'),
                          ' al',format(max(df$fecha),'%d-%m-%Y')) ,
         caption = caption,
         #tag = "Gráfico 1",
         x = "fecha",
         y = "%",
         colour = "País")+
    #scale_x_date(date_breaks='2 week',labels = date_format('%d-%m-%Y'))+
    scale_x_date(date_breaks='4 week',labels = date_format('%m-%Y'))+
    ggtitle(paste('Cambios en movilidad: ',nombre_paises))+
    theme(plot.title=element_text(size=rel(1.5),
                                  lineheight=.9,family="Times",
                                  face="bold.italic",colour="grey38"))+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_vline(data=df_eventos, mapping=aes(xintercept=a), color="orange",linetype="dotted", size=.5, alpha=.5)}+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_text(data=df_eventos,aes(x=a, label=b , y = posicion_leyenda), colour="black", angle=90, size=1.8)}
}


##############################################
#3 comparacion paises por elemento
gg_comp_paises <- function(paises, 
                           sub_nac=FALSE,
                           post=FALSE,
                           weekend=FALSE,
                           inicio=FALSE,
                           fin=FALSE,
                           elemento,
                           hitos=FALSE){
  #llamada a funcion para generar df
  df <- global_filtros(paises = paises,
                       sub_nac = sub_nac,
                       post=post,
                       weekend = weekend,
                       inicio= inicio,
                       fin=fin)
  
  #variables de ayuda para grafico
  fecha_f=max(global$fecha)
  fecha_i=min(global$fecha)
  posicion_leyenda=2
  if(inicio!=FALSE){fecha_i=as.Date(inicio)}
  if(fin!=FALSE){fecha_f=as.Date(fin)}
  if(mean(df[,elemento], na.rm = TRUE)<0){posicion_leyenda=-10}
  
  nombre_paises=paste(paste(paises,collapse = '-'),
                      if(sub_nac){paste(sub_nac,collapse = '-')})
  ggplot(df, aes(x=fecha,colour=region) ) +
    aes_string(y=elemento)+
    geom_line(size=.8)+
    #geom_point()+
    geom_hline(aes( yintercept=0), color="black", size=.3, alpha=.8)+
    theme_hc()+
    #theme(panel.background = element_rect( colour = "#6D9EC1",#fill = "snow",
                                      #size = 2, linetype = "solid"))+
    theme(legend.position = 'bottom')+
    labs(subtitle = paste('Sector: ',gsub('_',' ',elemento),". desde: ",format(min(df$fecha),'%d-%m-%Y'),
                          ' hasta:',format(max(df$fecha),'%d-%m-%Y')) ,
         caption = caption,
         #tag = "Gráfico 1",
         x = "fecha",
         y = "%",
         colour = "País")+
    
    #scale_color_viridis(discrete = TRUE, option = "D")+
    #scale_fill_viridis(discrete = TRUE) +
    #scale_x_date(date_breaks='1 week',labels = date_format('%d-%m-%Y'))+
    scale_x_date(date_breaks='4 week',labels = date_format('%m-%Y'))+
    ggtitle(paste('Cambios en movilidad: ',nombre_paises))+
    theme(plot.title=element_text(size=rel(1.5),
                                  lineheight=.9,family="Times",
                                  face="bold.italic",colour="grey38"))+
    theme(panel.background = element_rect(fill = 'grey90', colour = 'red'))+
    scale_color_manual(values = wes_palette("Royal1", n = length(paises)))+#BottleRocket1,Darjeeling2,Royal1
    #scale_color_brewer(palette = "Set1")+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_vline(data=df_eventos, mapping=aes(xintercept=a), color="red",linetype="dotted", size=.5, alpha=.5)}+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_text(data=df_eventos,aes(x=a, label=b , y = posicion_leyenda), colour="black", angle=90, size=1.8)}
}  


#####################
#4 comparaciones paises en un mismo plano
gg_comp_paises_mismo_plano  <- function(paises, 
                                        sub_nac=FALSE,
                                        post=FALSE,
                                        weekend=FALSE,
                                        inicio=FALSE,
                                        fin=FALSE,
                                        elemento,
                                        hitos=FALSE){
  
  df <- global_filtros(paises = paises,
                       sub_nac = sub_nac,
                       post=post,
                       weekend = weekend,
                       inicio= inicio,
                       fin=fin)
  #variables de ayuda para grafico
  fecha_f=max(global$fecha)
  fecha_i=min(global$fecha)
  posicion_leyenda=2
  if(inicio!=FALSE){fecha_i=as.Date(inicio)}
  if(fin!=FALSE){fecha_f=as.Date(fin)}
  if(mean(df[,elemento], na.rm = TRUE)<0){posicion_leyenda=-10}
  
  nombre_paises=paste(paste(paises,collapse = '-'),
                      if(sub_nac){paste(sub_nac,collapse = '-')})
  
  ggplot(df, aes(x=fecha,colour=region)) +
    aes_string(y=elemento)+
    geom_line()+
    #geom_point()+
    geom_hline(aes( yintercept=0), color="black", size=.3, alpha=.8)+
    geom_smooth(method = "lm", se=FALSE)+
    stat_smooth(method = lm, se=FALSE, size=.5)+
    scale_colour_brewer(palette = 'Set1')+
    facet_grid(region~.)+
    theme_linedraw()+
    scale_fill_brewer('set1')+
    theme(
      panel.background = element_rect(fill = "snow", colour = "#6D9EC1",
                                      size = 2, linetype = "solid"))+
    labs(subtitle = paste('Sector ',gsub('_',' ',elemento),". Del ",format(min(df$fecha),'%d-%m-%Y'),
                          ' al',format(max(df$fecha),'%d-%m-%Y')) ,
         caption = caption,
         #tag = "Gráfico 1",
         x = "fecha",
         y = "%",
         colour = "País")+
    #scale_x_date(date_breaks='2 week',labels = date_format('%d-%m-%Y'))+
    scale_x_date(date_breaks='4 week',labels = date_format('%m-%Y'))+
    ggtitle(paste('Cambios en movilidad',nombre_paises))+
    theme(plot.title=element_text(size=rel(1.5),
                                  lineheight=.9,family="Times",
                                  face="bold.italic",colour="grey38"))+
    theme(legend.position = 'bottom')+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_vline(data=df_eventos, mapping=aes(xintercept=a), color="orange",linetype="dotted", size=.5, alpha=.5)}+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_text(data=df_eventos,aes(x=a, label=b , y = posicion_leyenda), colour="black", angle=90, size=1.8)}
  
}


####################################
#5 comparar elemento en niveles sub nacionales de un pais
gg_comp_regiones <- lineas <- function(paises, 
                                       sub_nac=FALSE,
                                       post=FALSE,
                                       weekend=FALSE,
                                       inicio=FALSE,
                                       fin=FALSE,
                                       elemento,
                                       hitos=FALSE){
  
  df <- global_filtros_dos(paises = paises,
                           sub_nac = sub_nac,
                           post=post,
                           weekend = weekend,
                           inicio= inicio,
                           fin=fin)
  #variables de ayuda para grafico
  fecha_f=max(global$fecha)
  fecha_i=min(global$fecha)
  posicion_leyenda=2
  if(inicio!=FALSE){fecha_i=as.Date(inicio)}
  if(fin!=FALSE){fecha_f=as.Date(fin)}
  if(mean(df[,elemento], na.rm = TRUE)<0){posicion_leyenda=-10}
  
  
  nombre_paises=paste(paste(paises,collapse = '-'),'en: ',
                      if(sub_nac!=FALSE){paste(sub_nac,collapse = '-')})
  
  ggplot(df, aes(x=fecha,colour=sub_reg1) ) +
    aes_string(y=elemento)+
    geom_hline(aes( yintercept=0), color="black", size=.3, alpha=.8)+
    geom_line()+
    #geom_point()+
    scale_fill_brewer('set1')+
    theme(
      panel.background = element_rect(fill = "snow", colour = "#6D9EC1",
                                      size = 2, linetype = "solid"))+
    labs(subtitle = paste('Sector ',gsub('_',' ',elemento),". Del ",format(min(df$fecha),'%d-%m-%Y'),
                          ' al',format(max(df$fecha),'%d-%m-%Y')) ,
         caption = caption,
         #tag = "Gráfico 1",
         x = "fecha",
         y = "%",
         colour = "País")+
    #scale_x_date(date_breaks='2 week',labels = date_format('%d-%m-%Y'))+
    scale_x_date(date_breaks='4 week',labels = date_format('%m-%Y'))+
    ggtitle(paste('Cambios en movilidad ',nombre_paises))+
    theme(plot.title=element_text(size=rel(1.5),
                                  lineheight=.9,family="Times",
                                  face="bold.italic",colour="grey38"))+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_vline(data=df_eventos, mapping=aes(xintercept=a), color="orange",linetype="dotted", size=.5, alpha=.5)}+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_text(data=df_eventos,aes(x=a, label=b , y = posicion_leyenda), colour="black", angle=90, size=1.8)}
  
}


##################################################
#6) comparar regiones_pais (falta completar y no filtrar pais)
gg_comp_regiones_pais <- lineas <- function(paises, 
                                            sub_nac=FALSE,
                                            post=FALSE,
                                            weekend=FALSE,
                                            inicio=FALSE,
                                            fin=FALSE,
                                            elemento,
                                            hitos=FALSE){
  
  df <- global_filtros_dos(paises = paises,
                           sub_nac = sub_nac,
                           post=post,
                           weekend = weekend,
                           inicio= inicio,
                           fin=fin)
  df <- df%>%
    filter(sub_reg1!='') -> df
  #variables de ayuda para grafico
  fecha_f=max(global$fecha)
  fecha_i=min(global$fecha)
  posicion_leyenda=2
  if(inicio!=FALSE){fecha_i=as.Date(inicio)}
  if(fin!=FALSE){fecha_f=as.Date(fin)}
  if(mean(df[,elemento], na.rm = TRUE)<0){posicion_leyenda=-10}
  
  
  nombre_paises=paste(paste(paises,collapse = '-'),'en: ',
                      if(sub_nac!=FALSE){paste(sub_nac,collapse = '-')})
  
  ggplot(df, aes(x=fecha,colour=sub_reg1) ) +
    aes_string(y=elemento)+
    geom_line()+
    geom_hline(aes( yintercept=0), color="black", size=.3, alpha=.8)+
    #geom_point()+
    scale_fill_brewer('set1')+
    theme(
      panel.background = element_rect(fill = "snow", colour = "#6D9EC1",
                                      size = 2, linetype = "solid"))+
    labs(subtitle = paste('Sector ',gsub('_',' ',elemento),". Del ",format(min(df$fecha),'%d-%m-%Y'),
                          ' al',format(max(df$fecha),'%d-%m-%Y')) ,
         caption = caption,
         #tag = "Gráfico 1",
         x = "fecha",
         y = "%",
         colour = "País")+
    #scale_x_date(date_breaks='2 week',labels = date_format('%d-%m-%Y'))+
    scale_x_date(date_breaks='4 week',labels = date_format('%m-%Y'))+
    ggtitle(paste('Cambios en movilidad ',nombre_paises))+
    theme(plot.title=element_text(size=rel(1.5),
                                  lineheight=.9,family="Times",
                                  face="bold.italic",colour="grey38"))+
    theme(legend.position = 'bottom')+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_vline(data=df_eventos, mapping=aes(xintercept=a), color="orange",linetype="dotted", size=.5, alpha=.5)}+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_text(data=df_eventos,aes(x=a, label=b , y = posicion_leyenda), colour="black", angle=90, size=1.8)}
}


###############################
#7animacion

gg_animado_paises <- lineas <- function(paises, 
                                        sub_nac=FALSE,
                                        post=FALSE,
                                        weekend=FALSE,
                                        inicio=FALSE,
                                        fin=FALSE,
                                        elemento,
                                        height_v = 400, 
                                        width_v =800,
                                        hitos=FALSE){
  
  df <- global_filtros(paises = paises,
                       sub_nac = sub_nac,
                       post=post,
                       weekend = weekend,
                       inicio= inicio,
                       fin=fin)
  #variables de ayuda para grafico
  fecha_f=max(global$fecha)
  fecha_i=min(global$fecha)
  posicion_leyenda=2
  if(inicio!=FALSE){fecha_i=as.Date(inicio)}
  if(fin!=FALSE){fecha_f=as.Date(fin)}
  if(mean(df[,elemento], na.rm = TRUE)<0){posicion_leyenda=-10}
  
  
  nombre_paises=paste(paste(paises,collapse = '-'),
                      if(sub_nac){paste(sub_nac,collapse = '-')})
  
  animado <- ggplot(df, aes(x=fecha,,colour=region) ) +
    aes_string(y=elemento)+
    geom_hline(aes( yintercept=0), color="black", size=.3, alpha=.8)+
    geom_line(size=.8)+
    geom_point()+
    scale_colour_brewer(palette = 'Set1')+
    #facet_grid(region~.)+
    theme_linedraw()+
    #scale_fill_brewer('set1')+
    transition_reveal(fecha)+
    #labs( title= "En casa / fecha: {frame_along}")+
    #theme(panel.background = element_rect(fill = "snow", colour = "#6D9EC1",
                                      #size = 2, linetype = "solid"))+
    theme(panel.background = element_rect(fill = 'grey60', colour = 'red'))+
    labs(subtitle = paste('Sector: ',gsub('_',' ',elemento),". desde: ",format(min(df$fecha),'%d-%m-%Y'),
                          ' hasta:',format(max(df$fecha),'%d-%m-%Y')) ,
         caption = caption,
         #tag = "Gráfico 1",
         x = "fecha",
         y = "%",
         colour = "País")+
    #scale_x_date(date_breaks='1 week',labels = date_format('%d-%m-%Y'))+    
    scale_x_date(date_breaks='4 week',labels = date_format('%m-%Y'))+
    scale_color_viridis(discrete = TRUE, option = "D")+
    ggtitle(paste('Cambios en movilidad: ',nombre_paises))+
    theme(plot.title=element_text(size=rel(1.5),
                                  lineheight=.9,family="Times",
                                  face="bold.italic",colour="grey38"))+
    #theme(legend.position = 'bottom')+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_vline(data=df_eventos, mapping=aes(xintercept=a), color="orange",linetype="dotted", size=.5, alpha=.5)}+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_text(data=df_eventos,aes(x=a, label=b , y = posicion_leyenda), colour="black", angle=90, size=1.8)}
  
  animate(animado, height = height_v, width =width_v,fps=5,end_pause = 15)
} #end_pause = 30
#renderer = gifski_renderer(loop = F)
