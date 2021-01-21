#codigos para graficar los datos del Google Mobility Report (GMR)

#funcion cargar paquetes
carga_paquetes <- function(){
  library(tidyverse)
  library(dplyr)
  library(gganimate)
  #install.packages('datapasta')
  library(RColorBrewer)
  library(lubridate)
  library(gridExtra)
  library(scales)
  library(plyr)
  library(ggrepel)
  library(rvest)
  library(viridis)
  library(wesanderson)
  library(ggthemes)
  library(unikn) 
  
}
directorios <- list


carga_paquetes()
pal_freiburg_info <- c("#2a6ebb", "#a7c1e3", "#7b2927", "#de3831", "#739600", "#92d400", 
                       "#4d4f53", "#747678", "#b2b4b3", "#d5d6d2", "#e98300", "#efbd47")
names(pal_freiburg_info) <- c("mid-blau", "hell-blau", "dark-red", "hell-red", "mid-green", "hell-green", 
                              "anthrazit", "dark-grey", "mid-grey", "hell-grey", "orange", "gelb")

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#creacion variables
inicio_ven <- as.Date('2020-03-17')
esp <- c('viernes','lunes','sabado','domingo','jueves','martes','miercoles')
meses <- c('abril','febrero','marzo','mayo', 'junio')
sur_america <- c('Argentina','Bolivia', 'Brazil','Chile','Colombia','Ecuador','Paraguay','Peru','Uruguay','Venezuela')
col_names0 <- c('cod_pais','region',
               'sub_reg1','sub_reg2',
               'fecha',
               'tiendas_y_ocio',
               'supermercados_y_farmacias',
               'parques',
               'estaciones_de_transporte_publico',
               'lugares_de_trabajo',
               'zonas_residenciales')

col_names1 <- c('cod_pais','region',
               'sub_reg1','sub_reg2',
               'fecha',
               'tiendas_y_ocio',
               'supermercados_y_farmacias',
               'parques',
               'estaciones_de_transporte_publico',
               'lugares_de_trabajo',
               'zonas_residenciales')

dias_laborales <- c('viernes','lunes','jueves','martes','miercoles')

#descargar datos
#download.file('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=57b4ac4fc40528e2','Global_Mobility_Report0.csv')
#download.file ('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=d09f7f6f428c6783','Global_Mobility_Report1.csv')
#download.file('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=b3ad67084527db32','Global_Mobility_Report2.csv')
#download.file('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=b8b0c30cbee5f341','Global_Mobility_Report3.csv')
#download.file('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=c050b74b9ee831a7','Global_Mobility_Report4.csv')
#download.file('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=31928890d4c9fde9','Global_Mobility_Report5.csv')
archivo <- 'https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv'
descarga_GMR <- function(){
  download.file('https://www.google.com/covid19/mobility/', 'fecha_nueva_html.html', method='curl')
  fecha_nueva <- read_html('fecha_nueva_html.html')%>%
    html_node('.report-info-text:nth-child(1)')%>%
    html_text()%>%
    substr(.,nchar(.)-10,nchar(.)-1)
  if (fecha_nueva!=fecha_vieja){
    download.file(archivo,paste0('reports/GMR_',fecha_nueva,'.csv'), method='curl')
  #fecha_vieja <- fecha_nueva
  return(fecha_nueva)
  }
  return(fecha_nueva)
}

fecha_vieja <- descarga_GMR() 

 # download.file('https://www.google.com/covid19/mobility/', 'fecha_nueva_html.html')
# fecha_nueva <- read_html('fecha_nueva_html.html')%>%
#   html_node('.report-info-text:nth-child(1)')%>%
#   html_text()%>%
#   substr(.,nchar(.)-10,nchar(.)-1)
# fecha_nueva
# 
# archivo <- 'https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv'


# numero_descarga <- 6
#download.file(archivo,paste0('reports/GMR_',fecha_nueva,'.csv'), method = 'curl')
# https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv

#cargar datos y transformarlos
global <- read.csv(paste0('reports/GMR_',fecha_nueva,'.csv'),stringsAsFactors=FALSE)%>%
  `colnames<-`(col_names)%>%
  mutate(fecha= as.Date(fecha))%>%
  mutate(sub_reg1,as.character(global$sub_reg1))%>%
  mutate(sub_reg2,as.character(global$sub_reg2))%>%
  mutate(region,as.character(global$region))%>%
  mutate(dia_semana= mapvalues( weekdays(fecha),levels(as.factor(weekdays(fecha))), esp))%>%
  mutate(dia = day(fecha))%>%
  mutate(n_mes=month(fecha))%>%
  mutate(mes=as.factor(mapvalues(months(fecha),levels(as.factor(months(fecha))),meses[1:length(levels(as.factor(months(fecha))))])))%>%
  mutate(region=as.factor(as.character(region)))%>%
  mutate(fecha= as.Date(fecha,"%d/%m/%Y"))%>%
  select(-c('metro_area',"iso_3166_2_code","census_fips_code" ))%>%
  mutate(region=as.character(region))%>%
  mutate(mes=as.character(mes))

  

  


listado_paises <- levels(global$region)

#variables leyenda grafico
caption=paste("datos obtenidos de www.google.com/covid19/mobility/ procesados @javendaXtw. Actualizados al",max(global$fecha))

#funcion de ayuda generacion graficos
global_filtros <- function (paises, 
                            sub_nac=FALSE,
                            post=FALSE,
                            weekend=FALSE,
                            inicio=FALSE,
                            fin=FALSE){
  #fechas por defecto
  if(inicio!=FALSE & fin==FALSE){fin=max(global$fecha)}
  if(fin!=FALSE & inicio==FALSE){inicio=min(global$fecha)}
  
  filter(global,region %in% paises)->global
  global$region <- as.character(global$region)
  
  
  if(sub_nac!=FALSE){
    filter(global, sub_reg1 %in% sub_nac) ->global
  }else{
    filter (global, sub_reg1=='') -> global
  }
  if(post==TRUE){
    filter(global, fecha>=inicio_ven) -> global
  }
  if(weekend==TRUE){
    filter(global, dia_semana %in% dias_laborales)-> global
  }
  if (inicio!= FALSE){
    filter(global, fecha>=inicio&fecha<=fin)-> global
  }
  return(global)
}

#funcion listado sub_regiones_1
levels_sub_reg1 <- function(region_){
  global%>%
    filter(region==region_) -> seleccion
  print(unique(seleccion$sub_reg1))
}

#listado de datos a graficar
tipo_elemento <- names(global)[6:11]


#parametros generacion de graficos
#paises = vector con paises a seleccionar. listado en 'listado paises'
# sub_nac=FALSE vector con niveles sub_nacionales. Consultar listado con 'levels_sub_reg1('nombre_pais)'
# post=FALSE filtrar solo fechas posteriores a inicio_ven
# weekend=FALSE excluir fechas de fines de semana
# inicio=FALSE seleccion intervalo fecha inicio formato ('Y-m-d')
# fin=FALSE)seleccion intervalo fecha fin formato ('Y-m-d')

#funcion ayuda dos filtro
global_filtros_dos <- function(paises, 
                               sub_nac=FALSE,
                               post=FALSE,
                               weekend=FALSE,
                               inicio=FALSE,
                               fin=FALSE){
  #fechas por defecto
  if(inicio!=FALSE & fin==FALSE){fin=max(global$fecha)}
  if(fin!=FALSE & inicio==FALSE){inicio=min(global$fecha)}
  
  
  filter(global,region %in% paises )->global
  global$region <- as.character(global$region)
  global_tmp <- global
  global_tmp <- filter(global_tmp,sub_reg1=="")
  
  if(sub_nac!=FALSE){
    filter(global, sub_reg1 %in% sub_nac) ->global
  }else{
    filter (global, sub_reg1=='') -> global
  }
  global <- rbind(global_tmp,global)
  if(post==TRUE){
    filter(global, fecha>=inicio_ven) -> global
  }
  if(weekend==TRUE){
    filter(global, dia_semana %in% dias_laborales)-> global
  }
  if (inicio!= FALSE){
    filter(global, fecha>=inicio&fecha<=fin)-> global
  }
  
  return(global)
}

#funcion filtrar pais
df_pais <- function(pais){
  global%>%
    filter(region==pais)-> temp
  return(temp)
}

########data frame con eventos a destacar
df_eventos <- data.frame(
  a=as.Date(c('2020-04-09','2020-04-12','2020-05-01')),
  b=c('inicio Semana Santa','fin de Semana Santa','Día del trabajador'),
  stringsAsFactors = F)

latan <- global%>%
  filter(region %in% c('Argentina', 'Colombia','Venezuela','Paraguay', 'Bolivia','Brazil','Uruguay', 'Chile','Ecuador','Peru')&
           sub_reg1==''&
           sub_reg2=='')%>%
  mutate(region=as.character(region))%>%
  mutate(mes=as.character(mes))

saveRDS(latan,paste0('latan_',fecha_nueva,'.rds'))

