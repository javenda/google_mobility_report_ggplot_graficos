#ejecutar al inicio esta dos lineas de codigo para cargar informacion
source('01_funciones_descarga.R')
source('02_funciones_generacion_graficos.R')
###############parametros generacion de graficos
#paises = vector con paises a seleccionar. listado en 'listado paises'
# sub_nac=FALSE vector con niveles sub_nacionales. Consultar listado con 'levels_sub_reg1('nombre_pais)'
# post=FALSE filtrar solo fechas posteriores a inicio_ven
# weekend=FALSE excluir fechas de fines de semana
# inicio=FALSE seleccion intervalo fecha inicio formato ('Y-m-d')
# fin=FALSE)seleccion intervalo fecha fin formato ('Y-m-d')
# elemento= tipo de dato del GMR q se grafica. lista en 'tipo_datos'
# hitos= muestra barra vertical con fechas especiales
#OPCIONES:
#'lugares_de_trabajo'
#'supermercados_y_farmacias'
#'parques'
#'zonas_residenciales'
#'estaciones_de_transporte_publico'
#'tiendas_y_ocio'



#1) grafico barras verticales. filtra pais y elemento
gg_barra(c('Venezuela'),
         sub_nac=FALSE,
         #post=TRUE, 
         weekend=TRUE,
         inicio='2020-03-15',
         #fin='2020-05-27',
         elemento='lugares_de_trabajo',
         hitos=TRUE)

#2) comparacion lineas entre dos elementos de un pais
gg_lineas(c('Venezuela'),
          sub_nac=FALSE,
          post=FALSE, 
          weekend=TRUE, 
          inicio='2020-04-01',
          #fin='2020-05-07',
          elemento=c('supermercados_y_farmacias',
                     'parques'),
          hitos=FALSE)

#3)lineas verticales comparacion entre paises
gg_comp_paises(paises=c('Argentina','Venezuela', 'Colombia'),
               sub_nac=FALSE,
               post=FALSE, 
               weekend=TRUE, 
               inicio='2020-05-27',
               #fin='2020-05-14',
               elemento='lugares_de_trabajo',
               hitos=TRUE)

#4) comparacion paises en un mismo plano facet_grid
gg_comp_paises_mismo_plano(c('Ecuador','Colombia','Venezuela','Peru'),
                           sub_nac=FALSE,
                           post=FALSE, 
                           weekend=TRUE, 
                           inicio='2020-05-01',
                           #fin='2020-05-10',
                           elemento='lugares_de_trabajo',
                           hitos=TRUE)
#5) comparar regiones de una pais con el pais
gg_comp_regiones('Colombia',
                 sub_nac=c('North Santander','Tolima'),
                 post=FALSE,
                 weekend=FALSE,
                 inicio='2020-02-28',
                 fin=FALSE,
                 elemento='zonas_residenciales',
                 hitos=TRUE)
#se puede recurrir a :
levels_sub_reg1('Colombia')
#para listar las sub_reg1 del pais

#6) lineas verticales regiones del pais sin el pais
gg_comp_regiones_pais('Colombia',
                      sub_nac=c('North Santander','Tolima'),
                      elemento='supermercados_y_farmacias',
                      hitos=TRUE) 


#7) grafica animada
gg_animado_paises(c('Venezuela','Argentina'),
                  sub_nac=FALSE,
                  weekend=TRUE, 
                  post=TRUE, 
                  inicio='2020-03-15',
                  fin=FALSE,
                  elemento='zonas_residenciales',
                  height_v = 400,#tamano vertical
                  width_v = 800,#tamano horizontal
                  hitos=FALSE)

##################################################
#ejemplo multiplot
p1 <- gg_comp_regiones_pais('Colombia',
                            sub_nac=c('North Santander','Tolima'),
                            elemento='supermercados_y_farmacias',
                            hitos=TRUE) 8
p2 <- gg_comp_paises(paises=c('Argentina','United States','Venezuela'),
                     sub_nac=FALSE,
                     post=FALSE, 
                     weekend=TRUE, 
                     #inicio='2020-04-01',
                     fin='2020-05-14',
                     elemento='parques',
                     hitos=TRUE)

multiplot(p1,p2)


#############funciones y listados de ayuda
#1)crear una df con datos filtrados por pais
df_pais('Venezuela') #argumento nombre del pais

#listado de 'elementos' que se pueden seleccionar
tipo_elemento

#listado de sub_reg_1 para un pais. 
#Argumento nombre pais
levels_sub_reg1('Argentina')



