# google_mobility_report_ggplot_graficos
funciones para genear gráficos con ggplot2 sobre el reporte de movilidad (Google Mobility Report) con motivo del COVID 19

desde el script 'ejemplos_graficos' se cargan los paquetes y funciones necesarias. Igualmente contiene ejemplos para la generación de los gráficos.

el script procederá a crear el directorio donde se decaragará el "Google Mobility Report (GMR)".
Cada vez que se ejecute el script se procederá a verificar si hay una nueva versión publicada y se descargará automáticamente actualizando la data frame que contiene el conjunto de datos.

El archivo rds que contiene el GMR no se sobreescribe. Por ser acumulativo el reporte queda a opción del usuario eliminar las descargas anteriores ya que para la fecha de elaboración de este repositorio el GMR tiene un peso superior a los 250 mb.
