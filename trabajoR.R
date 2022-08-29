#TRABAJO PRACTICO DE R
#Integrantes: Jorge Vassallo, Carla Domecq, Clemente Suarez
install.packages("tidyverse")
install.packages("data.table")
install.packages("lubridate")
library(tidyverse)
library(lubridate)
library(data.table)


#Leemos los datos a utilizar
datos = read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/atencion-ciudadana/sistema-unico-atencion-ciudadana/sistema-unico-de-atencion-ciudadana-2021.csv", sep = ";", header = TRUE)
head(datos)
sapply(datos, class)

#Creamos columna de mes para luego poder graficarlo
datos = mutate(datos,
               mes = substr(periodo, 5,6))

#Transformamos el tipo de dato a numeric para poder aplicar transformaciones y obtener el nombre del mes.
datos = transform(datos,
                  mes = as.numeric(mes))
#ordenamos el dataframe por mes
datos = arrange(datos, mes)

datos = datos %>% 
mutate(mes = as.character(lubridate::month(mes, label = TRUE, abbr = FALSE)))
#Ponemos los nombres con la primera letra en mayuscula
datos = mutate(datos, mes = gsub("\\b([a-z])", "\\U\\1", mes, perl=TRUE))

#funcion para estimar el porcentaje de valores NA y valores vacios por columna para estimaciones de dropeo de columnas
analisis_faltantes = function(df, ArrayofColumns = names(df)){
  # se selecciona las columnas
  datos_seleccion = df[, ArrayofColumns]
  
  # calculo de los porcentajes y metricas
  porcentaje_na = colMeans(is.na(datos_seleccion))*100
  tipo_dato = lapply(datos_seleccion, class)
  porcentaje_vacios = lapply(datos_seleccion, function(m) sum(as.character(m) == "", na.rm = TRUE) / nrow(datos_seleccion)*100)
  
  porcentajes = data.table(columna = names(datos_seleccion), 
                       `%NA` = porcentaje_na, 
                       `%Vacio` = (porcentaje_vacios),
                       Tipodedato = tipo_dato)
  porcentajes
  
}

analisis_faltantes(datos, c("contacto", "periodo", "categoria", "subcategoria", "prestacion", "tipo_prestacion", "fecha_ingreso", "hora_ingreso", "domicilio_comuna", "domicilio_barrio", "domiclio_calle", "domicilio_altura", "domicilio_esquina_proxima", "lat", "lon", "canal", "genero", "estado_del_contacto", "fecha_cierre_contacto", "domicilio_calle", "mes"))


###cantidad de contactos registrados en 2021
nrow(datos)

###Cantidad de contactos cuyo estado_del_contacto haya quedado registrado como “Abierto”
contacto_abierto = datos %>%
  filter(estado_del_contacto == "Abierto")
#RESULTADO FINAL
nrow(contacto_abierto)
#RESULTADO FINAL 2
contacto_abierto_conteo = count(contacto_abierto, estado_del_contacto) %>%
  rename(total_contactos = n)

#Aca solamente hice un dataframe con los contactos cerrados para verificar que el numero total de registro concuerda
contacto_cerrado = datos %>%
  filter(estado_del_contacto == "Cerrado")
nrow(contacto_cerrado)#RESULTADO FINAL

###Cantidad de contactos por genero cuyo valor sea distinto de nulo y de vacío.

#vemos los valores distintos en esta columna
distinct(datos, genero)

#creo el dataframe con los valores que no son nulos o vacios
contactos_genero_sin_nulos = datos %>%
  filter((genero == "Femenino") | (genero == "Masculino") | (genero == "Otros"))

#creo un dataframe con los datos vacios/nulos y los agrupo para su conteo
genero_nulo = datos %>%
  filter(genero == "") 
#creamos esto para luego comprobar con el total de filas 
group_by(genero_nulo, genero) %>%
  summarise(conteo_genero=n())

#conteo y agrupacion de datos por genero
#RESULTADO FINAL
genero_sin_nulos = group_by(contactos_genero_sin_nulos, genero) %>%
  summarise(conteo_genero=n())

###Cantidad de contactos por tipo_prestacion y categoria

#conteo con valores nulos
datos = datos %>% 
  mutate(tipo_prestacion = toupper(tipo_prestacion))#pongo en mayuscula todas las celdas ya que hay datos que no siguen este formato
         
grouped = group_by(datos, tipo_prestacion, categoria)

#RESULTADO 
prest_cat_agrupado = count(grouped, tipo_prestacion, categoria) %>%
  rename(total_contactos = n)

#conteo sin valores nulos 

#caso 1, se borra solo una fila que tenga nulos en ambas columnas, pero sigue conteniendo campos vacios
datos_limpios = datos[!(is.na(datos$tipo_prestacion) | datos$tipo_prestacion==""), ]

agrupacion_limpia = group_by(datos_limpios, tipo_prestacion, categoria)

limpio_prest_cat_agrupado = count(agrupacion_limpia, tipo_prestacion, categoria) %>%
  rename(total_contactos = n)

#caso 2 se borran todas las filas que tengan nulos o campos vacios 
datos_limpios = datos[!(is.na(datos$categoria) | datos$categoria==""), ]

agrupacion_limpia = group_by(datos_limpios, tipo_prestacion, categoria)
#RESULTADO 
limpio_prest_cat_agrupado = count(agrupacion_limpia, tipo_prestacion, categoria) %>%
  rename(total_contactos = n)

###Cantidad de contactos cuyo tipo_prestacion sea “SOLICITUD”, categoria sea “ALUMBRADO” y subcategoria sea “REPARACIÓN DE LUMINARIA”, por domicilio_barrio en el mes de Junio de 2021.

#filtramos los datos por los datos requeridos
df_filtrado = datos %>%
  filter((tipo_prestacion == "SOLICITUD") & (categoria == "ALUMBRADO") & (subcategoria == "REPARACIÓN DE LUMINARIA") & (mes == "Junio"))

#los agrupamos por domicilio_barrio
agrupacion2 = group_by(df_filtrado, domicilio_barrio)

#realizamos el conteo
#RESULTADO 
conteo_agrupacion2 = count(agrupacion2, tipo_prestacion, categoria, subcategoria, mes) %>%
  rename(total_contactos = n)

###Mes con mayor cantidad de contactos cuyo tipo_prestacion sea “DENUNCIA”.
df_denuncia = datos %>%
  filter(tipo_prestacion == "DENUNCIA")

denuncia_agrupada = group_by(df_denuncia, mes)

maximo_mes_denuncia = count(denuncia_agrupada, tipo_prestacion, mes) %>%
  rename(total_contactos = n)

denuncia_ordenada = arrange(maximo_mes_denuncia, desc(total_contactos))#ordenamos para luego quedarnos solo con la primera fila
#RESULTADO 
maxima_denuncia = head(denuncia_ordenada, 1)

#Valor de la Media, Mediana, Varianza y Desviación estándar correspondientes a la cantidad de contactos por mes.

#creo el dataframe con los contactos por mes
agrup_mes = group_by(datos, mes)
contactos_mes = count(agrup_mes) %>%
  rename(total_contactos = n)

###Calculo de media, mediana, desviacion estandar y varianza
mean(contactos_mes$total_contactos, na.rm = TRUE)#media
median(contactos_mes$total_contactos, na.rm = TRUE)#mediana
var(contactos_mes$total_contactos, na.rm = TRUE)#varianza
sd(contactos_mes$total_contactos, na.rm = TRUE)#desviacion estandar

###Gráfico de barras (con librería ggplot2) correspondiente a la cantidad de contactos por mes
library(ggplot2)

#creamos un factor ordenado para que ggplot grafique los meses en el orden correcto.
contactos_mes$mes = factor(contactos_mes$mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))

#para evitar notacion cientifica
options(scipen=999)

#GRAFICO
ggplot(contactos_mes, aes(x = mes, y = total_contactos)) + 
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  ggtitle("Cantidad de contactos totales por mes") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Meses") + ylab("Cant. contactos") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold")
  )


###Gráfico pastel (con librería ggplot2) correspondiente a la cantidad de contactos por mes.

ggplot(contactos_mes, aes(x = "", y = total_contactos, fill = mes)) + 
  geom_bar(stat="identity", color = "black") + ggtitle("Cantidad de contactos totales por mes") + theme(plot.title = element_text(hjust = 0.5)) + xlab("") + ylab("") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic")
  ) + coord_polar("y", start=0) +
  geom_text(aes(label = total_contactos), position = position_stack(vjust = 0.5)) + guides(fill=guide_legend(title="Mes"))

###Gráfico de barras horizontal (con librería ggplot2) correspondiente a la cantidad de contactos por domicilio_barrio, apilado por tipo_prestacion, cuya vía de contacto (canal) sea “App”.

#chequeamos los valores distintos para ver cuantas "App" hay
distinct(datos, canal)

#filtramos por columna canal los registros que empiecen con "App"
filtro_app = filter(datos, grepl("^App*", canal))

#Conteo con valores nulos en domicilio_barrio
agrup_barrio_prestacion = group_by(filtro_app, domicilio_barrio, tipo_prestacion, canal)

contactos_app = count(agrup_barrio_prestacion) %>%
  rename(total_contactos = n)

#Conteo sin valores nulos en domicilio_barrio
limpio_barrio_prestacion = agrup_barrio_prestacion[!(is.na(agrup_barrio_prestacion$domicilio_barrio) | agrup_barrio_prestacion$domicilio_barrio==""), ]

contactos_app_sin_nulos = count(limpio_barrio_prestacion) %>%
  rename(total_contactos = n)

#GRAFICO
ggplot(contactos_app_sin_nulos, aes(fill=tipo_prestacion, y=domicilio_barrio, x=total_contactos)) + 
  geom_bar(position="stack", stat="identity") + 
  ggtitle("Cantidad de contactos por barrio y tipo de prestacion") + xlab("Cant. contactos") + ylab("Barrio") +
  theme(plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=12, face="bold"),
    axis.title.y = element_text(color="black", size=12, face="bold")
  ) + guides(fill=guide_legend(title="Tipo de prestacion"))  

###Graficos en mapa estilo Heatmap
install.packages("sf")
library("sf")

#Leemos datos GeoJson
barrios_caba = st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/barrios/barrios.geojson")

#Creamos un dataframe de cantidad de contactos por barrio
agrup_contactos_barrio = group_by(datos, domicilio_barrio)

#Sacamos valores nulos o vacios
limpio_contactos_barrio = agrup_contactos_barrio[!(is.na(agrup_contactos_barrio$domicilio_barrio) | agrup_contactos_barrio$domicilio_barrio==""), ]

#Dataframe limpio
contactos_por_barrio = count(limpio_contactos_barrio) %>%
  rename(total_contactos = n)

#Ordenamos la columna de barrio en el dataframe con datos geograficos para que coincidan con los valores de contactos_por_barrio
barrios_caba = arrange(barrios_caba, BARRIO)

#instalamos las librerias necesarias para graficar correctamente
install.packages("tidycensus")
install.packages("scales")
library(tidycensus)
library(scales)

#GRAFICO
ggplot(barrios_caba) + 
  geom_sf(aes(fill = contactos_por_barrio$total_contactos)) +
  geom_sf(data = barrios_caba, fill = NA) +
  geom_sf_label(aes(label = BARRIO), size = 2.5) +
  scale_fill_viridis_c(name = "Total estimada de contactos") +
  labs(title = "Cantidad de contactos por barrio en 2021",
       subtitle = "Fuente: Sistema unico de atencion ciudadana (CABA)") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"))
  
###Mapa interactivo donde los datos salen cuando se hace click en el barrio

install.packages("leaflet")
library(leaflet)

#creamos paleta de colores en base al total de contactos
caba_state_pop = colorNumeric("viridis", contactos_por_barrio$total_contactos)

#creamos las etiquetas interactivas en base al barrio y cantidad de contactos correspondiente
etiqueta_caba = function(barrio, contacto){
  str_glue("{barrio} con una cantidad de contactos de {contacto}")
}

#GRAFICO
barrios_caba %>%
  leaflet() %>%
  addPolygons(weight = 1,
              color = "white",
              fillColor = ~caba_state_pop(contactos_por_barrio$total_contactos),
              fillOpacity = 0.9,
              popup = ~etiqueta_caba(BARRIO, contactos_por_barrio$total_contactos),
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1, weight = 2, sendToBack = FALSE, color = "black")) %>%
  addLegend(pal = caba_state_pop,
            values = ~contactos_por_barrio$total_contactos,
            opacity = 1,
            title = "Total de contactos") 
  
