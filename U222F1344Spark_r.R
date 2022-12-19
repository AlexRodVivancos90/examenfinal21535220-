
# ¿De donde saco la info? -------------------------------------------------



url_="https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/"


# ¿Que bibliotecas usar? --------------------------------------------------



install.packages("pacman")
library(pacman)
p_load(tidyverse, janitor, jsonlite, leaflet)
data <- fromJSON(url_)


# Veamos los precios con los que trabajaremos -----------------------------


ds_raw <- data$ListaEESSPrecio
data$ListaEESSPrecio

# a -----------------------------------------------------------------------



# i - limpiar Dataset -----------------------------------------------------


ds_f <-ds_raw %>% clean_names() %>% type_convert(locale=locale(decimal_mark = ","))%>% as_tibble()


# ii - Informe de anomalias -----------------------------------------------

# Ver memoria 

# iii ---------------------------------------------------------------------

# Tras haber estudiado el dataset, definimos las gasolineras que no son low cost y añadimos esa columna a dicho dataset


no_low_cost <- c('REPSOL','CEPSA','GALP','SHELL','BP','PETRONOR','AVIA','Q8','CAMPSA','BONAREA')

ds_lowcost <- ds_f %>% mutate(low_cost=!rotulo %in% no_low_cost)

ds_lowcost %>% count(rotulo, low_cost)

#Posteriormente, organizado por CCAA, definimos el precio promedio de todos los combustibles calsificando las gasolineras en low_cost o no_low_cost

ds_precio_promedio_ccaa <-ds_lowcost %>% select(precio_gasoleo_a, precio_gasoleo_b, precio_gasoleo_premium, precio_gasolina_95_e5, precio_gasolina_95_e10, precio_gasolina_95_e5_premium, precio_gasolina_98_e10,idccaa, rotulo) %>% group_by(idccaa) %>% summarise(GASOLEO_a=mean(precio_gasoleo_a,na.rm=TRUE),GASOLINA_95=mean(precio_gasolina_95_e5,na.rm=TRUE), GASOLEO_B=mean(precio_gasoleo_b,na.rm=TRUE), GASOLEO_PREMIUM=mean(precio_gasoleo_premium,na.rm=TRUE), GASOLINA_95e10=mean(precio_gasolina_95_e10,na.rm=TRUE), GASOLINA_95_PREMIUM=mean(precio_gasolina_95_e5_premium,na.rm=TRUE), GASOLINA_98=mean(precio_gasolina_98_e10,na.rm=TRUE)) %>% view()


# iv ----------------------------------------------------------------------

#Primeramente definimos el ranking de las gasolineras mas baratas: Como muestra he usado el precio_gasolina_98_e5 como variable pero bastaría con seleccionar otra cualquiera para reordenar las gasolineras como se quisiera 

mas_baratas= ds_lowcost[ ds_lowcost$low_cost,] 
mas_baratas=mas_baratas[order(mas_baratas$precio_gasolina_98_e5,decreasing = FALSE),]
top_20_baratas = mas_baratas[1:20,]
top_20_baratas %>% leaflet() %>% addTiles() %>% addCircleMarkers(lng = ~longitud_wgs84,lat = ~latitud )


#De la misma manera modificando únicamente el número de gasolineras a incluir e indicando que el orden no debe de ser descendente he obtenido el ranking de gasolineras mas caras

mas_caras= ds_lowcost[ ds_lowcost$low_cost,] 
mas_caras=mas_caras[order(mas_caras$precio_gasolina_98_e5,decreasing = TRUE),]
top_10_caras = mas_caras[1:10,]
top_10_caras %>% leaflet() %>% addTiles() %>% addCircleMarkers(lng = ~longitud_wgs84,lat = ~latitud )



# v -----------------------------------------------------------------------

#Una vez acabado el ejercico de hayar el precio promedio por gasolinera he creado un csv

write_excel_csv(ds_lowcost,"low-cost-222F1344.csv")



# b -----------------------------------------------------------------------



# i y ii ------------------------------------------------------------------


#Primero definimos el número de gasolineras  por comunidad y sacamos la media, max y min de gasóleo A, y gasolina 95 e Premium. 

ds_lowcost_Comunidades <- ds_lowcost %>% count(idccaa, rotulo, low_cost)
ds_promedio_min_max_por_ccaa <- ds_lowcost %>% select(idccaa, precio_gasoleo_a, precio_gasolina_95_e5_premium) %>% group_by(idccaa) %>% summarise_all(funs(mean,max,min),na.rm=TRUE) %>% filter(idccaa == "09" |idccaa == "13")

#Unimos dos tablas : tabla1) Los promedios, minimos, y maximos de las gasolinas filtrado por ccaa: Madrid, Cataluña y tabla2) Cuenta las gasolineras lowcost:

ds_MABN_lowcost <-inner_join(ds_lowcost_Comunidades,ds_promedio_min_max_por_ccaa,by='idccaa')


# iii ---------------------------------------------------------------------


write_excel_csv(ds_MABN_lowcost,"informe_MAD_BCN_U222F1344.xls")


# c -----------------------------------------------------------------------


# i -----------------------------------------------------------------------


ds_lowcost$municipio %>% view()

#Primero hayamos el número de gasolineras por municipio organizados por LowCost o NoLowCost

ds_lowcost_municipio<- ds_lowcost %>% count(id_municipio, rotulo, low_cost)
unique(ds_lowcost$id_municipio) %>% view

# Para poder filtrar y quitar las grandes ciudades análogamente a lo que se hizo con las gasolineras no low cost crearemos una columna con estas ciudades
p<- c("Barcelona","Madrid","Valencia","Sevilla")

#Posterirmente a un ds nuevo añadiremos esta columna y la usaremos para filtrar y quitar aquellas filas que tengan en su municipio a estas ciudades

ds_no_MBVS <-ds_lowcost[!(ds_lowcost$id_municipio %in% p),]

#Creamos un nuevo ds usando la ds anteriormente creada y con ello incorporamos las columnas de los precios de carburantes que se nos solicita y agrupamos los rsultados por municipio

ds_metricas_no_MBVS<- ds_no_MBVS [,c('id_municipio','precio_gasoleo_a','precio_gasolina_95_e5_premium')] %>% group_by(id_municipio) %>% summarise_all(funs(mean,max,min),na.rm=TRUE)

# Por último hacemos un innerjoint de la ds que acabamos de crar y la que nos permitió ver cuantas gasolineras por municipio hay organizados por LowCost o NoLowCost
ds_sinMBvs_Lowcost<-inner_join(ds_lowcost_municipio,ds_metricas_no_MBVS,by='id_municipio') %>% view() %>% view()

 


# ii ----------------------------------------------------------------------

# Creamos un documento Excel con los resultados obtenidos

write_excel_csv(ds_sinMBvs_Lowcost,"informe_no_grandes_ciudades_U222F1344.xls")


# d -----------------------------------------------------------------------


# i -----------------------------------------------------------------------

# Primero veamos qué horarios tienen las gasolineras para poder determinar cual de ellos es el que interesa (en este caso L-D: 24H)

ds_lowcost %>% count(horario, sort = TRUE) %>% View()

#Con esta info procedemos a filtar aquellas gasolineras que cumplan este requisito y nos aseguramos que el campo de horario no aparece desselecionándolo

ds_no_24h <-ds_lowcost %>% filter(horario == 'L-D: 24H') %>% select(!horario) %>% view()


# ii ----------------------------------------------------------------------


write_excel_csv(ds_no_24h,"no_24_horas_U222F1344")



# e -----------------------------------------------------------------------


# i -----------------------------------------------------------------------

# Tras haber obtenido de una fuente fiable la información sobre municipios (INEI), procedemos a introducir el excel descargado en un ds

pob21 <- readxl::read_excel("pobmun21.xlsx", skip = 1)

# Sobre dicho ds creamos uno al que limpiaremos con clean_names y renombramos el campo nombre a municipio
pob_def <-pob21 %>% select(NOMBRE,POB21) %>% clean_names() %>% rename(municipio = nombre)

# tras hacer esto, aprovechamos para juntar este nuevo ds con el que hemos usado durante todo el ejercicio usando como campo de unión la variable municipio
poblacio_final <-inner_join(ds_lowcost,pob_def, by="municipio") %>% view()




