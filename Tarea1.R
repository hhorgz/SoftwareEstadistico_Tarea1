remove(list = ls())
setwd("D:/Repositorios/softwareEstadistico_Tarea1")
getwd()

library(foreign)
datos_migracion <- read.spss("MIGRACION_BDP.sav", to.data.frame = T)

# INCISO 1
migrantesPorDepartamento <- table(datos_migracion$DEPARTAMENTO, datos_migracion$PEI3)

# INCISO 2
?t
?par
t(migrantesPorDepartamento)
par(mai = c(2,1,1,1))
barplot(
  t(migrantesPorDepartamento),
  main = "Migrantes de Guatemala por departamento - Censo 2018",
  ylab = "Cantidad de migrantes", 
  xlab = "",
  legend.text = TRUE,
  ylim = c(0,50000),
  las = 3,
  cex.names = 0.8
  )
mtext("Departamentos", side = 1, line = 7, las = 1)

# INCISO 3
head(sort(table(datos_migracion$DEPARTAMENTO), decreasing = TRUE), n= 1)

# INCISO 4
# Determine el año en que emigro mayor cantidad de personas, auxilice de las funciones table, apply
class(datos_migracion$PEI5)
str(datos_migracion$PEI5)
head(sort(table(datos_migracion$PEI5), decreasing = TRUE), n = 1)

# INCISO 5
# Genere un grafico de barras por año y numero de persona que emigraron
barplot(
  table(datos_migracion$PEI5, exclude = 9999),
  main = "Migrantes de Guatemala por año - Censo 2018",
  ylab = "Cantidad de migrantes",
  xlab = "Año",
  ylim = c(0, 30000),
  las = 3
)

# INCISO 6
# De que area emigra mas personas rural o urbana, y cual es la cantidad?
sort(table(datos_migracion$AREA), decreasing = TRUE)

# INCISO 7
# Cual es la mediana de edad por sexo de la emigracion
table(datos_migracion$PEI4, datos_migracion$PEI3)
median(table(datos_migracion$PEI4, datos_migracion$PEI3))

datos_migracion$PEI4
subset(datos_migracion$PEI4, datos_migracion$PEI3 == "Hombre")
median(subset(datos_migracion$PEI4, datos_migracion$PEI3 == "Hombre"))
median(subset(datos_migracion$PEI4, datos_migracion$PEI3 == "Mujer"))


########################################################################
#                       VIVIENDA_BDP
########################################################################
datosVivienda <- read.spss("VIVIENDA_BDP.sav", to.data.frame = T)

# INCISO 8
# Determinar el tipo de vivienda y el material de construccion en el municipio de Santa Apolonia, Chimaltenango
View(datosVivienda)
viviendaChimaltenango <- subset(datosVivienda, datosVivienda$DEPARTAMENTO == "Chimaltenango" & datosVivienda$MUNICIPIO == "Santa Apolonia")
class(viviendaChimaltenango)
table(
  viviendaChimaltenango$PCV1, viviendaChimaltenango$PCV2
)
write.csv(
  table(viviendaChimaltenango$PCV1, viviendaChimaltenango$PCV2),
  "TipoVivienda_vs_Material.csv"
)

########################################################################
#                       HOGAR_BDP.sav
########################################################################
datosHogar <- read.spss("HOGAR_BDP.sav", to.data.frame = TRUE)
# INCISO 9
# Determine la proporcion de la variable computadora vrs internet a nivel nacional
View(datosHogar)
# computadora   datosHogar$PCH9_H
# internet      datosHogar$PCH9_I

computadoraVsInternet <- table(datosHogar$PCH9_H, datosHogar$PCH9_I)
computadoraVsInternet
rownames(computadoraVsInternet) = c("Con Computadora", "Sin Computadora")
colnames(computadoraVsInternet) = c("Con Internet", "Sin Internet")
round(100 * computadoraVsInternet / sum(computadoraVsInternet), 1)

# INCISO 10
# Realice un grafico de Pie q muestre la proporcion de la principal fuente para cocinar
fuenteCocinar <- table(datosHogar$PCH14)
fuenteCocinar
etiquetasPorcentaje <- round(100 * fuenteCocinar / sum(fuenteCocinar), 2)
etiquetasPorcentaje
rownames(fuenteCocinar)
pie(
  etiquetasPorcentaje,
  labels = paste(etiquetasPorcentaje, "%"),
  main = "Principal fuente para cocinar a nivel nacional",
  radius = 1,
  cex = 0.8,
  col = rainbow(length(rownames(etiquetasPorcentaje)))
  )
legend(
  "topright", 
  rownames(etiquetasPorcentaje),
  fill = rainbow(length(rownames(etiquetasPorcentaje))),
  cex = 0.8
)

########################################################################
#                       PERSONA_BDP.sav
########################################################################
datosPersona <- read.spss("PERSONA_BDP.sav", to.data.frame = TRUE)
View(datosPersona)

# INCISO 11
# Determine el porcentaje de hombres y mujeres que usaron el telefono en los ultimos 3 meses por area rural y urbana

# ha utilizado celular en los ultimos tres meses = PCP26_A
# sexo de la persona = PCP6
# area = area
utilizacionTelefono <- table(datosPersona$PCP6, datosPersona$AREA, datosPersona$PCP26_A)
utilizacionTelefono[,,1]

########################################################################
#                       Guatemala.txt
########################################################################

# INCISO 12
# Usando el archivo Guatemala.txt, determine los estadisticos por mes para cada una de las estaciones
