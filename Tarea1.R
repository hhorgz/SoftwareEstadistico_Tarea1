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
