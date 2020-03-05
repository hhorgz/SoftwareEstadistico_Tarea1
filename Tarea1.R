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
  main = "Hombres y mujeres que han emigrado por departamento",
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
