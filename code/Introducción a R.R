#------------------------------------------------------------------------------#
# Modelos VAR aplicados a la macroeconomía y la historia económica: 
# fundamentos y aplicaciones empíricas

# Escuela de Verano: Repensando el desarrollo
# Ciudad de México. Octubre 2025

# Tutora: Ana Laura Catelén

#------------------------------------------------------------------------------#

# Introducción a R ----

# Algunos ejemplos

# 1. Trabajo con archivos
# Establecer el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #ajusta automáticamente el directorio de trabajo a la carpeta donde está guardado tu script en RStudio

# 2. Variables y tipos de datos
# R usa <- para asignar valores
# También se puede usar "="
x <- 5            # Variable numérica
y <- "Hola"       # Variable de texto (carácter)
z <- TRUE         # Variable lógica (VERDADERO/FALSO)

# 3. Operaciones básicas
a <- x + 3        # Suma
b <- x * 2        # Multiplicación
c <- paste(y, "Mundo")  # Concatenación de cadenas de texto
d <- !z           # Negación lógica

# 4. Vectores
# Crear vectores numéricos
vector_numerico <- c(1, 2, 3, 4, 5)
# Crear vectores de texto
vector_texto <- c("manzana", "banana", "cereza")

# 5. Indexación y selección
primer_elemento <- vector_numerico[1]
subconjunto <- vector_numerico[2:4]

# 6. Data Frames
# Crear un data frame
df <- data.frame(Nombre=c("Alicia", "Roberto", "Carlos"),
                 Edad=c(25, 30, 22))

# Acceder a columnas de un data frame
edades <- df$Edad
nombres <- df$Nombre

# 7. Funciones y librerías
# Cargar una librería
library(ggplot2)

# Usar una función de una librería
datos <- c(1, 2, 3, 4, 5)
hist(datos)

# 8. Estructuras de control
# Sentencia if-else
if (x > 3) {
  message("x es mayor que 3")
} else {
  message("x no es mayor que 3")
}

# Bucle para
for (i in 1:5) {
  print(i)
}

# 9. Análisis de datos
# Cargar un conjunto de datos incluido en R
data(mtcars)

# Estadísticas descriptivas
head(mtcars) # Muestra las primeras filas
summary(mtcars) # Resumen estadístico
str(mtcars)  # Estructura de las variables

# 10. Visualización de datos
# Crear un diagrama de dispersión
plot(mtcars$mpg, mtcars$hp, main="Diagrama de dispersión", 
     xlab="Millas por galón", ylab="Caballos de fuerza")

# 11. Exportar resultados
# Guardar un gráfico como imagen
png("scatter_plot.png")
plot(mtcars$mpg, mtcars$hp, main="Diagrama de dispersión", 
     xlab="Millas por galón", ylab="Caballos de fuerza")
dev.off()

