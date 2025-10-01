#------------------------------------------------------------------------------#
# Modelos VAR aplicados a la macroeconomía y la historia económica: 
# fundamentos y aplicaciones empíricas

# Escuela de Verano: Repensando el desarrollo
# Ciudad de México. Octubre 2025

# Tutora: Ana Laura Catelén

#------------------------------------------------------------------------------#

#Librerías
library(readxl)
library(tsibble)
library(dplyr)
library(ggplot2)
library(urca)
library(purrr)


#Preparación del entorno de trabajo
rm(list = ls()) #Limpia el entorno del trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Establecer el directorio de trabajo


# 1. Introducción a Series de tiempo ----

## Procesos básicos de series temporales --
## Ruido blanco
ruido <- rnorm(500, mean=0, sd=1)           # genera 500 observaciones de ruido blanco (media 0, desvío estándar = 1)
n <- 1:500
ruido_blanco <- data.frame(n, ruido)
plot(ruido_blanco$n, ruido_blanco$ruido, type = "l")   # grafica la serie como línea

## Ruido blanco + tendencia determinística
ruido_blanco$deterministico <- 0.5 + 0.02 * ruido_blanco$n + ruido_blanco$ruido
plot(ruido_blanco$n, ruido_blanco$deterministico , type = "l")   

## Ruido blanco + tendencia estocástica
ruido_blanco$estocastico[1] <- 0
for(i in 2:500){
  ruido_blanco$estocastico[i] <- rnorm(1, mean=0, sd=1) + ruido_blanco$estocastico[i-1]
}
plot(ruido_blanco$n, ruido_blanco$estocastico , type = "l")   

# 2. Importación de datos  -------------------------------------------

rm(list = ls()) #Limpia el entorno del trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Establecer el directorio de trabajo

PS1 <- read_excel("C:/Users/anaca/Dropbox/Investigación/Mexico 2025 - Escuela de verano/Scripts/PS 1.xlsx")
View(PS1)

# 3. Formato series de tiempo y gráficos --------------------------------------

# Transformar el data frame en tsibble
PS1_ts <- PS1 %>%
  mutate(Año = as.integer(Año)) %>%   # asegurar que Año es numérico
  as_tsibble(index = Año)

# Para graficar, primero creamos un vector de variables de interés
vars <- names(PS1_ts)[names(PS1_ts) != "Año"]

# Loop para graficar cada variable
for (v in vars) {
  p <- ggplot(PS1_ts, aes(x = Año, y = .data[[v]])) +
    geom_line() +
    labs(title = paste("Serie temporal de", v),
         x = "Año", y = v)
  
  print(p)   # muestra el gráfico en la consola/gráficos de RStudio
}


# 4. Logaritmos --------------------------------------------
PS1_ts <- PS1_ts %>%
  mutate(
    lPIB = log(PIB),
    lSalarios = log(Salarios_reales),
    lTI = log(TI),
    lExt_debt = log(Ext_debt),
    lTCR = log(TCR),
    lRRII = log(RRII)
  )


# 5. Tests de raíces unitarias --------------------------------------------

# Lista de variables a testear
vars <- c("lPIB", "lSalarios", "Crec_socios", "lTI", "BC", "lExt_debt", "lTCR", "lRRII")

# Aplicar el test ADF a cada variable
adf_results <- map(vars, ~ {
  serie <- PS1_ts[[.x]]
  test <- ur.df(y = serie, type = "none", lags = 2, selectlags = "Fixed")
  list(variable = .x, resultado = summary(test))
})
# Mostrar resultados
for (res in adf_results) {
  cat("\n==============================\n")
  cat("Variable:", res$variable, "\n")
  print(res$resultado)
}

## Otros tests de raíces unitarias

# Aplicar el test Elliott, Rothenberg & Stock a cada variable
ers_results <- map(vars, ~ {
  serie <- PS1_ts[[.x]]
  test <- ur.ers(y = serie, type = "DF-GLS", model = "const", lag.max = 4)
  list(variable = .x, resultado = summary(test))
})
# Mostrar resultados
for (res in ers_results) {
  cat("\n==============================\n")
  cat("Variable:", res$variable, "\n")
  print(res$resultado)
}

# Aplicar Kwiatkowski–Phillips–Schmidt–Shin test (KPSS) a cada variable
# Nota: A diferencia de ADF o DF-GLS, en el test KPSS la hipótesis nula (H0) 
# es que la serie ES estacionaria. Rechazar H0 implica no estacionariedad.
kpss_results <- map(vars, ~ {
  serie <- PS1_ts[[.x]]
  test <- ur.kpss(y = serie, type = "tau", lags = "short")
  list(variable = .x, resultado = summary(test))
})
# Mostrar resultados
for (res in kpss_results) {
  cat("\n==============================\n")
  cat("Variable:", res$variable, "\n")
  print(res$resultado)
}

# Aplicar Phillips-Perron a cada variable
pp_results <- map(vars, ~ {
  serie <- PS1_ts[[.x]]
  test <- ur.pp(serie, type = "Z-tau", model = "trend", lags = "short")
  list(variable = .x, resultado = summary(test))
})
# Mostrar resultados
for (res in pp_results) {
  cat("\n==============================\n")
  cat("Variable:", res$variable, "\n")
  print(res$resultado)
}

# Aplicar Schmidt & Phillips a cada variable
sp_results <- map(vars, ~ {
  serie <- PS1_ts[[.x]]
  test <- ur.sp(y = serie, type = "tau", pol.deg = 1, signif = 0.01)
  list(variable = .x, resultado = summary(test))
})
# Mostrar resultados
for (res in sp_results) {
  cat("\n==============================\n")
  cat("Variable:", res$variable, "\n")
  print(res$resultado)
}

# Aplicar Zivot & Andrews a cada variable
za_results <- map(vars, ~ {
  serie <- PS1_ts[[.x]]
  test <- ur.za(y = serie, model = "both", lag = 2)
  list(variable = .x, resultado = summary(test))
})
# Mostrar resultados
for (res in za_results) {
  cat("\n==============================\n")
  cat("Variable:", res$variable, "\n")
  print(res$resultado)
}


# 6. Primeras diferencias sobre las no-estacionarias  --------

PS1_ts <- PS1_ts %>%
  mutate(
    d_lPIB       = c(NA, diff(lPIB)),       # diferencia de log PIB
    d_lSalarios  = c(NA, diff(lSalarios)),  # diferencia de log salarios
    d_lTI        = c(NA, diff(lTI)),        # diferencia de log tasa interés
    d_lExt_debt  = c(NA, diff(lExt_debt)),  # diferencia de log deuda externa
    d_lTCR       = c(NA, diff(lTCR)),       # diferencia de log TCR
    d_lRRII      = c(NA, diff(lRRII))       # diferencia de log reservas
  )
