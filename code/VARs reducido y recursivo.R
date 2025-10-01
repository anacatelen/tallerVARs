#------------------------------------------------------------------------------#
# Modelos VAR aplicados a la macroeconomía y la historia económica: 
# fundamentos y aplicaciones empíricas

# Escuela de Verano: Repensando el desarrollo
# Ciudad de México. Octubre 2025

# Tutora: Ana Laura Catelén

#------------------------------------------------------------------------------#

#Librerías
library(vars)
library(dplyr)
library(tidyr)
library(ggplot2)

#Preparación del entorno de trabajo
rm(list = ls()) #Limpia el entorno del trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Establecer el directorio de trabajo

source("Introducción a ST.R") #Llamamos al código anterior

# 1. VAR reducido ----

# Armo el objeto Y con todas las variables del VAR
Y <- as.matrix(PS1_ts[, c("Crec_socios", "d_lTI", "d_lPIB", "BC", "d_lExt_debt", "d_lSalarios")])
Y <- Y[complete.cases(Y), ]  # saco las filas con NA

# chequeo que no esté "Año"
colnames(Y)        # lo paso a matriz (formato que pide el VAR)

# Selección del orden de rezagos ----
pmax <- 4   # número máximo de rezagos a evaluar (datos anuales → suele bastar 1 o 2)

popt <- VARselect(Y, lag.max = pmax, type = "const")  
# VARselect (del paquete vars) compara distintos criterios de info
# Evalúa todos los VAR desde 1 hasta pmax rezagos
# type = "const" → incluye un intercepto en cada ecuación

popt   # muestra los resultados de AIC, BIC, HQ y FPE

p <- popt$selection["HQ(n)"] #extraigo el número de rezagos sugerido por el criterio de Hannan-Quinn (HQ)


# Estimación del VAR reducido ---------------------------------------------

VAR_red <- VAR(Y, p = 2, type = "const")   
# estimo el VAR reducido con los datos de Y, usando p rezagos y con constante

summary(VAR_red)  
# muestro el resumen con coeficientes y tests básicos

m <- VAR_red$K     # cantidad de variables en el VAR
m
T <- VAR_red$obs   # número de observaciones efectivas (después de perder p al inicio)
T

plot(VAR_red)  # grafica las series ajustadas vs. observadas y los residuos (sirve para diagnóstico inicial)    


# Gráfico manual de residuos del VAR ----
e <- resid(VAR_red)                                # saco los residuos del VAR estimado
e <- ts(e, start = min(PS1_ts$Año, na.rm = TRUE),  # los paso a serie de tiempo
        end   = max(PS1_ts$Año, na.rm = TRUE),
        frequency = 1)                             # frecuencia anual

colnames(e) <- paste("e.", colnames(Y), sep = "")  # les pongo nombre e.var
plot(e, main = "Residuos del VAR reducido")        # grafico todos los residuos


# Test de causalidad de Granger -------------------------------------------

# Tomamos  los nombres de las variables desde Y
vars <- colnames(Y)

# Lista para guardar resultados
GC_pairwise <- list()

# Loop sobre todas las combinaciones de pares (X causa Y)
for (dep in vars) {
  for (cause in vars) {
    if (dep != cause) {
      # Testeo si "cause" causa en sentido de Granger a "dep"
      res <- causality(VAR_red, cause = cause)$Granger
      
      # Guardo el resultado con nombres claros
      GC_pairwise[[paste(cause, "->", dep)]] <- list(
        causa = cause,
        dependiente = dep,
        estadistico = res$statistic,
        pvalor = res$p.value
      )
    }
  }
}

# Paso 2: Mostrar resultados de forma legible
for (n in names(GC_pairwise)) {
  r <- GC_pairwise[[n]]
  cat("\n--------------------------------------\n")
  cat("Test:", r$causa, "→", r$dependiente, "\n")
  cat("Estadístico F:", r$estadistico, "\n")
  cat("p-valor:", r$pvalor, "\n")
  if (r$pvalor < 0.05) {
    cat("➡ Se rechaza H0: existe causalidad de Granger.\n")
  } else {
    cat("➡ No se rechaza H0: no hay evidencia de causalidad de Granger.\n")
  }
}


# Tests de diagnóstico para el VAR reducido -------------------------------

# a. Estabilidad del VAR ----
# Raíces del polinomio característico (deben ser < 1 en módulo para estabilidad)
VAR.roots <- roots(VAR_red, modulus = TRUE)
VAR.roots

# b. Autocorrelación de residuos ----

# Regla práctica de Hyndman: min(10, T/5)
h.PT <- min(10, trunc(T / 5))
h.PT
# Test de Portmanteau (asintótico). H0: los residuos no tienen autocorrelación hasta el rezago h (en este caso, hasta 10)
VAR.PT.test.serial <- serial.test(VAR_red, lags.pt = h.PT, type = "PT.asymptotic")
VAR.PT.test.serial

# c. Normalidad de residuos ----
# Jarque-Bera multivariado y univariado. H0: los residuos del VAR son normalmente distribuidos
VAR.JB.test <- normality.test(VAR_red, multivariate.only = FALSE)
VAR.JB.test

# d. Heterocedasticidad condicional ----
q <- 2
VAR.ARCH.test <- arch.test(VAR_red, lags.multi = 12, multivariate.only = FALSE) #H0: no hay heterocedasticidad condicional → la varianza de los residuos es constante en el tiempo.
VAR.ARCH.test

# e. Test de estabilidad estructural ----
stab.test <- stability(VAR_red, type = "fluctuation")
plot(stab.test)


# 2. VAR recursivo --------------------------------------------------------

# a. Funciones de Impulso-Respuesta ----
# Respuestas de cada variable a los shocks en las otras del modelo
irf_all <- irf(VAR_red, n.ahead = 5, boot = TRUE, ci = 0.95, ortho = TRUE)
plot(irf_all)

# IRFs acumuladas para todas las variables 
irf_all <- irf(VAR_red, n.ahead = 5, boot = TRUE, ci = 0.95, ortho = TRUE, cumulative = TRUE)
plot(irf_all)

#IRFs acumuladas individuales
# Ejemplo: impulso = d_lPIB, respuesta = BC
irf_bc_pib <- irf(VAR_red, impulse = "d_lPIB", response = "BC", 
                  n.ahead = 5, boot = TRUE, ci = 0.95, 
                  ortho = TRUE, cumulative = TRUE)

# Paso 1: extraer IRF + intervalos
h <- 0:5
irf_df <- data.frame(
  horizonte = h,
  irf = irf_bc_pib$irf$d_lPIB[, "BC"],
  lower = irf_bc_pib$Lower$d_lPIB[, "BC"],
  upper = irf_bc_pib$Upper$d_lPIB[, "BC"]
)

# Paso 2: graficar
ggplot(irf_df, aes(x = horizonte, y = irf)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5) + # bandas de confianza
  geom_line(color = "black", size = 1) +  # línea central
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # línea base
  labs(title = "IRF acumulada: respuesta de la Balanza Comercial a un shock en el PIB",
       x = "Horizonte (años)", 
       y = "Respuesta acumulada de BC") +
  theme_minimal(base_size = 13)

# 3. Descomposición de varianza de los errores de pronóstico (FEVD)
library(ggplot2)
library(dplyr)
library(tidyr)

# b. Descomposición de varianza ----
fevd_chol <- fevd(VAR_red, n.ahead = 5)

# Paso 1: convertir a data frame largo (tidy)
fevd_df <- lapply(names(fevd_chol), function(var) {
  as.data.frame(fevd_chol[[var]]) %>%
    mutate(horizonte = 1:nrow(.), variable = var)
}) %>%
  bind_rows() %>%
  pivot_longer(-c(horizonte, variable),
               names_to = "shock",
               values_to = "prop_var")

# Paso 2: graficar con barras apiladas
ggplot(fevd_df, aes(x = horizonte, y = prop_var, fill = shock)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap(~ variable, ncol = 2, scales = "free_y") +
  labs(title = "Descomposición de varianza de los errores de pronóstico",
       x = "Horizonte", 
       y = "Proporción de la varianza explicada",
       fill = "Shock") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# 3. VAR en dos submuestras: 1930-1975 y 1976-2018 ----
# Submuestra 1: 1930-1975 ----
PS1_sub1 <- PS1_ts %>%
  filter(Año >= 1930 & Año <= 1975)

Y1 <- as.matrix(PS1_sub1[, c("Crec_socios", "d_lTI", "d_lPIB", "BC", 
                             "d_lExt_debt", "d_lSalarios")])
Y1 <- Y1[complete.cases(Y1), ]

# Selección del orden de rezagos ----
pmax1 <- 2   # número máximo de rezagos a evaluar (datos anuales → suele bastar 1 o 2)

popt1 <- VARselect(Y, lag.max = pmax1, type = "const")  
# VARselect (del paquete vars) compara distintos criterios de info
# Evalúa todos los VAR desde 1 hasta pmax rezagos
# type = "const" → incluye un intercepto en cada ecuación

popt1   # muestra los resultados de AIC, BIC, HQ y FPE

p1 <- popt1$selection["HQ(n)"] #extraigo el número de rezagos sugerido por el criterio de Hannan-Quinn (HQ)

# Estimación del VAR reducido ---------------------------------------------

VAR_red1 <- VAR(Y1, p = 2, type = "const")   
# estimo el VAR reducido con los datos de Y, usando p rezagos y con constante

summary(VAR_red1)  
# muestro el resumen con coeficientes y tests básicos

m1 <- VAR_red1$K     # cantidad de variables en el VAR
m1
T1 <- VAR_red1$obs   # número de observaciones efectivas (después de perder p al inicio)
T1

plot(VAR_red1)  # grafica las series ajustadas vs. observadas y los residuos (sirve para diagnóstico inicial)    

# Test de causalidad de Granger ----

# Tomamos  los nombres de las variables desde Y
vars1 <- colnames(Y1)

# Lista para guardar resultados
GC_pairwise1 <- list()

# Loop sobre todas las combinaciones de pares (X causa Y)
for (dep in vars1) {
  for (cause in vars1) {
    if (dep != cause) {
      # Testeo si "cause" causa en sentido de Granger a "dep"
      res <- causality(VAR_red1, cause = cause)$Granger
      
      # Guardo el resultado con nombres claros
      GC_pairwise1[[paste(cause, "->", dep)]] <- list(
        causa = cause,
        dependiente = dep,
        estadistico = res$statistic,
        pvalor = res$p.value
      )
    }
  }
}

# Paso 2: Mostrar resultados de forma legible
for (n in names(GC_pairwise1)) {
  r <- GC_pairwise1[[n]]
  cat("\n--------------------------------------\n")
  cat("Test:", r$causa, "→", r$dependiente, "\n")
  cat("Estadístico F:", r$estadistico, "\n")
  cat("p-valor:", r$pvalor, "\n")
  if (r$pvalor < 0.05) {
    cat("➡ Se rechaza H0: existe causalidad de Granger.\n")
  } else {
    cat("➡ No se rechaza H0: no hay evidencia de causalidad de Granger.\n")
  }
}

# Tests de diagnóstico ----------------------------------------------------
# a. Estabilidad del VAR ----
# Raíces del polinomio característico (deben ser < 1 en módulo para estabilidad)
VAR1.roots <- roots(VAR_red1, modulus = TRUE)
VAR1.roots

# b. Autocorrelación de residuos ----
# Regla práctica de Hyndman: min(10, T/5)
h.PT1 <- min(10, trunc(T1 / 5))
h.PT1

# Test de Portmanteau (asintótico)
VAR1.PT.test.serial <- serial.test(VAR_red1, lags.pt = h.PT1, type = "PT.asymptotic")
VAR1.PT.test.serial

# c. Normalidad de residuos ----
# Jarque-Bera multivariado y univariado. H0: los residuos del VAR son normalmente distribuidos
VAR1.JB.test <- normality.test(VAR_red1, multivariate.only = FALSE)
VAR1.JB.test

# d. Heterocedasticidad condicional ----
q1 <- 2
VAR1.ARCH.test <- arch.test(VAR_red1, lags.multi = 12, multivariate.only = FALSE) 
# H0: no hay heterocedasticidad condicional → la varianza de los residuos es constante en el tiempo.
VAR1.ARCH.test


# VAR recursivo -----------------------------------------------------------

#IRFs acumulada: respuesta de la Balanza Comercial a un shock en el PIB ----

# Paso 1: calcular IRF
irf_bc_pib_sub1 <- irf(VAR_red1, impulse = "d_lPIB", response = "BC", 
                       n.ahead = 5, boot = TRUE, ci = 0.95, 
                       ortho = TRUE, cumulative = TRUE)

# Paso 2: armar data frame con horizonte e intervalos de confianza
h <- 0:5
irf_df_sub1 <- data.frame(
  horizonte = h,
  irf = irf_bc_pib_sub1$irf$d_lPIB[, "BC"],
  lower = irf_bc_pib_sub1$Lower$d_lPIB[, "BC"],
  upper = irf_bc_pib_sub1$Upper$d_lPIB[, "BC"]
)

# Paso 3: graficar
ggplot(irf_df_sub1, aes(x = horizonte, y = irf)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5) + # bandas de confianza
  geom_line(color = "black", size = 1) +  # línea central
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # línea base
  labs(title = "IRF acumulada: respuesta de la Balanza Comercial a un shock en el PIB",
       x = "Horizonte (años)", 
       y = "Respuesta acumulada de la Balanza Comercial") +
  theme_minimal(base_size = 13)


# Descomposición de la varianza -------------------------------------------

# Paso 1: calcular descomposición de varianza
fevd_chol1 <- fevd(VAR_red1, n.ahead = 5)

# Paso 2: convertir a data frame en formato largo (tidy)
fevd_df1 <- lapply(names(fevd_chol1), function(var) {
  as.data.frame(fevd_chol1[[var]]) %>%
    mutate(horizonte = 1:nrow(.), variable = var)
}) %>%
  bind_rows() %>%
  pivot_longer(-c(horizonte, variable),
               names_to = "shock",
               values_to = "prop_var")

# Paso 3: graficar con áreas apiladas
ggplot(fevd_df1, aes(x = horizonte, y = prop_var, fill = shock)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap(~ variable, ncol = 2, scales = "free_y") +
  labs(title = "Descomposición de varianza de los errores de pronóstico (1930–1975)",
       x = "Horizonte", 
       y = "Proporción de la varianza explicada",
       fill = "Shock") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# Submuestra 2: 1976-2018 ----
PS1_sub2 <- PS1_ts %>%
  filter(Año >= 1976 & Año <= 2018)

Y2 <- as.matrix(PS1_sub2[, c("Crec_socios", "d_lTI", "d_lPIB", "BC", 
                             "d_lExt_debt", "d_lSalarios")])
Y2 <- Y2[complete.cases(Y2), ]

# Selección del orden de rezagos ----
pmax2 <- 2   # número máximo de rezagos a evaluar (datos anuales → suele bastar 1 o 2)

popt2 <- VARselect(Y2, lag.max = pmax2, type = "const")  
popt2   # muestra los resultados de AIC, BIC, HQ y FPE

p2 <- popt2$selection["HQ(n)"] #extraigo el número de rezagos sugerido por HQ

# Estimación del VAR reducido ---------------------------------------------
VAR_red2 <- VAR(Y2, p = 2, type = "const")   # estimo VAR con 2 rezagos
summary(VAR_red2)

m2 <- VAR_red2$K     # cantidad de variables en el VAR
m2
T2 <- VAR_red2$obs   # número de observaciones efectivas
T2

plot(VAR_red2)  # gráfico inicial de series ajustadas y residuos

# Test de causalidad de Granger ----
vars2 <- colnames(Y2)
GC_pairwise2 <- list()

for (dep in vars2) {
  for (cause in vars2) {
    if (dep != cause) {
      res <- causality(VAR_red2, cause = cause)$Granger
      GC_pairwise2[[paste(cause, "->", dep)]] <- list(
        causa = cause,
        dependiente = dep,
        estadistico = res$statistic,
        pvalor = res$p.value
      )
    }
  }
}

for (n in names(GC_pairwise2)) {
  r <- GC_pairwise2[[n]]
  cat("\n--------------------------------------\n")
  cat("Test:", r$causa, "→", r$dependiente, "\n")
  cat("Estadístico F:", r$estadistico, "\n")
  cat("p-valor:", r$pvalor, "\n")
  if (r$pvalor < 0.05) {
    cat("➡ Se rechaza H0: existe causalidad de Granger.\n")
  } else {
    cat("➡ No se rechaza H0: no hay evidencia de causalidad de Granger.\n")
  }
}

# Tests de diagnóstico ----------------------------------------------------
VAR2.roots <- roots(VAR_red2, modulus = TRUE)
VAR2.roots

h.PT2 <- min(10, trunc(T2 / 5))
h.PT2
VAR2.PT.test.serial <- serial.test(VAR_red2, lags.pt = h.PT2, type = "PT.asymptotic")
VAR2.PT.test.serial

VAR2.JB.test <- normality.test(VAR_red2, multivariate.only = FALSE)
VAR2.JB.test

q2 <- 2
VAR2.ARCH.test <- arch.test(VAR_red2, lags.multi = 12, multivariate.only = FALSE) 
VAR2.ARCH.test

# VAR recursivo -----------------------------------------------------------

# IRF acumulada: respuesta de la Balanza Comercial a un shock en el PIB ----
irf_bc_pib_sub2 <- irf(VAR_red2, impulse = "d_lPIB", response = "BC", 
                       n.ahead = 5, boot = TRUE, ci = 0.95, 
                       ortho = TRUE, cumulative = TRUE)

h <- 0:5
irf_df_sub2 <- data.frame(
  horizonte = h,
  irf = irf_bc_pib_sub2$irf$d_lPIB[, "BC"],
  lower = irf_bc_pib_sub2$Lower$d_lPIB[, "BC"],
  upper = irf_bc_pib_sub2$Upper$d_lPIB[, "BC"]
)

ggplot(irf_df_sub2, aes(x = horizonte, y = irf)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.5) +
  geom_line(color = "black", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "IRF acumulada (1976–2018): respuesta de la Balanza Comercial a un shock en el PIB",
       x = "Horizonte (años)", 
       y = "Respuesta acumulada de la Balanza Comercial") +
  theme_minimal(base_size = 13)

# Descomposición de la varianza -------------------------------------------
fevd_chol2 <- fevd(VAR_red2, n.ahead = 5)

fevd_df2 <- lapply(names(fevd_chol2), function(var) {
  as.data.frame(fevd_chol2[[var]]) %>%
    mutate(horizonte = 1:nrow(.), variable = var)
}) %>%
  bind_rows() %>%
  pivot_longer(-c(horizonte, variable),
               names_to = "shock",
               values_to = "prop_var")

ggplot(fevd_df2, aes(x = horizonte, y = prop_var, fill = shock)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap(~ variable, ncol = 2, scales = "free_y") +
  labs(title = "Descomposición de varianza de los errores de pronóstico (1976–2018)",
       x = "Horizonte", 
       y = "Proporción de la varianza explicada",
       fill = "Shock") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
