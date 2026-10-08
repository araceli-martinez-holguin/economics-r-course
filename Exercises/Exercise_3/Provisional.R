#' ---
#' title: "Un modelo bayesiano utilizando la Penn World Table"
#' author: "Araceli Martínez Holguín"
#' date: "Octubre 2026"
#' output:
#'   pdf_document:
#'     keep_tex: true
#' ---
#'
#' OBJETIVO DEL LABORATORIO
#' ========================
#' Estimar los determinantes de la PRODUCTIVIDAD LABORAL a nivel país usando
#' modelos de regresión bayesianos. 
#' 
#' La variable dependiente es:
#'
#'   y = rgdpo / emp   →  PIB real por trabajador (en millones de USD de 2017)
#'
#' Usamos los Penn World Tables v11.0 (PWT), una base de datos macroeconómica
#' que cubre ~180 países desde 1950 hasta 2023.
#'
#' ESTRUCTURA DEL LABORATORIO
#' ──────────────────────────
#'   Sección 1 : Carga de librerías
#'   Sección 2 : Configuración del entorno
#'   Sección 3 : Carga, limpieza y exploración de datos
#'   Sección 4 : Modelo 1 — Efecto del capital humano sobre productividad
#'   Sección 5 : Modelo 2 — Agregando región como covariable categórica
#'   Sección 6 : Comparación gráfica de distribuciones posteriores
#'   Sección 7 : Comparación formal de modelos por LOO-CV
#'
#' TEORÍA BAYESIANA BÁSICA (repaso)
#' ─────────────────────────────────
#' En estadística frecuentista estimamos θ buscando el valor que maximiza la
#' verosimilitud: θ̂_MLE = argmax P(datos | θ).
#'
#' En estadística bayesiana combinamos:
#'   • Prior  P(θ)           : creencia sobre θ ANTES de ver los datos
#'   • Verosimilitud P(datos|θ): cuán probables son los datos dado θ
#'   • Posterior P(θ|datos)  : creencia ACTUALIZADA sobre θ tras los datos
#'
#'   Teorema de Bayes:  P(θ|datos) ∝ P(datos|θ) × P(θ)
#'
#' MCMC (Markov Chain Monte Carlo) nos permite obtener muestras de P(θ|datos)
#' cuando esta distribución no tiene forma analítica cerrada.
#'
#' El paquete brms compila el modelo en Stan y corre el muestreador HMC/NUTS.


# =============================================================================
# SECCIÓN 1: CARGA DE LIBRERÍAS
# =============================================================================
#   - readxl:     Leer archivos Excel (.xlsx) directamente en R
#   - dplyr:      Manipulación de datos (filter, mutate, group_by, etc.)
#   - tidyr:      Limpieza y reestructuración de datos (drop_na, pivot_*)
#   - brms:       Modelos de regresión bayesianos (usa Stan como motor)
#   - cmdstanr:   Interfaz a CmdStan — backend más rápido que RStan
#   - bayesplot:  Gráficas diagnósticas de cadenas MCMC y distribuciones posteriores
#   - tidybayes:  Extrae y visualiza muestras posteriores en formato tidy
#   - ggplot2:    Sistema base de gráficos (requerido por bayesplot y tidybayes)

# install traditional packages
install.packages(c("readxl", "dplyr", "tidyr","ggplot2",
                   "brms", "bayesplot", "tidybayes"))

# install rtools


# install cmdstanr
install.packages("cmdstanr", 
                 repos = c('https://stan-dev.r-universe.dev', 
                           getOption("repos")))


# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(brms)
library(cmdstanr)
library(bayesplot)
library(tidybayes)
library(ggplot2)

# Verify if runs smoothly
check_cmdstan_toolchain()

# Set cores
install_cmdstan(cores = 2)

# =============================================================================
# SECCIÓN 2: CONFIGURACIÓN DEL ENTORNO
# =============================================================================
# Ajusta este path al directorio donde guardaste el archivo pwt110.xlsx

setwd("~/Cursos-R/Laboratorios/Laboratorio-6")


# =============================================================================
# SECCIÓN 3: CARGA, LIMPIEZA Y EXPLORACIÓN DE DATOS
# =============================================================================

# ── 3.1 Cargar datos ──────────────────────────────────────────────────────────
# Los PWT se distribuyen como .xlsx. La hoja 3 ("Data") contiene los datos
# de panel: una fila por (país × año), 180 países × ~74 años ≈ 13,690 filas.

pwt <- read_xlsx("pwt110.xlsx", sheet = 3)

unique(pwt$country)

# Verificamos la estructura: tipos de variable, primeras observaciones, NAs.
str(pwt)

head(pwt)

# ── 3.2 Construcción de variables ─────────────────────────────────────────────
# Variables que usaremos en los modelos:
#
#   y   = rgdpo / emp    → Productividad laboral: PIB real por trabajador
#                          rgdpo = PIB real (millones USD 2017, base de producción)
#                          emp   = personas empleadas (millones)
#                          Unidad resultante: miles de USD por trabajador
#
#   x1  = hc             → Índice de capital humano (Psacharopoulos)
#                          Refleja años de escolaridad y retornos a la educación.
#                          Rango aprox. 1 (bajo) a 4 (alto).
#
#   x2  = log(rnna/pop)  → Log del stock de capital natural per cápita
#                          rnna = activos naturales nacionales (millones USD 2017)
#                          Controla por dotación de recursos naturales.
#
#   x3  = labsh          → Participación del trabajo en el ingreso nacional
#                          Rango 0–1. Una participación alta sugiere economías
#                          intensivas en trabajo; baja, intensivas en capital.
#
#   region → variable categórica creada manualmente por prefijo de countrycode
#            (proxy regional para efectos fijos de grupo en M2)

pwt_clean <- pwt %>%
  mutate(
    # Variable dependiente: productividad laboral (miles USD por trabajador)
    y    = rgdpo / emp,
    
    # Predictor 1: capital humano (ya está en escala 1–4, sin transformar)
    x1   = hc,
    
    # Predictor 2: log del capital natural per cápita
    # Se usa log para reducir sesgo por asimetría (distribución muy sesgada)
    # Se suma 1 antes del log para evitar log(0) cuando rnna o pop son ~0
    x2   = log((rnna / pop) + 1),
    
    # Predictor 3: participación laboral en el ingreso
    x3   = labsh,
    
    # Variable de región (proxy) basada en primeras letras del código de país
    # Esta clasificación es APROXIMADA — en un análisis real usarías una tabla
    # de correspondencia país-región validada (e.g., clasificación del Banco Mundial)
    region = case_when(
      countrycode %in% c("USA","CAN","MEX","GTM","HND","SLV","NIC","CRI",
                         "PAN","CUB","DOM","HTI","JAM","TTO","BLZ","GUY",
                         "SUR","BRB","LCA","VCT","GRD","ATG","DMA","KNA") ~ "Americas",
      countrycode %in% c("DEU","FRA","GBR","ITA","ESP","PRT","NLD","BEL",
                         "CHE","AUT","SWE","NOR","DNK","FIN","GRC","IRL",
                         "POL","CZE","SVK","HUN","ROU","BGR","HRV","SVN",
                         "LTU","LVA","EST","LUX","CYP","MLT","ALB","MKD",
                         "BIH","SRB","MNE","XKX","ISL","MDA","BLR","UKR",
                         "RUS","GEO","ARM","AZE","KAZ","UZB","TKM","KGZ","TJK") ~ "Europe_CentralAsia",
      countrycode %in% c("CHN","JPN","KOR","TWN","HKG","SGP","MYS","THA",
                         "IDN","PHL","VNM","MMR","KHM","LAO","BRN","MNG",
                         "PRK","BGD","IND","PAK","LKA","NPL","BTN","MDV",
                         "AFG","PNG","FJI","SLB","VUT","WSM","TON") ~ "Asia_Pacific",
      countrycode %in% c("NGA","ZAF","KEN","ETH","GHA","TZA","UGA","MOZ",
                         "AGO","ZMB","ZWE","CMR","CIV","SEN","MLI","BFA",
                         "NER","TCD","SDN","MDG","RWA","BDI","SLE","LBR",
                         "GIN","BEN","TGO","GAB","COG","COD","CAF","ERI",
                         "DJI","SOM","MRT","GMB","GNB","SWZ","LSO","BWA",
                         "NAM","MUS","CPV","COM","STP","SYC") ~ "Africa",
      countrycode %in% c("SAU","IRN","TUR","ISR","ARE","QAT","KWT","BHR",
                         "OMN","YEM","JOR","LBN","SYR","IRQ","EGY","DZA",
                         "MAR","TUN","LBY","MDA") ~ "MiddleEast_NorthAfrica",
      countrycode %in% c("BRA","ARG","COL","CHL","PER","VEN","ECU","BOL",
                         "PRY","URY","GUY","SUR") ~ "LatinAmerica_Caribbean",
      TRUE ~ "Other"
    )
  ) %>%
  # Eliminar filas con NAs en cualquiera de las variables del modelo
  # (MCMC no tolera valores faltantes; hay que imputar o eliminar)
  drop_na(y, x1, x2, x3, region) %>%
  # Filtrar valores implausibles: productividad negativa o cero no tiene sentido
  filter(y > 0, emp > 0, rgdpo > 0)

# Resumen descriptivo de las variables del modelo
summary(pwt_clean[, c("y", "x1", "x2", "x3")])

# ── 3.3 Exploración visual ────────────────────────────────────────────────────
# Antes de ajustar cualquier modelo bayesiano, conviene visualizar:
#   (a) La distribución de y (¿es simétrica? ¿sesgada? ¿bimodal?)
#   (b) La relación bivariada entre cada predictor y y

# Distribución de la productividad laboral
ggplot(pwt_clean, aes(x = y)) +
  geom_histogram(bins = 60, fill = "steelblue", color = "white") +
  labs(title = "Distribución de la productividad laboral",
       x = "PIB real por trabajador (millones USD)", y = "Frecuencia") +
  theme_minimal()

# NOTA PEDAGÓGICA: ¿La distribución de y es muy asimétrica?
# Si sí, considera transformar: y_log = log(y). Una distribución más simétrica
# hace que el supuesto de familia gaussiana sea más razonable.
# En un ejercicio extendido, compara M_gaussian vs M_lognormal con LOO-CV.

# Relación x1 (capital humano) vs y
ggplot(pwt_clean, aes(x = x1, y = y)) +
  geom_point(alpha = 0.15, color = "steelblue") +
  geom_smooth(method = "lm", color = "firebrick") +
  labs(title = "Capital humano vs Productividad laboral",
       x = "Índice de capital humano (hc)",
       y = "PIB real por trabajador") +
  theme_minimal()

# Relación x3 (participación laboral) vs y
ggplot(pwt_clean, aes(x = x3, y = y)) +
  geom_point(alpha = 0.15, color = "steelblue") +
  geom_smooth(method = "lm", color = "firebrick") +
  labs(title = "Participación laboral vs Productividad",
       x = "Participación del trabajo en el ingreso (labsh)",
       y = "PIB real por trabajador") +
  theme_minimal()

# Productividad por región (boxplot)
ggplot(pwt_clean, aes(x = region, y = y, fill = region)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  labs(title = "Distribución de productividad por región",
       x = "Región", y = "PIB real por trabajador") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")


# =============================================================================
# SECCIÓN 4: MODELO 1 — Efecto del capital humano sobre productividad
# =============================================================================
#
# ESPECIFICACIÓN TEÓRICA
# ──────────────────────
# Basado en la función de producción de Cobb-Douglas aumentada con capital humano
# (Mankiw, Romer & Weil, 1992), la productividad laboral (Y/L) depende del:
#   - Capital humano (hc): educación y habilidades de la fuerza laboral
#   - Capital natural per cápita (rnna/pop): recursos disponibles por habitante
#   - Participación del trabajo (labsh): proxy de la estructura productiva
#
# FÓRMULA DEL MODELO
# ──────────────────
#   y ~ 1 + x1 + x2 + x3
#
#   β₀ (Intercept): productividad esperada cuando x1=x2=x3=0 (valor base)
#   β₁ (x1=hc):    cambio en productividad por 1 unidad de capital humano
#   β₂ (x2=log K natural/pop): efecto del capital natural per cápita
#   β₃ (x3=labsh): efecto de la participación laboral en el ingreso
#
# PRIOR
# ─────
# prior = normal(0, 1000) → Prior muy débilmente informativa ("vaga").
# Con σ=1000, la prior cubre un rango enorme de valores de β, prácticamente
# dejando que los datos determinen la distribución posterior.
#
# EJERCICIO DE REFLEXIÓN (para el estudiante):
# ¿Qué pasaría si usamos una prior más informativa, e.g. normal(0, 10)?
# ¿Cómo cambiaría la posterior si los datos son escasos?
#
# PARÁMETROS DE MCMC
# ──────────────────
# chains = 2  → Dos cadenas independientes (para diagnóstico de convergencia)
# iter = 2000 → 2000 iteraciones por cadena (1000 warmup + 1000 muestreo)
# cores = 4   → 4 núcleos de CPU en paralelo (1 por cadena + procesamiento)
# backend = "cmdstanr" → Usa CmdStan (más rápido y con mejor manejo de errores)

M1 <- brm(
  formula = y ~ 1 + x1 + x2 + x3,
  family  = "gaussian",
  prior   = c(set_prior("normal(0, 1000)", class = "b")),
  data    = pwt_clean,
  chains  = 2,
  iter    = 2000,
  cores   = 4,
  backend = "cmdstanr"
)

# ── 4.1 Resumen del modelo ────────────────────────────────────────────────────
# La tabla muestra para cada parámetro:
#   Estimate   = Media de la distribución posterior
#   Est.Error  = Desviación estándar posterior (≈ "error estándar bayesiano")
#   l-95% CI   = Límite inferior del intervalo de credibilidad al 95%
#   u-95% CI   = Límite superior del intervalo de credibilidad al 95%
#   Rhat       = Estadístico de convergencia (debe ser ≈ 1.00; >1.01 = problema)
#   Bulk_ESS   = Tamaño efectivo de muestra en el centro de la distribución
#   Tail_ESS   = Tamaño efectivo de muestra en las colas
#
# INTERPRETACIÓN:
# Si el intervalo de credibilidad de β₁ NO incluye el 0, hay evidencia bayesiana
# de que el capital humano tiene un efecto positivo sobre la productividad.

summary(M1, waic = TRUE)

# Extrae solo los coeficientes fijos (más limpio para presentaciones)
fixef(M1)
