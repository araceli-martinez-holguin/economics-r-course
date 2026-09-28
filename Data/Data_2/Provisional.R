# Laboratorio 3: Estimación de Pobreza con la ENIGH 2024
# Araceli Martínez Holguín
# 2026-04-19
# ======================================================================


# ### Curso: R para economistas


# #### Araceli Martínez Holguín


# #### 2026-04-19


# # 1 Introducción


# # 2 Instalación y carga de paquetes


# ## 2.1 Instalación (solo la primera vez)

# Ejecuta este bloque UNA SOLA VEZ en tu computadora.
# Después puedes comentarlo con # para no repetir la instalación.
# install.packages(c(
#  "tidyr", "readr", "ggplot2", "dplyr", "scales",
#  "survey", "srvyr", "foreign", "reldist", "ggthemes",
#  "purrr", "bayesplot", "tidybayes", "forcats"
# ))

# Instalación de RStan (motor bayesiano):
# Consulta las instrucciones oficiales en: https://mc-stan.org/install/
# remotes::install_github("stan-dev/rstan", ref = "develop", subdir = "rstan/rstan")
# Una vez instalado RStan, instala rstanarm:
# install.packages("rstanarm")


# ## 2.2 Carga de paquetes

# Cada paquete tiene un propósito específico:
library(tidyr)      # Transformación de datos (pivot_longer, pivot_wider)
library(readr)      # Lectura eficiente de archivos CSV
library(dplyr)      # Manipulación de datos (mutate, filter, group_by, etc.)
library(ggplot2)    # Visualizaciones con la gramática de gráficos
library(scales)     # Formatos para ejes (porcentajes, moneda, etc.)
library(survey)     # Análisis con diseños muestrales complejos
library(srvyr)      # Interfaz "tidyverse" para el paquete survey
library(reldist)    # Cálculo del coeficiente de Gini
library(foreign)    # Lectura de archivos de otros softwares estadísticos
library(ggthemes)   # Temas adicionales para ggplot2
library(forcats)    # Manejo de variables categóricas (factores)
library(purrr)      # Programación funcional (map, reduce, etc.)
library(bayesplot)  # Visualizaciones para modelos bayesianos
library(tidybayes)  # Extracción de resultados bayesianos en formato tidy


# # 3 Carga y exploración inicial de datos


# ## 3.1 Directorio de trabajo y lectura del archivo

# Establece la carpeta donde están tus archivos de datos.
# Cambia esta ruta a la ubicación correcta en tu computadora.
setwd("~/Cursos-R/Laboratorios/Laboratorio-3")

# Leemos el archivo principal de la ENIGH.
# Cada fila representa un hogar en México.
conc <- read_csv("concentradohogar.csv")

# conocer la estructura de la base
str(conc)

# Número observaciones
nrow(conc)

# Número de variables
ncol(conc)

# Nombre de variables
names(conc)


# ## 3.2 Exploración rápida
# # 4 Diseño muestral complejo

# Declaramos el diseño muestral con as_survey_design() del paquete srvyr.
# Argumentos:
#   ids     = upm      → Unidades Primarias de Muestreo (conglomerados/clústeres)
#   strata  = est_dis  → Estratos de diseño
#   weights = factor   → Factores de expansión poblacional
#   nest    = TRUE     → Indica que las UPM están anidadas dentro de los estratos
str(conc)

diseno <- conc %>%
  as_survey_design(
    ids     = upm,
    strata  = est_dis,
    weights = factor,
    nest    = TRUE
  )

str(diseno)

# Resumen del diseño: verifica que esté correctamente especificado
summary(diseno)


# # 5 Ingreso corriente: media ponderada vs. no ponderada


# ## 5.1 Comparación de estimaciones

# INCORRECTO: Media aritmética simple (ignora el diseño muestral)
media_simple <- mean(conc$ing_cor, na.rm = TRUE)
ingr_mens <- media_simple/3
ingr_mens
ingr_mens_per <- ingr_mens/2
ingr_mens_per

#Factor de expansion
conc$factor

# CORRECTO: Media ponderada usando el diseño muestral
media_pond <- diseno %>%
  summarise(
    media     = survey_mean(ing_cor, na.rm = TRUE),
    total_hog = survey_total(1)   # Número total de hogares que representa la encuesta
  )

ingr_mens_factor <-media_pond/3
ingr_mens_factor
ingr_mens_per_factor <- ingr_mens_factor/2
ingr_mens_per_factor
cat("=== Comparación de medias del ingreso corriente trimestral ===\n")

cat("Media sin ponderar: $", round(media_simple, 0), "\n")

cat("Media ponderada:    $", round(media_pond$media, 0), "\n")

cat("Error estándar:     $", round(media_pond$media_se, 0), "\n")

cat("Total de hogares representados:",
    format(media_pond$total_hog, big.mark = ","), "\n")


# ## 5.2 Estadísticas nacionales del ingreso mensual

# La ENIGH reporta el ingreso de forma trimestral.
# Dividimos entre 3 para obtener el equivalente mensual.

ing_nacional <- diseno %>%
  summarise(
    media   = survey_mean(ing_cor, na.rm = TRUE, vartype = "ci"),  # con intervalo de confianza
    mediana = survey_median(ing_cor, na.rm = TRUE),
    p25     = survey_quantile(ing_cor, quantiles = 0.25, na.rm = TRUE),
    p75     = survey_quantile(ing_cor, quantiles = 0.75, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~ . / 3))  # convertir a mensual

ing_nacional

cat("\n--- Ingreso corriente MENSUAL promedio por hogar ---\n")

cat("Media:   $", format(round(ing_nacional$media, 0), big.mark = ","), "\n")

cat("IC 95%: [$", format(round(ing_nacional$media_low, 0), big.mark = ","),
    ", $", format(round(ing_nacional$media_upp, 0), big.mark = ","), "]\n")


# ## 5.3 Distribución del ingreso mensual

# Creamos una nueva variable de ingreso mensual en el diseño muestral
diseno <- diseno %>%
  mutate(ing_mensual = ing_cor / 3)

# Extraemos los datos como data.frame para graficar con ggplot2
df_plot <- diseno %>%
  select(ing_mensual, factor) %>%
  as.data.frame()

# Calculamos media y mediana ponderadas para anotarlas en la gráfica
media_w <- diseno %>%
  summarise(media = survey_mean(ing_mensual, na.rm = TRUE)) %>%
  pull(media)

mediana_w <- diseno %>%
  summarise(mediana = survey_median(ing_mensual, na.rm = TRUE)) %>%
  pull(mediana)

# Histograma con curva de densidad (ponderada)
distribucion_ingreso <- ggplot(df_plot, aes(x = ing_mensual, weight = factor)) +
  geom_histogram(aes(y = after_stat(density)), bins = 80,
                 fill = "forestgreen", alpha = 0.6, color = "white") +
  geom_density(color = "black", lwd = 1, adjust = 1.2) +
  scale_x_continuous(
    labels = scales::comma,
    limits = c(0, quantile(df_plot$ing_mensual, 0.99, na.rm = TRUE))
  ) +
  geom_vline(xintercept = media_w, color = "blue", linetype = "dashed", lwd = 1) +
  geom_vline(xintercept = mediana_w, color = "darkorchid", linetype = "dashed", lwd = 1) +
  annotate("text", x = media_w + 2000, y = 0.00002,
           label = paste("Media:", round(media_w)), color = "blue") +
  annotate("text", x = mediana_w + 2000, y = 0.000018,
           label = paste("Mediana:", round(mediana_w)), color = "darkorchid") +
  labs(
    title = "Distribución del ingreso mensual del hogar (con ponderadores)",
    x = "Ingreso mensual del hogar (MXN)", y = "Densidad"
  ) +
  theme_minimal()
# Notacion cientifica 
distribucion_ingreso


# # 6 Deciles de ingreso y desigualdad

# ## 6.1 Construcción de deciles con factores de expansión

# Método acumulativo: ordena los hogares por ingreso y asigna deciles
# considerando cuántos hogares reales representa cada fila (factor de expansión).
range(conc$ing_cor)


conc <- conc %>%
  arrange(ing_cor) %>%
  mutate(
    acum_peso    = cumsum(factor),          # peso acumulado
    total_peso   = sum(factor),             # peso total de la población
    decil_ingreso = ceiling(acum_peso / total_peso * 10),  # decil de 1 a 10
    decil_ingreso = pmin(decil_ingreso, 10) # garantiza que el máximo sea 10
  )

unique(conc$acum_peso)

# Reconstruimos el diseño con la nueva variable de decil
diseno2 <- conc %>%
  as_survey_design(ids = upm, strata = est_dis, weights = factor, nest = TRUE)

# Ingreso promedio mensual por decil
ing_decil <- diseno2 %>%
  group_by(decil_ingreso) %>%
  summarise(
    ing_prom  = survey_mean(ing_cor, na.rm = TRUE),
    n_hogares = survey_total(1)
  ) %>%
  mutate(ing_prom_mensual = ing_prom / 3)

print(ing_decil)


# ## 6.2 Gráfica de ingreso por decil

deciles_ingreso_plot <- ggplot(
  ing_decil,
  aes(x = factor(decil_ingreso), y = ing_prom_mensual, fill = factor(decil_ingreso))
) +
  geom_col(alpha = 0.85) +
  geom_text(
    aes(label = dollar(ing_prom_mensual, prefix = "$", big.mark = ",", accuracy = 1)),
    vjust = -0.4, size = 3
  ) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  scale_fill_manual(
    values = colorRampPalette(c("#fee5c9", "#b5450a"))(10),
    guide  = "none"
  ) +
  labs(
    title    = "Ingreso corriente mensual promedio por decil de hogares",
    subtitle = "ENIGH 2024, pesos corrientes",
    x = "Decil", y = "Ingreso promedio mensual ($)",
    caption  = "Fuente: INEGI, ENIGH 2024."
  ) +
  theme_minimal(base_size = 12)

deciles_ingreso_plot


# ## 6.3 Desigualdad: razón 10/1 y participación en el ingreso

# Razón decil 10 / decil 1: ¿cuántas veces más gana el grupo más rico?
razon_10_1 <- ing_decil$ing_prom[ing_decil$decil_ingreso == 10] /
  ing_decil$ing_prom[ing_decil$decil_ingreso == 1]

cat("Razón decil 10 / decil 1:", round(razon_10_1, 1), "veces\n")

# Participación porcentual de cada decil en el ingreso total
ing_decil <- ing_decil %>%
  mutate(participacion = ing_prom * n_hogares / sum(ing_prom * n_hogares) * 100)

print(select(ing_decil, decil_ingreso, ing_prom_mensual, participacion))

max_part <- max(ing_decil$participacion, na.rm = TRUE)

participacion_ingreso_plot <- ggplot(
  ing_decil,
  aes(x = factor(decil_ingreso), y = participacion, fill = factor(decil_ingreso))
) +
  geom_col(alpha = 0.85) +
  geom_text(
    aes(label = paste0(round(participacion, 0), "%")),
    vjust = -0.4, size = 3.5
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, max_part * 1.1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = colorRampPalette(c("#deebf7", "#08306b"))(10),
    guide  = "none"
  ) +
  labs(
    title    = "Participación de cada decil en el ingreso total",
    subtitle = "ENIGH 2024",
    x = "Decil de hogares", y = "Participación en el ingreso total (%)",
    caption  = "Fuente: INEGI, ENIGH 2024."
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

participacion_ingreso_plot


# ## 6.4 Coeficiente de Gini

# El coeficiente de Gini mide desigualdad:
# 0 = igualdad perfecta (todos ganan lo mismo)
# 1 = desigualdad perfecta (un solo hogar concentra todo el ingreso)
# Para México, valores típicos están entre 0.40 y 0.55.

gini_val <- with(conc, gini(ing_cor, w = factor))
cat("Coeficiente de Gini (ENIGH 2024):", round(gini_val, 4), "\n")
