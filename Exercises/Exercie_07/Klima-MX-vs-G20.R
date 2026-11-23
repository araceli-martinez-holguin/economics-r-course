# Analizando variables climáticas utilizando el CRU-data
#### Nombre: araceli martinez
#### Practica 1: Analizando variables climáticas
#### Fecha: 17 de marzo de 2026 

# En caso de no tener instalada la libreria
# install.packages("readr")

# Librerias
library(readr)

# Directorio de trabajo
setwd("~/Laboratorio-1")

# Establecemos las variables a descargar
variables <- c("pre", "tmn", "tmp", "tmx")

# countries <- c("Mexico", "Brazil", "USA", "Germany", "China",
# "Argentina", "Russia", "Finland", "United_Kingdom","Japan" )

# Seleccionamos los países
g20_countries <- unique(c(
  "Argentina", "Australia", "Brazil", "Canada", "China",
  "France", "Germany", "India", "Indonesia", "Italy",
  "Japan", "Mexico", "Russia", "Saudi_Arabia", "South_Africa",
  "South_Korea", "Turkey", "United_Kingdom", "USA",
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
  "Czech_Republic", "Denmark", "Estonia", "Finland", "Greece",
  "Hungary", "Ireland", "Latvia", "Lithuania", "Luxembourg",
  "Malta", "Netherlands", "Poland", "Portugal", "Romania",
  "Slovakia", "Slovenia", "Spain", "Sweden"
))

# URL de dónde vamos a descargar
base_url <- "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/crucy.2503061057.v4.09/countries"

# Este loop nos va a ayudar a descargar los archivos per
for (country in g20_countries) {
  for (var in variables) {
    file <- paste0("crucy.v4.09.1901.2024.", country, ".", var, ".per")
    if (!file.exists(file)) {
      tryCatch(
        download.file(paste0(base_url, "/", var, "/", file), destfile = file, mode = "wb"),
        error = function(e) message("404: ", file)
      )
    }
  }
}

# Establecemos el nombre de las columnas
col_names <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
               "MAM", "JJA", "SON", "DJF", "ANN")

# Creamos una lista vacía
climate_data <- list()

# Este loop nos sirve para cargar los archivos .per como dataframes
for (country in g20_countries) {
  for (var in variables) {
    file <- paste0("crucy.v4.09.1901.2024.", country, ".", var, ".per")
    if (file.exists(file)) {
      df <- read_table(file, skip = 4, col_names = col_names, na = "-999.0")
      df$Country <- country
      df$Variable <- var
      climate_data[[paste0(country, "_", var)]] <- df
    }
  }
}

# ============================================================
# 1. Calcular la precipitación mensual promedio para México
# ============================================================

# Seleccionamos México y la variable precipitación
mexico_pre <- climate_data[["Mexico_pre"]]

# Creamos el vector mes
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Obtenemos la media, desviación estándar, mínimo y máximo
mean_monthly <- sapply(months, function(m) mean(mexico_pre[[m]], na.rm = TRUE))
sd_monthly   <- sapply(months, function(m) sd(mexico_pre[[m]], na.rm = TRUE))
min_monthly  <- sapply(months, function(m) min(mexico_pre[[m]], na.rm = TRUE))
max_monthly  <- sapply(months, function(m) max(mexico_pre[[m]], na.rm = TRUE))


# Convertimos a entero y redondeamos a 0 decimales
mean_monthly <- as.integer(round(mean_monthly))
sd_monthly   <- as.integer(round(sd_monthly))
min_monthly  <- as.integer(round(min_monthly))
max_monthly  <- as.integer(round(max_monthly))

# Guardamos las estadística en una tabla
stats_mexico <- data.frame(
  Month = months,
  Mean  = mean_monthly,
  SD    = sd_monthly,
  Min   = min_monthly,
  Max   = max_monthly
)
print(stats_mexico)

# Graficamos en un gráfico de barras
png("bp_precipitacion_mexico.png", width = 800, height = 600, res = 100)
bp_mexico <- barplot(mean_monthly,
              names.arg = months,
              col       = "deepskyblue4",
              main      = "Precipitación Mensual Promedio en México (1901-2024)",
              ylab      = "Precipitación (milímetros de lluvia - un litro por metro cuadrado)",
              xlab      = "Mes",
              ylim      = c(0, max(mean_monthly) * 1.15),
              cex.names = 0.9,
              cex.axis  = 0.9,
              las       = 1,
              font.lab  = 2)

text(x      = bp_mexico[,1],
     y      = mean_monthly + 2,
     labels = mean_monthly,
     cex    = 0.8,
     col    = "black",
     adj = c(0.5, 0))

abline(h   = mean(mean_monthly),
       col = "red",
       lwd = 2,
       lty = 2)

legend("topleft",
       legend = paste("Media anual:", round(mean(mean_monthly)), "mm"),
       col    = "red",
       lty    = 2,
       lwd    = 2,
       bty    = "n")


dev.off()


# ============================================================
# EJERCICIO 1: Calcular la precipitación mensual promedio para otro país
# ============================================================

# Seleccionamos Brasil y la variable precipitación
brazil_pre <- climate_data[["Brazil_pre"]]

mean_monthly <- sapply(months, function(m) mean(brazil_pre[[m]], na.rm = TRUE))
sd_monthly   <- sapply(months, function(m) sd(brazil_pre[[m]], na.rm = TRUE))
min_monthly  <- sapply(months, function(m) min(brazil_pre[[m]], na.rm = TRUE))
max_monthly  <- sapply(months, function(m) max(brazil_pre[[m]], na.rm = TRUE))

# Obtenemos las estadísticas
mean_monthly <- as.integer(round(mean_monthly))
sd_monthly   <- as.integer(round(sd_monthly))
min_monthly  <- as.integer(round(min_monthly))
max_monthly  <- as.integer(round(max_monthly))

# Las guardamos en una tabla
stats_brazil <- data.frame(
  Month = months,
  Mean  = mean_monthly,
  SD    = sd_monthly,
  Min   = min_monthly,
  Max   = max_monthly
)
print(stats_brazil)

png("bp_precipitacion_brasil.png", width = 800, height = 600, res = 100)
bp_brazil <- barplot(mean_monthly,
                     names.arg = months,
                     col       = "dodgerblue4",
                     main      = "Precipitación Mensual Promedio  en Brasil (1901-2024)",
                     ylab      = "Precipitación (milímetros de lluvia - un litro por metro cuadrado )",
                     xlab      = "Mes",
                     ylim      = c(0, max(mean_monthly) * 1.15),
                     cex.names = 0.9,
                     cex.axis  = 0.9,
                     las       = 1,
                     font.lab  = 2)
text(x      = bp_brazil[,1],
     y      = mean_monthly + 2,
     labels = mean_monthly,
     cex    = 0.8,
     col    = "black",
     adj = c(0.5, 0))
abline(h   = mean(mean_monthly),
       col = "red",
       lwd = 2,
       lty = 2)
legend("topright",
       legend = paste("Media anual:", round(mean(mean_monthly)), "mm"),
       col    = "red",
       lty    = 2,
       lwd    = 2,
       bty    = "n")
dev.off()

# ============================================================
# EJERCICIO 2: Calcular la temperatura mensual promedio para dos países
# ============================================================


# ============================================================
# 2. Estimar la precipitación promedio anual para diferentes países
# ============================================================

# Media anual de precipitación para cada país
annual_pre <- data.frame(Country = character(), Mean_ANN = numeric())

# Este loop calcula la precipitación media anual durante todo el periodo para cada país
for (country in g20_countries) {
  key <- paste0(country, "_pre")
  if (key %in% names(climate_data)) {
    mean_ann <- mean(climate_data[[key]]$ANN, na.rm = TRUE)
    annual_pre <- rbind(annual_pre, data.frame(Country = country, Mean_ANN = mean_ann))
  }
}

# Ordenar de mayor a menor precipitación
annual_pre <- annual_pre[order(annual_pre$Mean_ANN, decreasing = TRUE), ]

# Crear una paleta de colores para degradarlos
blues_palette <- colorRampPalette(c("#97FFFF", "#8DEEEE", "#79CDCD", "#528B8B", "#2F4F4F"))
colors <- blues_palette(nrow(annual_pre))[rank(annual_pre$Mean_ANN)]
# Barplot
png("precipitacion_anual_g20.png", width = 1200, height = 600, res = 100)
bp_g20 <- barplot(annual_pre$Mean_ANN,
                  names.arg = annual_pre$Country,
                  las       = 2,
                  col       = colors,
                  main      = "Precipitación anual promedio por país G20",
                  ylab      = "Precipitación (mm)",
                  xlab      = "País",
                  ylim      = c(0, max(annual_pre$Mean_ANN) * 1.15),
                  cex.names = 0.6,
                  cex.axis  = 0.9,
                  font.lab  = 2)

text(x      = bp_g20[,1],
     y      = annual_pre$Mean_ANN + 5,
     labels = round(annual_pre$Mean_ANN),
     cex    = 0.6,
     col    = "black",
     adj    = c(0.5, 0))

abline(h   = mean(annual_pre$Mean_ANN),
       col = "red",
       lwd = 2,
       lty = 2)

legend("topright",
       legend = paste("Media G20:", round(mean(annual_pre$Mean_ANN)), "mm"),
       col    = "red",
       lty    = 2,
       lwd    = 2,
       bty    = "n")

dev.off()

# ============================================================
# Ejercicio 3. Estimar la temperatura promedio anual para diferentes países
# ============================================================

# Media anual de temperatura para cada país
annual_tmp <- data.frame(Country = character(), Mean_ANN = numeric())

for (country in g20_countries) {
  key <- paste0(country, "_tmp")
  if (key %in% names(climate_data)) {
    mean_ann <- mean(climate_data[[key]]$ANN, na.rm = TRUE)
    annual_tmp <- rbind(annual_tmp, data.frame(Country = country, Mean_ANN = mean_ann))
  }
}

# Ordenar de mayor a menor temperatura
annual_tmp <- annual_tmp[order(annual_tmp$Mean_ANN, decreasing = TRUE), ]

# Barplot
dark_palette <- colorRampPalette(c("brown4", "brown", "brown3", "brown2","brown1"))
colors <- dark_palette(nrow(annual_tmp))[rank(annual_tmp$Mean_ANN)]

png("temperatura_anual_g20.png", width = 1200, height = 600, res = 100)
bp_tmp_g20 <- barplot(annual_tmp$Mean_ANN,
                  names.arg = annual_tmp$Country,
                  las       = 2,
                  col       = colors,
                  main      = "Temperatura anual promedio por país G20",
                  ylab      = "Temperatura (°C)",
                  xlab      = "País",
                  ylim      = c(0, max(annual_tmp$Mean_ANN) * 1.15),
                  cex.names = 0.6,
                  cex.axis  = 0.9,
                  font.lab  = 2)

text(x      = bp_tmp_g20[,1],
     y      = annual_tmp$Mean_ANN + 5,
     labels = round(annual_tmp$Mean_ANN),
     cex    = 0.6,
     col    = "black",
     adj    = c(0.5, 0))

abline(h   = mean(annual_tmp$Mean_ANN),
       col = "red",
       lwd = 2,
       lty = 2)

legend("topright",
       legend = paste("Media G20:", round(mean(annual_tmp$Mean_ANN)), "mm"),
       col    = "red",
       lty    = 2,
       lwd    = 2,
       bty    = "n")

dev.off()


# ============================================================
# 3. Calcular la serie de tiempo con tendencia para la temperatura 
# ============================================================
png("tmp_tendencia_mexico.png", width = 800, height = 600, res = 100)

par(mar = c(5, 4, 4, 2))

# Serie de tiempo de temperatura anual para un país
country_ts <- climate_data[["Mexico_tmp"]]

tmp_tendencia_mexico <- plot(country_ts$Year, country_ts$ANN,
     type = "l",
     col  = "tomato",
     lwd  = 2,
     main = "Temperatura Promedio Anual de México (1901-2024)",
     xlab = "Año",
     ylab = "Temperatura (°C)")

# Línea de tendencia
trend <- lm(ANN ~ Year, data = country_ts)
abline(trend, col = "black", lwd = 2, lty = 2)

legend("topleft",
       legend = c("Temperatura anual", "Tendencia"),
       col    = c("tomato", "black"),
       lty    = c(1, 2),
       lwd    = 2)

dev.off()

# ============================================================
# Ejercicio 4. Calcular la serie de tiempo con tendencia para la temperatura de otro país
# ============================================================


png("tmp_tendencia_brazil.png", width = 800, height = 600, res = 100)

par(mar = c(5, 4, 4, 2))

# Serie de tiempo de temperatura anual para un país
country_ts <- climate_data[["Brazil_tmp"]]

tmp_tendencia_brazil <- plot(country_ts$Year, country_ts$ANN,
                             type = "l",
                             col  = "firebrick",
                             lwd  = 2,
                             main = "Temperatura Promedio Anual de Brazil (1901-2024)",
                             xlab = "Año",
                             ylab = "Temperatura (°C)")

# Línea de tendencia
trend <- lm(ANN ~ Year, data = country_ts)
abline(trend, col = "black", lwd = 2, lty = 2)

legend("topleft",
       legend = c("Temperatura anual", "Tendencia"),
       col    = c("firebrick", "black"),
       lty    = c(1, 2),
       lwd    = 2)

dev.off()

# ============================================================
# Ejercicio 4. Calcular la serie de tiempo con tendencia para la precipitación de 2 países
# ============================================================
png("pre_tendencia_mexico.png", width = 800, height = 600, res = 100)

par(mar = c(5, 4, 4, 2))

# Serie de tiempo de temperatura anual para un país
country_ts <- climate_data[["Mexico_pre"]]

pre_tendencia_mexico <- plot(country_ts$Year, country_ts$ANN,
                             type = "l",
                             col  = "slateblue1",
                             lwd  = 2,
                             main = "Precipitación Promedio Anual de México (1901-2024)",
                             xlab = "Año",
                             ylab = "Precipitación (mm)")

# Línea de tendencia
trend <- lm(ANN ~ Year, data = country_ts)
abline(trend, col = "black", lwd = 2, lty = 2)

legend("topleft",
       legend = c("Precipitación anual", "Tendencia"),
       col    = c("slateblue1", "black"),
       lty    = c(1, 2),
       lwd    = 2)

dev.off()

png("pre_tendencia_brazil.png", width = 800, height = 600, res = 100)

par(mar = c(5, 4, 4, 2))

# Serie de tiempo de temperatura anual para un país
country_ts <- climate_data[["Brazil_pre"]]

pre_tendencia_brazil <- plot(country_ts$Year, country_ts$ANN,
                             type = "l",
                             col  = "slateblue4",
                             lwd  = 2,
                             main = "Precipitación Promedio Anual de Brazil (1901-2024)",
                             xlab = "Año",
                             ylab = "Precipitación (mm)")

# Línea de tendencia
trend <- lm(ANN ~ Year, data = country_ts)
abline(trend, col = "black", lwd = 2, lty = 2)

legend("topleft",
       legend = c("Temperatura anual", "Tendencia"),
       col    = c("slateblue4", "black"),
       lty    = c(1, 2),
       lwd    = 2)

dev.off()

# ============================================================
# 4. Estimar la Tasa de Crecimiento de la Temperatura 
# ------------------------------------------------------------
par(mar = c(5, 4, 4, 2))

# Serie de tiempo de temperatura anual para un país
country_ts <- climate_data[["Mexico_tmp"]]

# --- YoY Growth Rate ---
country_ts$YoY_growth <- c(NA, diff(country_ts$ANN) / head(country_ts$ANN, -1) * 100)

png("yoy_growth_mexico.png", width = 800, height = 600, res = 100)
par(mar = c(5, 4, 4, 2))

plot(country_ts$Year[-1], country_ts$YoY_growth[-1],
     type = "l",
     col  = "tomato",
     lwd  = 2,
     main = "Tasa de Crecimiento de Temperatura en México (1901-2024)",
     xlab = "Año",
     ylab = "%")
abline(h = 0, col = "black", lty = 2)

dev.off()

# ============================================================
# Ejercicio 5. Estimar la Tasa de Crecimiento de la Temperatura para otro país
# ============================================================

par(mar = c(5, 4, 4, 2))

# Serie de tiempo de temperatura anual para un país
country_ts <- climate_data[["Brazil_tmp"]]

# --- YoY Growth Rate ---
country_ts$YoY_growth <- c(NA, diff(country_ts$ANN) / head(country_ts$ANN, -1) * 100)

png("yoy_growth_brazil.png", width = 800, height = 600, res = 100)
par(mar = c(5, 4, 4, 2))

plot(country_ts$Year[-1], country_ts$YoY_growth[-1],
     type = "l",
     col  = "firebrick",
     lwd  = 2,
     main = "Tasa de Crecimiento de Temperatura en Brasil (1901-2024)",
     xlab = "Año",
     ylab = "(%)")
abline(h = 0, col = "black", lty = 2)

dev.off()

# ============================================================
# Ejercicio 6. Estimar la Tasa de Crecimiento de la Precipitación para dos países
# ============================================================
par(mar = c(5, 4, 4, 2))

# Serie de tiempo de temperatura anual para un país
country_ts <- climate_data[["Mexico_pre"]]

# --- YoY Growth Rate ---
country_ts$YoY_growth <- c(NA, diff(country_ts$ANN) / head(country_ts$ANN, -1) * 100)

png("yoy_growth_mexico.png", width = 800, height = 600, res = 100)
par(mar = c(5, 4, 4, 2))

plot(country_ts$Year[-1], country_ts$YoY_growth[-1],
     type = "l",
     col  = "slateblue",
     lwd  = 2,
     main = "Tasa de Crecimiento de la Precipitación en México (1901-2024)",
     xlab = "Año",
     ylab = "%")
abline(h = 0, col = "black", lty = 2)

dev.off()

par(mar = c(5, 4, 4, 2))

# Serie de tiempo de temperatura anual para un país
country_ts <- climate_data[["Brazil_pre"]]

# --- YoY Growth Rate ---
country_ts$YoY_growth <- c(NA, diff(country_ts$ANN) / head(country_ts$ANN, -1) * 100)

png("yoy_growth_brazil.png", width = 800, height = 600, res = 100)
par(mar = c(5, 4, 4, 2))

plot(country_ts$Year[-1], country_ts$YoY_growth[-1],
     type = "l",
     col  = "slateblue4",
     lwd  = 2,
     main = "Tasa de Crecimiento de Precipitación en Brasil (1901-2024)",
     xlab = "Año",
     ylab = "(%)")
abline(h = 0, col = "black", lty = 2)

dev.off()


# ============================================================
# 4. Realizar un bubble plot utilizando temperatura sobre el año más cálido y el más frío para cada país
# ============================================================
# ── Extreme years bubble panel — base R ───────────────────────────────────────

extreme_years <- data.frame(
  Country   = character(),
  Hottest   = numeric(),
  Coldest   = numeric(),
  Year_Hot  = numeric(),
  Year_Cold = numeric()
)

for (country in g20_countries) {
  key <- paste0(country, "_tmp")
  if (key %in% names(climate_data)) {
    df        <- climate_data[[key]]
    extreme_years <- rbind(extreme_years, data.frame(
      Country   = country,
      Hottest   = round(max(df$ANN, na.rm = TRUE), 2),
      Coldest   = round(min(df$ANN, na.rm = TRUE), 2),
      Year_Hot  = df$Year[which.max(df$ANN)],
      Year_Cold = df$Year[which.min(df$ANN)]
    ))
  }
}

print(extreme_years)

# ── Reusable plot function ────────────────────────────────────────────────────
plot_bubble_panel <- function(data, temp_col, year_col, file_prefix, title, ramp, unit) {
  
  years      <- sort(unique(data[[year_col]]))
  chunks     <- split(years, ceiling(seq_along(years) / 6))
  global_min <- min(data[[temp_col]])
  global_max <- max(data[[temp_col]])
  
  for (i in seq_along(chunks)) {
    
    chunk_years <- chunks[[i]]
    n_panels    <- length(chunk_years)
    n_cols      <- 3
    n_rows      <- ceiling(n_panels / n_cols)
    
    file <- paste0(file_prefix, "_part", i, ".png")
    png(file, width = 5000, height = ceiling(n_rows * 3500 / 2), res = 300)
    
    par(mfrow = c(n_rows, n_cols),
        mar   = c(7.2, 6.0, 4.8, 1.8),
        oma   = c(2.4, 3.6, 6.0, 1.2),
        bg    = "white")
    
    for (yr in chunk_years) {
      
      sub <- data[data[[year_col]] == yr, ]
      sub <- sub[order(sub[[temp_col]]), ]
      n   <- nrow(sub)
      x   <- seq_len(n)
      
      cex_vals <- 1.8 + (sub[[temp_col]] - global_min) / (global_max - global_min) * 7.2
      
      pal      <- colorRampPalette(ramp)(100)
      idx      <- round((sub[[temp_col]] - global_min) / (global_max - global_min) * 99) + 1
      col_vals <- pal[pmax(1, pmin(100, idx))]
      
      local_min <- min(sub[[temp_col]])
      local_max <- max(sub[[temp_col]])
      y_pad     <- max((local_max - local_min) * 0.45, abs(local_min) * 0.20)
      ylim      <- c(local_min - y_pad, local_max + y_pad)
      
      plot(x, sub[[temp_col]],
           type = "n", xlim = c(0.5, n + 0.5), ylim = ylim,
           xaxt = "n", yaxt = "n", xlab = "", ylab = "",
           main = yr, font.main = 2, cex.main = 1.92)
      
      abline(h = pretty(ylim, n = 4), col = "grey88", lty = 1, lwd = 0.8)
      abline(h = 0, col = "grey60", lty = 2, lwd = 1)
      
      points(x, sub[[temp_col]],
             pch = 21, bg = col_vals,
             col = "white", cex = cex_vals, lwd = 0.8)
      
      text(x, sub[[temp_col]] + (local_max - local_min + y_pad) * 0.14,
           labels = paste0(round(sub[[temp_col]]), unit),
           cex = 1.14, col = "grey20", font = 2)
      
      axis(1, at = x, labels = sub$Country, las = 2,
           cex.axis = 1.14, tick = FALSE, line = 0)
      
      axis(2, at = pretty(ylim, n = 4),
           labels = paste0(pretty(ylim, n = 4), unit),
           las = 1, cex.axis = 1.08, col.axis = "grey40",
           tick = TRUE, col = "grey80")
      
      box(col = "grey80")
    }
    
    if (n_panels %% n_cols != 0) {
      for (k in seq_len(n_cols - n_panels %% n_cols)) plot.new()
    }
    
    mtext(paste0(title, "  [", min(chunk_years), "–", max(chunk_years), "]"),
          outer = TRUE, side = 3, line = 2.8, font = 2, cex = 1.44)
    mtext(paste0("Part ", i, " of ", length(chunks)),
          outer = TRUE, side = 3, line = 1.3, font = 3, cex = 1.14, col = "grey50")
    mtext(paste0("Annual temperature (", unit, ")"),
          outer = TRUE, side = 2, line = 1.2, cex = 1.20, col = "grey30")
    
    dev.off()
    cat("Saved:", file, "\n")
  }
}

# ── Hottest years ─────────────────────────────────────────────────────────────
plot_bubble_panel(
  data        = extreme_years,
  temp_col    = "Hottest",
  year_col    = "Year_Hot",
  file_prefix = "scatter_panel_hottest_years",
  title       = "Hottest annual temperature on record by country, grouped by peak year",
  ramp        = c("steelblue", "gold", "firebrick"),
  unit        = "°C"
)

# ── Coldest years ─────────────────────────────────────────────────────────────
plot_bubble_panel(
  data        = extreme_years,
  temp_col    = "Coldest",
  year_col    = "Year_Cold",
  file_prefix = "scatter_panel_coldest_years",
  title       = "Coldest annual temperature on record by country, grouped by peak cold year",
  ramp        = c("firebrick", "gold", "steelblue"),
  unit        = "°C"
)

# ============================================================
# Ejercicio 7: Realizar un bubble plot utilizando precipitacionsobre el año más seco y el más húmedo para cada país
# ============================================================

# ── Extreme precipitation years bubble panel — base R ─────────────────────────

plot_bubble_panel <- function(data, pre_col, year_col, file_prefix, title, ramp, unit) {
  
  years      <- sort(unique(data[[year_col]]))
  chunks     <- split(years, ceiling(seq_along(years) / 6))
  global_min <- min(data[[pre_col]])
  global_max <- max(data[[pre_col]])
  
  for (i in seq_along(chunks)) {
    
    chunk_years <- chunks[[i]]
    n_panels    <- length(chunk_years)
    n_cols      <- 3
    n_rows      <- ceiling(n_panels / n_cols)
    
    file <- paste0(file_prefix, "_part", i, ".png")
    png(file, width = 5000, height = ceiling(n_rows * 3500 / 2), res = 300)
    
    par(mfrow = c(n_rows, n_cols),
        mar   = c(7.2, 6.0, 4.8, 1.8),
        oma   = c(2.4, 3.6, 6.0, 1.2),
        bg    = "white")
    
    for (yr in chunk_years) {
      
      sub <- data[data[[year_col]] == yr, ]
      sub <- sub[order(sub[[pre_col]]), ]
      n   <- nrow(sub)
      x   <- seq_len(n)
      
      # Bubble sizes — inline, no helper function
      cex_vals <- 1.8 + (sub[[pre_col]] - global_min) / (global_max - global_min) * 7.2
      
      # Bubble colors — inline, no helper function
      pal      <- colorRampPalette(ramp)(100)
      idx      <- round((sub[[pre_col]] - global_min) / (global_max - global_min) * 99) + 1
      col_vals <- pal[pmax(1, pmin(100, idx))]
      
      # Local y range
      local_min <- min(sub[[pre_col]])
      local_max <- max(sub[[pre_col]])
      y_pad     <- max((local_max - local_min) * 0.45, local_max * 0.20)
      ylim      <- c(max(0, local_min - y_pad), local_max + y_pad)
      
      plot(x, sub[[pre_col]],
           type = "n", xlim = c(0.5, n + 0.5), ylim = ylim,
           xaxt = "n", yaxt = "n", xlab = "", ylab = "",
           main = yr, font.main = 2, cex.main = 1.92)
      
      abline(h = pretty(ylim, n = 4), col = "grey88", lty = 1, lwd = 0.8)
      
      points(x, sub[[pre_col]],
             pch = 21, bg = col_vals,
             col = "white", cex = cex_vals, lwd = 0.8)
      
      text(x, sub[[pre_col]] + (local_max - local_min + y_pad) * 0.14,
           labels = paste0(round(sub[[pre_col]]), unit),
           cex = 1.14, col = "grey20", font = 2)
      
      axis(1, at = x, labels = sub$Country, las = 2,
           cex.axis = 1.14, tick = FALSE, line = 0)
      
      axis(2, at = pretty(ylim, n = 4),
           labels = paste0(pretty(ylim, n = 4), unit),
           las = 1, cex.axis = 1.08, col.axis = "grey40",
           tick = TRUE, col = "grey80")
      
      box(col = "grey80")
    }
    
    if (n_panels %% n_cols != 0) {
      for (k in seq_len(n_cols - n_panels %% n_cols)) plot.new()
    }
    
    mtext(paste0(title, "  [", min(chunk_years), "–", max(chunk_years), "]"),
          outer = TRUE, side = 3, line = 2.8, font = 2, cex = 1.44)
    mtext(paste0("Part ", i, " of ", length(chunks)),
          outer = TRUE, side = 3, line = 1.3, font = 3, cex = 1.14, col = "grey50")
    mtext(paste0("Annual precipitation (", unit, ")"),
          outer = TRUE, side = 2, line = 1.2, cex = 1.20, col = "grey30")
    
    dev.off()
    cat("Saved:", file, "\n")
  }
}

# ── Wettest years ─────────────────────────────────────────────────────────────
plot_bubble_panel(
  data        = extreme_years_pre,
  pre_col    = "Wettest",
  year_col    = "Year_Wet",
  file_prefix = "scatter_panel_wettest_years",
  title       = "Highest annual precipitation on record by country, grouped by peak year",
  ramp        = c("lightyellow", "deepskyblue", "navy"),
  unit        = "mm"
)

# ── Driest years ──────────────────────────────────────────────────────────────
plot_bubble_panel(
  data        = extreme_years_pre,
  pre_col    = "Driest",
  year_col    = "Year_Dry",
  file_prefix = "scatter_panel_driest_years",
  title       = "Lowest annual precipitation on record by country, grouped by driest year",
  ramp        = c("navy", "deepskyblue", "lightyellow"),
  unit        = "mm"
)

# ============================================================
# 5. Estimar la temperatura estacional para 7 países
# ============================================================
# DJF (December, January, February): Represents the Winter season.
# MAM (March, April, May): Represents the Spring season.
# JJA (June, July, August): Represents the Summer season.
# SON (September, October, November): Represents the Autumn (or Fall) 



# ============================================================
# Ejercicio 8: Estimar la precipitación estacional para 7 países
# ============================================================
# DJF (December, January, February): Represents the Winter season.
# MAM (March, April, May): Represents the Spring season.
# JJA (June, July, August): Represents the Summer season.
# SON (September, October, November): Represents the Autumn (or Fall) 

# Países de diferentes zonas climáticas
selected <- c("Germany", "Mexico", "Brazil", "India", "China", "Indonesia", "South_Africa")
seasons  <- c("DJF", "MAM", "JJA", "SON")

par(mfrow = c(2, 2))

for (s in seasons) {
  seasonal_means <- sapply(selected, function(country) {
    key <- paste0(country, "_pre")
    if (key %in% names(climate_data)) {
      mean(climate_data[[key]][[s]], na.rm = TRUE)
    } else {
      NA
    }
  })
  
  barplot(seasonal_means,
          names.arg = selected,
          col = c("#B0E0E6", "#FFF68F", "#9ACD32", "#191970",
                  "#C71585", "#48D1CC", "#F4A460"),
          main      = paste("Precipitación media -", s),
          ylab      = "Precipitación (mm)",
          ylim      = c(-10, 35))
}

par(mfrow = c(1, 1))


# ============================================================
# 6. Calcuar la Precipitación vs Temperatura
# ============================================================

# Unir precipitación y temperatura anual para México
pre_mx <- climate_data[["Mexico_pre"]][, c("Year", "ANN")]
tmp_mx <- climate_data[["Mexico_tmp"]][, c("Year", "ANN")]

colnames(pre_mx) <- c("Year", "Precipitation")
colnames(tmp_mx) <- c("Year", "Temperature")

merged_mx <- merge(pre_mx, tmp_mx, by = "Year")

# Scatterplot
plot(merged_mx$Temperature, merged_mx$Precipitation,
     pch  = 19,
     col  = c("tomato", "slateblue"),
     main = "Precipitación vs Temperatura anual - México",
     xlab = "Temperatura (°C)",
     ylab = "Precipitación (mm)")

# Línea de tendencia
abline(lm(Precipitation ~ Temperature, data = merged_mx),
       col = "red", lwd = 2, lty = 2)

# Correlación
cor_val <- cor(merged_mx$Temperature, merged_mx$Precipitation, use = "complete.obs")
legend("topright",
       legend = paste("r =", round(cor_val, 3)),
       bty    = "n")

# ============================================================
# Ejercicio 9. Calcuar la Precipitación vs Temperatura para otro país
# ============================================================
