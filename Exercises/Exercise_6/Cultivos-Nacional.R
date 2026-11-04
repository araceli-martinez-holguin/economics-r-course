# Practica: Principales cultivos de México de 1980 a 2002
# Nivel estatal

# Install packages
install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "scales",
                   "sf", "RColorBrewer", "gganimate", "gifski", 
                   "stringi", "ggrepel"))


# Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(sf)
library(RColorBrewer)
library(gifski)
library(stringr)
library(ggrepel)

# Set working directory
setwd("~/Rfiles/Laboratorio-cultivos")

# Leer el archivo Excel
data <- read_csv("cultivos_nacional_final.csv")

str(data)

# Cuantos cultivos producimos
unique(data$Nomcultivo)
# Hay 313 cultivos

#' Sección 1 — Diagnóstico Productivo
#' Preguntas:
#' 1A. ¿Cuál es el volumen de producción para cada cultivo?  
#' 1B. ¿Cuánto aporta cada cultivo en valor?  
#' 1C. ¿Cuánto ocupa de superficie cada cultivo?
#' 1D. ¿Qué rendimiento tiene cada cultivo?
#' 1E. ¿Qué precio tiene cada cultivo?


# Dataframe con participaciones acumuladas por cultivo
participacion <- data %>%
  group_by(Nomcultivo) %>%
  summarise(
    volumen_acumulado  = sum(Volumenproduccion, na.rm = TRUE),
    valor_acumulado    = sum(Valorproduccion,   na.rm = TRUE),
    sembrada_acumulada = sum(Sembrada,          na.rm = TRUE)
  ) %>%
  mutate(
    participacion_volumen  = round(volumen_acumulado  / sum(volumen_acumulado)  * 100, 2),
    participacion_valor    = round(valor_acumulado    / sum(valor_acumulado)    * 100, 2),
    participacion_sembrada = round(sembrada_acumulada / sum(sembrada_acumulada) * 100, 2)
  ) %>%
  arrange(desc(participacion_valor))

# Reducimos a 20 cultivos
participacion_1 <- as.data.frame(participacion[1:20, ])


## Gráfica 1A: Volumen de Producción
participacion_volumen <- ggplot(participacion_1,
                                aes(x = reorder(Nomcultivo, participacion_volumen),
                                    y = participacion_volumen,
                                    fill = participacion_volumen)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(participacion_volumen, "%")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_gradient(low = "#b7e4c7", high = "#1b4332") +
  scale_y_continuous(limits = c(0, max(participacion_1$participacion_volumen) * 1.15)) +
  labs(
    title    = "Participación de Cada Cultivo en el Volumen Total",
    subtitle = "México 1980--2002 | Acumulado",
    x        = NULL,
    y        = "Participación (%)"
  ) +
  theme_minimal(base_size = 12)

print(participacion_volumen)
ggsave("participacion_volumen.png", participacion_volumen, dpi = 300, width = 12, height = 10)


## Gráfica 1B: Valor de Producción
participacion_valor <- ggplot(participacion_1,
                              aes(x = reorder(Nomcultivo, participacion_valor),
                                  y = participacion_valor,
                                  fill = participacion_valor)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(participacion_valor, "%")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_gradient(low = "indianred1", high = "indianred4") +
  scale_y_continuous(limits = c(0, max(participacion_1$participacion_valor) * 1.15)) +
  labs(
    title    = "Participación de Cada Cultivo en el Valor de Producción Total",
    subtitle = "México 1980--2002 | Acumulado",
    x        = NULL,
    y        = "Participación (%)"
  ) +
  theme_minimal(base_size = 12)

print(participacion_valor)
ggsave("participacion_valor.png", participacion_valor, dpi = 300, width = 12, height = 10)

## Gráfica 1C: Superficie Sembrada
participacion_sembrada <- ggplot(participacion_1,
                                 aes(x = reorder(Nomcultivo, participacion_sembrada),
                                     y = participacion_sembrada,
                                     fill = participacion_sembrada)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(participacion_sembrada, "%")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_gradient(low = "hotpink", high = "hotpink4") +
  scale_y_continuous(limits = c(0, max(participacion_1$participacion_sembrada) * 1.15)) +
  labs(
    title    = "Participación de Cada Cultivo en el Total de la Superficie Sembrada",
    subtitle = "México 1980--2002 | Acumulado",
    x        = NULL,
    y        = "Participación (%)"
  ) +
  theme_minimal(base_size = 12)

print(participacion_sembrada)
ggsave("participacion_sembrada.png", participacion_sembrada, dpi = 300, width = 12, height = 10)

#' Ejercicio 1
#' Ejercicio 1D: ¿Qué rendimiento tiene cada cultivo?   
#' Ejercicio 1E: ¿Qué precio tiene cada cultivo? 


#' Sección 2 — Estados por Cultivo
#' Preguntas:  
#' 2A. ¿Cuántos estados producen cada cultivo?
# Número de estados por cultivo, solo 1980 y 2002
# Top 20 cultivos de 2002 por VALOR de producción
top20_2002 <- data %>%
  filter(Anio == 2002) %>%
  group_by(Nomcultivo) %>%
  summarise(valor = sum(Valorproduccion, na.rm = TRUE),   # <- columna de volumen
            .groups = "drop") %>%
  slice_max(valor, n = 20, with_ties = FALSE) %>%
  pull(Nomcultivo)

# Número de estados productores de esos cultivos, 1980 y 2002
estados_cultivo <- data %>%
  filter(Anio %in% c(1980, 2002),
         Nomcultivo %in% top20_2002,
         Valorproduccion > 0) %>%                           # <- columna de volumen
  group_by(Anio, Nomcultivo) %>%
  summarise(
    num_estados = n_distinct(Nomestado),
    .groups = "drop"
  )

# Formato ancho, ordenado por 2002
estados_wide <- estados_cultivo %>%
  pivot_wider(
    names_from  = Anio,
    values_from = num_estados
  ) %>%
  mutate(cambio = `2002` - `1980`) %>%
  arrange(desc(`2002`))

estados_wide

## Gráfica 2A: Estados productores de los 20 cultivos de mayor volumen (2002)
estados_plot <- ggplot(estados_cultivo,
                       aes(x = Anio, y = num_estados,
                           color = Nomcultivo, group = Nomcultivo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_text_repel(
    aes(label = num_estados),
    size = 3,
    show.legend = FALSE,
    box.padding = 0.25,
    max.overlaps = Inf,
    seed = 123
  ) +
  scale_x_continuous(breaks = c(1980, 2002),
                     expand = expansion(mult = 0.15)) +
  scale_color_manual(values = colorRampPalette(
    brewer.pal(12, "Set3"))(n_distinct(estados_cultivo$Nomcultivo))) +
  labs(
    title    = "Estados Productores por Cultivo",
    subtitle = "Top 20 cultivos de 2002 por valor de producción, México, 1980 vs 2002",
    x        = "Año",
    y        = "Número de estados",
    color    = "Cultivo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.text     = element_text(size = 9)
  )

print(estados_plot)
ggsave("estados_por_cultivo.png", estados_plot, dpi = 300, width = 10, height = 8)

## Gráfica 2A2: Radar — Participación de cada cultivo en el valor de la producción (GIF animado)

# 1. Valor por cultivo y año, y su participación en el total del año
participacion <- data %>%
  group_by(Anio, Nomcultivo) %>%
  summarise(valor = sum(Valorproduccion, na.rm = TRUE), .groups = "drop") %>%
  group_by(Anio) %>%
  mutate(pct = valor / sum(valor)) %>%
  ungroup()

# 2. Top 20 cultivos por valor en cada año
top20_por_anio <- participacion %>%
  group_by(Anio) %>%
  slice_max(valor, n = 20, with_ties = FALSE) %>%
  arrange(Anio, desc(valor)) %>%
  ungroup()

# 3. Anillos: cada 2.5% hasta 15%
grid_max <- 0.15
anillos  <- seq(0, grid_max, by = 0.025)

# 4. Paleta: un color por año
anios   <- sort(unique(top20_por_anio$Anio))
colores <- colorRampPalette(brewer.pal(12, "Set3"))(length(anios))

# 5. Crear los cuadros
dir.create("frames_radar", showWarnings = FALSE)
unlink(list.files("frames_radar", full.names = TRUE))

for (i in seq_along(anios)) {
  anio_i <- anios[i]
  
  # Formato LARGO: una fila por cultivo (NO usar pivot_wider)
  datos_i <- top20_por_anio %>%
    filter(Anio == anio_i) %>%
    mutate(
      nombre   = as.character(Nomcultivo),
      nombre   = ifelse(nchar(nombre) > 18,
                        paste0(substr(nombre, 1, 15), "..."), nombre),
      nombre   = make.unique(nombre),
      Nomcultivo = factor(nombre, levels = nombre),
      pct_plot = pmin(pct, grid_max)
    )
  
  p <- ggplot(datos_i, aes(x = Nomcultivo, y = pct_plot, group = 1)) +
    geom_polygon(fill = colores[i], color = colores[i], alpha = 0.35, linewidth = 1.2) +
    geom_point(color = colores[i], size = 3) +
    scale_y_continuous(limits = c(0, grid_max),
                       breaks = anillos,
                       expand = c(0, 0)) +
    annotate("text", x = 0.5, y = anillos[-1],
             label = percent(anillos[-1], accuracy = 0.1),
             size = 2.6, color = "grey30", hjust = 1.1, vjust = -0.3) +
    coord_polar() +
    labs(title = paste0("Participación de los 20 cultivos de mayor valor\n",
                        "en el valor de la producción — México, ", anio_i),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_text(hjust = 0.5, size = 10),
      axis.text.x        = element_text(size = 8),
      axis.text.y        = element_blank(),
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.3),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3)
    )
  
  ggsave(
    filename = file.path("frames_radar", sprintf("frame_%02d.png", i)),
    plot = p, width = 10, height = 8, dpi = 300
  )
}

# 6. Generar GIF
frames <- list.files("frames_radar", full.names = TRUE, pattern = "frame_.*png")
gifski(
  png_files = frames,
  gif_file  = "radar_participacion_valor.gif",
  width     = 1500,
  height    = 1200,
  delay     = 1.5
)

### Tasa de crecimiento ###
# 1. Años de comparación
anio_ini <- 1980
anio_fin <- 2002

# 2. Top 20 cultivos por VALOR de la producción (año final)
top20_valor <- data %>%
  filter(Anio == anio_fin) %>%
  group_by(Nomcultivo) %>%
  summarise(valor = sum(Valorproduccion, na.rm = TRUE), .groups = "drop") %>%
  slice_max(valor, n = 20, with_ties = FALSE)

# 3. Volumen de esos cultivos en ambos años y tasa de crecimiento logarítmica
crec_log <- data %>%
  filter(Anio %in% c(anio_ini, anio_fin),
         Nomcultivo %in% top20_valor$Nomcultivo) %>%
  group_by(Nomcultivo, Anio) %>%
  summarise(volumen = sum(Volumenproduccion, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Anio, values_from = volumen, names_prefix = "anio_") %>%
  rename(inicial = !!paste0("anio_", anio_ini),
         final   = !!paste0("anio_", anio_fin)) %>%
  mutate(
    crec_log      = log(final / inicial) * 100,                      # tasa logarítmica total (%)
    crec_log_anual = crec_log / (anio_fin - anio_ini),               # tasa continua anual (%)
    signo = if_else(crec_log >= 0, "Crecimiento", "Disminución")
  ) %>%
  filter(is.finite(crec_log))

# Cultivos del top 20 que se excluyeron por no tener volumen en alguno de los años
setdiff(top20_valor$Nomcultivo, crec_log$Nomcultivo)

# 4. Gráfica
graf_log <- ggplot(crec_log,
                   aes(x = reorder(Nomcultivo, crec_log),
                       y = crec_log, fill = signo)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, color = "gray30", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.1f", crec_log),
                hjust = if_else(crec_log >= 0, -0.1, 1.1)),
            size = 3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.15))) +
  scale_fill_manual(values = c("Crecimiento" = "#238B45",
                               "Disminución" = "#CB181D"),
                    name = NULL) +
  labs(
    title    = "Crecimiento Logarítmico del Volumen de Producción",
    subtitle = paste0("20 principales cultivos por valor de la producción en ",
                      anio_fin, " — México, ", anio_ini, " a ", anio_fin),
    x = NULL,
    y = "Tasa de crecimiento logarítmica: ln(Vol. final / Vol. inicial) × 100",
    caption = "Cultivos elegidos por valor de la producción; la tasa se calcula sobre el volumen."
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(color = "gray40", hjust = 0.5),
        legend.position = "top",
        panel.grid.major.y = element_blank())

ggsave("crecimiento_log_volumen_top20_valor.png", graf_log,
       dpi = 300, width = 12, height = 9)
graf_log

# 5. Tabla con ambas tasas
crec_log %>%
  arrange(desc(crec_log)) %>%
  select(Nomcultivo, inicial, final, crec_log, crec_log_anual)

