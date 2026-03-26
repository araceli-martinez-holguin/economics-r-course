# ==============================================================================
#
#   PRÁCTICA 2
#   Consejo Asesor del Secretario de Economía
#
#   CONTEXTO: El T-MEC (USMCA) será renegociado en 2026. El equipo debe
#             identificar qué productos agrícolas de exportación proteger,
#             cuáles impulsar y cuáles presentan vulnerabilidades.
#
#   DATOS:    Principales cultivos de exportación 2003–2024 (SIAP)
#             Fuente: https://nube.agricultura.gob.mx/cierre_agricola/
#
#   LIBRERÍAS: tidyr, dplyr, readxl, ggplot2, scales, RColorBrewer, writexl
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN INICIAL
# ------------------------------------------------------------------------------

rm(list = ls(all = TRUE))
gc()

install.packages(c("tidyr", "dplyr", "readxl", "ggplot2",
                   "scales", "RColorBrewer", "writexl",
                   "gganimate", "gifski"))

install.packages(c("gganimate", "gifski", "devtools"))

devtools::install_github("ricardo-bion/ggradar")

library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(writexl)
library(ggradar)
library(gganimate)
library(gifski)

# Directorio de Trabajo
setwd("~/Documents/Cursos-R/Laboratorio-2")

# Cargamos base de datos
data <- read_xlsx("cultivos.xlsx")

# Damos un vistazo a la base
glimpse(data)

# Columnas disponibles
colnames(data)
#   origen, Anio, Idestado, Nomestado, Idddr, Nomddr, Idcader, Nomcader,
#   Idmunicipio, Nommunicipio, Idciclo, Nomcicloproductivo, Idmodalidad,
#   Nommodalidad, Idunidadmedida, Nomunidad, Idcultivo, Nomcultivo,
#   Sembrada, Cosechada, Siniestrada, Volumenproduccion, Rendimiento,
#   Precio, Valorproduccion

# ¿Qué cultivos incluye la base?
sort(unique(data$Nomcultivo))

# ==============================================================================
# SECCIÓN 1 — DIAGNÓSTICO PRODUCTIVO
#
# Brief: "Antes de negociar necesitamos saber qué tan grande es cada sector
#         y cómo ha evolucionado en dos décadas. 
#         Sin ese diagnóstico base,vamos ciegos a la mesa."
# ==============================================================================


# ------------------------------------------------------------------------------
# 1A. Peso relativo de cada cultivo (2003–2024)
#
# ¿Cuál es la participación de cada cultivo en el total exportable?
# Funciones: group_by(), summarise(), mutate(), arrange(), sum(), round()
# ------------------------------------------------------------------------------

participacion <- data %>%
  # Paso 1: Agrupar por cultivo y sumar las variables de interés
  group_by(Nomcultivo) %>%
  summarise(
    produccion_acumulada = sum(Volumenproduccion, na.rm = TRUE),
    valor_acumulado      = sum(Valorproduccion,   na.rm = TRUE),
    superficie_acumulada = sum(Sembrada,           na.rm = TRUE)
  ) %>%
  # Paso 2: Calcular participación de cada cultivo en el total
  # Nota: mutate() opera sobre el dataframe ya resumido
  mutate(
    participacion_prod  = round(produccion_acumulada / sum(produccion_acumulada) * 100, 2),
    participacion_valor = round(valor_acumulado      / sum(valor_acumulado)      * 100, 2),
    participacion_sem   = round(superficie_acumulada      / sum(superficie_acumulada)  *100, 2)
  ) %>%
  arrange(desc(participacion_valor))

View(participacion)

# Gráfica: participación por valor
# Usamos reorder() para ordenar las barras de mayor a menor
p_part_valor <- ggplot(participacion,
                       aes(x = reorder(Nomcultivo, participacion_valor),
                           y = participacion_valor,
                           fill = participacion_valor)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(participacion_valor, "%")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_gradient(low = "#b7e4c7", high = "#1b4332") +
  scale_y_continuous(limits = c(0, max(participacion$participacion_valor) * 1.15)) +
  labs(
    title    = "Participación en el Valor Total de la Producción Exportable",
    subtitle = "México 2003–2024 | Acumulado",
    x        = NULL,
    y        = "Participación (%)"
  ) +
  theme_minimal(base_size = 12)

p_part_valor
ggsave("participacion_valor.png", p_part_valor, dpi = 300, width = 12, height = 10)

# Gráfica: participación por volumen
p_part_prod <- ggplot(participacion,
                      aes(x = reorder(Nomcultivo, participacion_prod),
                          y = participacion_prod,
                          fill = participacion_prod)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(participacion_prod, "%")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_gradient(low = "indianred1", high = "indianred4") +
  scale_y_continuous(limits = c(0, max(participacion$participacion_prod) * 1.15)) +
  labs(
    title    = "Participación en el Volumen Total de la Producción Exportable",
    subtitle = "México 2003–2024 | Acumulado",
    x        = NULL,
    y        = "Participación (%)"
  ) +
  theme_minimal(base_size = 12)

p_part_prod
ggsave("participacion_prod.png", p_part_prod, dpi = 300, width = 12, height = 10)

# Gráfica: participación por superficie sembrada
p_part_sem <- ggplot(participacion,
                     aes(x = reorder(Nomcultivo, participacion_sem),
                         y = participacion_sem,
                         fill = participacion_sem)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(participacion_sem, "%")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_gradient(low = "#1E90FF", high = "#104E8B") +
  scale_y_continuous(limits = c(0, max(participacion$participacion_sem) * 1.15)) +
  labs(
    title    = "Participación de la Superficie Sembrada en la Producción Exportable",
    subtitle = "México 2003–2024 | Acumulado",
    x        = NULL,
    y        = "Participación (%)"
  ) +
  theme_minimal(base_size = 12)

p_part_sem
ggsave("participacion_sem.png", p_part_sem, dpi = 300, width = 12, height = 10)


# Interpretación: ¿Coinciden los líderes en valor con los líderes en volumen?
# Cuando no coinciden, algunos cultivos tienen precios más altos por tonelada,
# lo que los hace estratégicamente más interesantes para la negociación.


# ------------------------------------------------------------------------------
# 1B. Cobertura territorial
#
# ¿En cuántos municipios y estados se produce cada cultivo cada año?
# Un cultivo presente en muchos municipios tiene base productiva diversificada.
# Funciones: n_distinct(), pivot_wider(), arrange()
# ------------------------------------------------------------------------------

n_cultivos <- n_distinct(data$Nomcultivo)

# --- Municipios ---
municipios_cultivo <- data %>%
  group_by(Anio, Nomcultivo) %>%
  summarise(
    num_municipios = n_distinct(Idmunicipio),  # IDs únicos, más robusto que nombres
    .groups = "drop"
  ) %>%
  arrange(Nomcultivo, Anio)

# Formato ancho: filas = cultivos, columnas = años
municipios_wide <- municipios_cultivo %>%
  pivot_wider(names_from = Anio, values_from = num_municipios) %>%
  arrange(Nomcultivo)

View(municipios_wide)

# Gráfica de lineas
p_municipios <- ggplot(municipios_cultivo,
                       aes(x = Anio, y = num_municipios,
                           color = Nomcultivo, group = Nomcultivo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(2003, 2024, by = 1)) +
  scale_color_manual(values = colorRampPalette(brewer.pal(12, "Set3"))(n_cultivos)) +
  labs(
    title    = "Municipios Productores por Cultivo y Año",
    subtitle = "México 2003–2024",
    x = "", y = "Número de municipios", color = "Cultivo"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.text     = element_text(size = 9))

p_municipios
ggsave("municipios_por_cultivo.png", p_municipios, dpi = 300, width = 14, height = 8)

# Gráfica de barras traslapadas
municipios_cultivo <- municipios_cultivo %>%
  group_by(Anio) %>%
  mutate(pct = num_municipios / sum(num_municipios)) %>%
  arrange(Anio, pct) %>%
  ungroup() %>%
  mutate(Nomcultivo = reorder(Nomcultivo, pct))

pb_municipios <- ggplot(municipios_cultivo,
                        aes(x = Anio, y = pct, fill = Nomcultivo)) +
  geom_bar(stat = "identity", position = position_fill(reverse = FALSE)) +
  geom_text(aes(label = ifelse(pct >= 0.02, scales::percent(pct, accuracy = 1), "")),
            position = position_fill(vjust = 0.5),
            size = 2.5, color = "black") +
  scale_x_continuous(breaks = seq(2003, 2024, by = 1)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1.10),
                     expand = c(0, 0)) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Set3"))(n_cultivos)) +
  labs(
    title    = "Municipios Productores por Cultivo y Año",
    subtitle = "México 2003–2024",
    x = "", y = "Porcentaje de municipios", fill = "Cultivo"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.text     = element_text(size = 9))

pb_municipios
ggsave("pb_municipio.png", pb_municipios, dpi = 300, width = 14, height = 8)


# --- 1. Prepare data ---
radar_data <- municipios_cultivo %>%
  group_by(Anio) %>%
  mutate(pct = num_municipios / sum(num_municipios)) %>%
  ungroup() %>%
  select(Anio, Nomcultivo, pct) %>%
  pivot_wider(names_from = Nomcultivo, values_from = pct, values_fill = 0) %>%
  arrange(Anio) %>%
  mutate(group = as.character(Anio)) %>%
  select(group, everything(), -Anio)

# --- 2. Colors per year ---
n_anios <- nrow(radar_data)
colores <- colorRampPalette(brewer.pal(9, "Set1"))(n_anios)

# --- 3. Build one radar per year and save frames ---
anios <- sort(unique(municipios_cultivo$Anio))

for (i in seq_along(anios)) {
  
  anio_i  <- anios[i]
  datos_i <- radar_data %>% filter(group == as.character(anio_i))
  
  p <- ggradar(
    datos_i,
    values.radar     = c("0%", "25%", "50%"),
    grid.min         = 0,
    grid.mid         = 0.25,
    grid.max         = 0.50,
    group.colours    = colores[i],
    group.line.width = 1.5,
    group.point.size = 4,
    axis.label.size  = 3,
    grid.label.size  = 4,
    legend.position  = "none",
    plot.title       = paste0("Municipios Productores por Cultivo\nMéxico — ", anio_i)
  ) +
    theme(
      plot.title      = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  ggsave(
    filename = sprintf("frame_%02d.png", i),
    plot     = p,
    width    = 10, height = 8, dpi = 150
  )
  
  message(sprintf("Frame %d/%d — %d", i, length(anios), anio_i))
}

# --- 4. Assemble GIF with gifski ---
frames <- sprintf("frame_%02d.png", seq_along(anios))

gifski(
  frames,
  gif_file = "radar_municipios.gif",
  width    = 1500,
  height   = 1200,
  delay    = 0.8
)

# --- 5. Clean up frames ---
file.remove(frames)

message("✓ GIF saved as radar_municipios.gif")

