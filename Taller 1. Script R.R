library(tidyr)
library(dplyr)
library(ggplot2)


# Definir la ruta donde se creará el directorio
ruta_directorio <- "C:\\Users\\Marcos Gonzalez\\OneDrive\\Escritorio\\UR\\Trabajos\\Sexto Semestre\\Haciendo Economía\\Taller 1\\Outputs"


# Crear el directorio si no existe
if (!dir.exists(ruta_directorio)) {
  dir.create(ruta_directorio, recursive = TRUE)
}

# Verificar que el directorio se creó correctamente
list.dirs("C:\\Users\\Marcos Gonzalez\\OneDrive\\Escritorio\\UR\\Trabajos\\Sexto Semestre\\Haciendo Economía\\Taller 1\\Outputs")


#PREGUNTA 411. USO DEL INTERNET-------------------------------------------------
# Cargar librerías necesarias
library(ggplot2)
library(dplyr)

# Eliminar valores NA
ValBinarios <- na.omit(TenderosFU03_Publica$uso_internet)

# Convertir en factor
ValBinarios <- factor(ValBinarios, levels = c(0, 1), labels = c("No usa Internet", "Usa Internet"))

# Crear tabla de frecuencias como data frame
tabla_frecuencias <- as.data.frame(table(ValBinarios))

# Gráfico con ggplot2
ggplot(tabla_frecuencias, aes(x = ValBinarios, y = Freq, fill = ValBinarios)) +
  geom_bar(stat = "identity", width = 0.4) +
  geom_text(aes(label = Freq), vjust = -0.5, size = 3) +  # Añadir etiquetas con los valores
  scale_fill_manual(values = c("blue", "red")) +  # Definir colores
  labs(title = "Distribución del Uso de Internet en Tiendas de Barrio",
       x = "Uso de Internet",
       y = "Frecuencia") +
  theme_minimal()
ruta_directorio <- "C:\\Users\\Marcos Gonzalez\\OneDrive\\Escritorio\\UR\\Trabajos\\Sexto Semestre\\Haciendo Economía\\Taller 1\\Outputs\\grafico_uso_internet.png"
ggsave(ruta_directorio, width = 8, height = 6, dpi = 300)
  #PREGUNTA 805. TIPO DE MEDIOS DE PAGO-------------------------------------------

Data <- TenderosFU03_Publica %>% 
  select(medios_pago__3,medios_pago__2,medios_pago__1,medios_pago__10,medios_pago__8,medios_pago__8,medios_pago__5,medios_pago__4,medios_pago__9,medios_pago_otro)

frecuencia_pagos <- Data %>%
  summarise(
    Efectivo = sum(medios_pago__3 == 0, na.rm = TRUE),
    Tarjeta_Debito = sum(medios_pago__2 == 0, na.rm = TRUE),
    Tarjeta_Credito = sum(medios_pago__1 == 0, na.rm = TRUE),
    Billeteras_Digitales = sum(medios_pago__10 == 0, na.rm = TRUE),
    Pagos_Link = sum(medios_pago__8 == 0, na.rm = TRUE),
    Giros_No_Bancarios = sum(medios_pago__5 == 0, na.rm = TRUE),
    Transferencias = sum(medios_pago__4 == 0, na.rm = TRUE),
    Otro = sum(medios_pago__9 == 0, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Medio_Pago", values_to = "Frecuencia")

# Crear gráfico de barras
ggplot(frecuencia_pagos, aes(x = reorder(Medio_Pago, Frecuencia), y = Frecuencia, fill = Medio_Pago)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Frecuencia de Uso de Medios de Pago en Tiendas de Barrio",
    x = "Medio de Pago",
    y = "Número de Tiendas",
    fill = "Medio de Pago"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")
