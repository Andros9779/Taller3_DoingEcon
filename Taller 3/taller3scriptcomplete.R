# -----------------------------------------------------
# Taller 3: Empirical Project 1 – Measuring Climate Change
# Basado en Doing Economics
# Rango de tiempo: 1880 a 2025 (DJF: December-January-February, con NA en la primera y última entrada)
# -----------------------------------------------------

# ============================
# Configuración Inicial
# ============================
setwd("/Users/jdds/Documents/Doing Econ/Taller 3")
library(tidyverse)  # Para read_csv, dplyr, ggplot2, etc.
library(mosaic)     # Para funciones de resumen
library(readxl)     # Para leer archivos Excel (si es necesario en otras partes)

# ============================
# PART 1.1: The behaviour of average surface temperature over time
# ============================

# Importar el dataset CSV (NorthenHemis.csv)
tempdata <- read.csv("NorthenHemis.csv", skip = 1, na.strings = "***")
# Filtrar para incluir los años 1880 a 2025
tempdata <- subset(tempdata, Year >= 1880 & Year <= 2025)
cat("Número de filas tras el filtrado (1880-2025):", nrow(tempdata), "\n")
# Se esperan 2025 - 1880 + 1 = 146 filas

# Visualizar la estructura y primeras filas
head(tempdata)
str(tempdata)

# Convertir las variables mensuales a series de tiempo (frecuencia 1)
tempdata$Jan <- ts(tempdata$Jan, start = 1880, end = 2025, frequency = 1)
tempdata$DJF <- ts(tempdata$DJF, start = 1880, end = 2025, frequency = 1)  # Nota: DJF tiene NA en el primer y último año
tempdata$MAM <- ts(tempdata$MAM, start = 1880, end = 2025, frequency = 1)
tempdata$JJA <- ts(tempdata$JJA, start = 1880, end = 2025, frequency = 1)
tempdata$SON <- ts(tempdata$SON, start = 1880, end = 2025, frequency = 1)
tempdata$`J.D` <- ts(tempdata$`J.D`, start = 1880, end = 2025, frequency = 1)

# Gráfico: Anomalía en enero
plot(tempdata$Jan, type = "l", col = "blue", lwd = 2,
     ylab = "Temperature anomalies (°C)", xlab = "Year")
title("Average temperature anomaly in January (1880-2025)")
abline(h = 0, col = "darkorange2", lwd = 2)
text(2000, -0.1, "1951-1980 average")

# Gráfico: Anomalía anual (variable J.D)
plot(tempdata$`J.D`, type = "l", col = "blue", lwd = 2,
     ylab = "Temperature anomalies (°C)", xlab = "Year")
title("Average annual temperature anomaly (J.D) (1880-2025)")
abline(h = 0, col = "darkorange2", lwd = 2)

# ============================
# PART 1.2: Variation in temperature over time
# ============================
# Crear la variable Period: tres grupos (1921-1950, 1951-1980, 1981-2010)
tempdata$Period <- factor(NA, levels = c("1921-1950", "1951-1980", "1981-2010"), ordered = TRUE)
tempdata$Period[(tempdata$Year > 1920) & (tempdata$Year < 1951)] <- "1921-1950"
tempdata$Period[(tempdata$Year > 1950) & (tempdata$Year < 1981)] <- "1951-1980"
tempdata$Period[(tempdata$Year > 1980) & (tempdata$Year < 2011)] <- "1981-2010"

# Combinar datos de verano (suponiendo que existen Jun, Jul, Aug)
temp_summer <- c(tempdata$Jun, tempdata$Jul, tempdata$Aug)
temp_Period <- c(tempdata$Period, tempdata$Period, tempdata$Period)
temp_Period <- factor(temp_Period, levels = levels(tempdata$Period))

# Histograma para anomalías de verano en el periodo 1951-1980
hist_info <- hist(temp_summer[temp_Period == "1951-1980"],
                  breaks = seq(-0.5, 1.3, 0.1), plot = FALSE)
print(hist_info)

histogram(~ temp_summer | temp_Period, type = "count",
          breaks = seq(-0.5, 1.3, 0.10),
          main = "Histogram of Summer Temperature Anomalies",
          xlab = "Temperature anomaly (°C)")

# Calcular percentiles (30th y 70th) para 1951-1980 usando todos los meses (columnas 2 a 13)
temp_all_months_51_80 <- subset(tempdata, Year >= 1951 & Year <= 1980)
temp_51to80 <- unlist(temp_all_months_51_80[, 2:13])
perc <- quantile(temp_51to80, c(0.3, 0.7))
p30 <- perc[1]
p70 <- perc[2]
cat("30th percentile (cold threshold):", p30, "\n")
cat("70th percentile (hot threshold):", p70, "\n")

# Calcular proporciones para 1981-2010
temp_all_months_81_10 <- subset(tempdata, Year >= 1981 & Year <= 2010)
temp_81to10 <- unlist(temp_all_months_81_10[, 2:13])
prop_smaller <- mean(temp_81to10 < -0.1, na.rm = TRUE)
prop_larger <- mean(temp_81to10 > 0.11, na.rm = TRUE)
cat("Proportion < -0.1 (1981-2010):", prop_smaller, "\n")
cat("Proportion > 0.11 (1981-2010):", prop_larger, "\n")

# ============================
# PART 1.7: Mean and variance of DJF using temperature CSV data
# ============================
# Para el análisis de DJF, usaremos la variable DJF de tempdata (ya es una serie de tiempo).
# Como DJF tiene NA en la primera y última entrada, filtramos esos casos.

temp_DJF_df <- data.frame(Year = tempdata$Year,
                          DJF = as.numeric(tempdata$DJF),
                          Period = tempdata$Period)
temp_DJF_df <- temp_DJF_df[!is.na(temp_DJF_df$DJF), ]
cat("Análisis de DJF (número de observaciones con datos):", nrow(temp_DJF_df), "\n")

cat("Mean of DJF by Period (CSV data):\n")
mean_DJF_temp <- tapply(temp_DJF_df$DJF, temp_DJF_df$Period, mean)
print(mean_DJF_temp)

cat("Variance of DJF by Period (CSV data):\n")
var_DJF_temp <- tapply(temp_DJF_df$DJF, temp_DJF_df$Period, var)
print(var_DJF_temp)

# Aquí se pueden incluir interpretaciones adicionales de los resultados.

# ============================
# PART 1.3: Carbon emissions and the environment
# ============================
# Importar datos de CO2 (archivo "1_CO2 data.csv")
CO2data <- read.csv("1_CO2 data.csv")
# Seleccionar datos de junio
CO2data_june <- CO2data[CO2data$Month == 6, ]
names(CO2data_june)[1] <- "Year"

# Unir la base de temperatura (tempdata) con los datos de CO2 por "Year"
tempCO2data <- merge(tempdata, CO2data_june, by = "Year", all.x = TRUE)
head(tempCO2data[, c("Year", "Jun", "Trend")])

# Scatterplot: CO2 Trend vs. anomalías de temperatura en junio
plot(tempCO2data$Jun, tempCO2data$Trend, pch = 16,
     xlab = "June temperature anomalies",
     ylab = "CO2 Trend",
     main = "Scatterplot: CO2 Trend vs. June Temperature Anomalies")
cor_val <- cor(tempCO2data$Jun, tempCO2data$Trend, use = "complete.obs")
cat("Correlation coefficient (June anomalies vs. CO2 Trend):", cor_val, "\n")

# Convertir las series de CO2 a formato de tiempo (1958-2017)
tempCO2data$Jun <- ts(tempCO2data$Jun, start = 1958, end = 2017, frequency = 1)
tempCO2data$Trend <- ts(tempCO2data$Trend, start = 1958, end = 2017, frequency = 1)

# Gráfico con dos ejes verticales para comparar anomalías de temperatura en junio y CO2 Trend
plot(tempCO2data$Jun, type = "l", col = "blue", lwd = 2,
     ylab = "June Temperature Anomalies", xlab = "Year",
     ylim = range(tempCO2data$Jun, na.rm = TRUE))
title("June Temperature Anomalies and CO2 Trend (1958-2017)")
par(new = TRUE)
plot(tempCO2data$Trend, type = "l", col = "darkgreen", lwd = 2,
     axes = FALSE, xlab = "", ylab = "",
     ylim = range(tempCO2data$Trend, na.rm = TRUE))
axis(side = 4)
mtext("CO2 Trend", side = 4, line = 3)
legend("topright", legend = c("June Temp Anomaly", "CO2 Trend"),
       col = c("blue", "darkgreen"), lty = 1, lwd = 2)
