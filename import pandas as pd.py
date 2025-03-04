import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Cargar el archivo .dta
file_path = "/mnt/data/TenderosFU03_Publica.dta"  # Ajusta la ruta si es necesario
df = pd.read_stata(file_path)

# Ver las primeras filas y columnas disponibles
print(df.head())
print(df.columns)  # Mostrar nombres exactos de columnas

# Seleccionar variables relevantes (ajusta los nombres según el dataset)
solicitud_credito = "Solic_Credito"  # Ajustar según la variable real
uso_billeteras = "Elec_Wallet_Uso"  # Ajustar según la variable real

# Convertir a categóricas si es necesario
df[solicitud_credito] = df[solicitud_credito].astype("category")
df[uso_billeteras] = df[uso_billeteras].astype("category")

# Graficar solicitud de crédito
plt.figure(figsize=(8, 5))
sns.countplot(x=df[solicitud_credito])
plt.title("Solicitud de crédito en el último año")
plt.xlabel("Solicitó crédito (Sí/No)")
plt.ylabel("Frecuencia")
plt.show()

# Graficar uso de billeteras electrónicas
plt.figure(figsize=(8, 5))
sns.countplot(x=df[uso_billeteras])
plt.title("Uso de billeteras electrónicas")
plt.xlabel("Uso de billeteras electrónicas (Sí/No)")
plt.ylabel("Frecuencia")
plt.show()