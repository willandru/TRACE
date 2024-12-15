import requests
from datetime import datetime, timedelta

# Paquetes predefinidos
packages = ["vaccineff", "sivirep", "ggplot2"]

# Fechas para los últimos 15 días
end_date = datetime.today()
start_date = end_date - timedelta(days=15)

# Función para obtener descargas diarias de CRAN
def get_daily_downloads(package, from_date, to_date):
    try:
        url = f"https://cranlogs.r-pkg.org/downloads/daily/{from_date}/{to_date}/{package}"
        response = requests.get(url, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            if "downloads" in data and len(data["downloads"]) > 0:
                for record in data["downloads"]:
                    print(f"Paquete: {package} | Fecha: {record['day']} | Descargas: {record['downloads']}")
            else:
                print(f"No hay descargas para el paquete {package} en el rango de fechas.")
        else:
            print(f"Error al consultar la API para {package}: {response.status_code}")
    except Exception as e:
        print(f"Error al obtener descargas para {package}: {e}")

# Convertir las fechas a formato YYYY-MM-DD
from_date = start_date.strftime("%Y-%m-%d")
to_date = end_date.strftime("%Y-%m-%d")

# Obtener y mostrar descargas para cada paquete
for package in packages:
    get_daily_downloads(package, from_date, to_date)
