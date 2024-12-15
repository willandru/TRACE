import requests
from bs4 import BeautifulSoup

# Función para obtener la fecha de publicación inicial de un paquete en CRAN
def get_cran_first_publication_date(package_name):
    url = f"https://cran.r-project.org/web/packages/{package_name}/index.html"
    try:
        response = requests.get(url, timeout=10)
        if response.status_code == 200:
            soup = BeautifulSoup(response.content, 'html.parser')
            description = soup.find('table', {'summary': 'Package details'})
            if description:
                for row in description.find_all('tr'):
                    if 'Published' in row.text:
                        return row.find('td').text.strip()
    except requests.RequestException as e:
        return f"Error: {e}"
    return "Fecha no encontrada"

# Lista de paquetes a consultar
packages = ['sivirep', 'vaccineff', 'serofoi']
publication_dates = {}

# Consultar las fechas de publicación
for package in packages:
    publication_dates[package] = get_cran_first_publication_date(package)

# Mostrar resultados
for package, date in publication_dates.items():
    print(f"Paquete: {package}, Fecha de publicación: {date}")
