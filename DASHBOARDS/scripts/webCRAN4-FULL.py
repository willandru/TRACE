import requests

# Función para obtener la información básica del paquete
def get_package_info(package_name):
    url = f"https://crandb.r-pkg.org/{package_name}"
    response = requests.get(url)
    
    if response.status_code == 200:
        package_info = response.json()
        latest_version = package_info.get("Version", "No version found")
        publication_date = package_info.get("Date/Publication", "No publication date found")
        return {
            "name": package_name,
            "latest_version": latest_version,
            "publication_date": publication_date
        }
    else:
        return {"name": package_name, "error": f"Failed to fetch data"}

# Función para obtener descargas diarias
def get_daily_downloads(package_name, date):
    url = f"https://cranlogs.r-pkg.org/downloads/daily/{date}/{date}/{package_name}"
    response = requests.get(url)
    
    if response.status_code == 200:
        downloads_data = response.json()
        downloads = downloads_data.get("downloads", 0)
        return {"name": package_name, "downloads": downloads, "date": date}
    else:
        return {"name": package_name, "error": "Failed to fetch downloads data"}

# Función para obtener todas las versiones
def get_package_versions(package_name):
    url = f"https://crandb.r-pkg.org/{package_name}/all"
    response = requests.get(url)
    
    if response.status_code == 200:
        package_info = response.json()
        versions = list(package_info.get("timeline", {}).keys())
        return {"name": package_name, "versions": versions}
    else:
        return {"name": package_name, "error": "Failed to fetch versions"}

# Procesar varios paquetes
def process_packages(packages, date):
    results = []
    for package in packages:
        basic_info = get_package_info(package)
        downloads = get_daily_downloads(package, date)
        versions = get_package_versions(package)
        
        results.append({
            "name": package,
            "latest_version": basic_info.get("latest_version", "N/A"),
            "publication_date": basic_info.get("publication_date", "N/A"),
            "downloads": downloads.get("downloads", "N/A"),
            "versions": versions.get("versions", [])
        })
    return results

# Paquetes a consultar
packages = ['vaccineff', 'serofoi', 'sivirep']
date = "2024-12-13"  # Fecha específica (YYYY-MM-DD)

# Obtener información
results = process_packages(packages, date)

# Mostrar resultados
for result in results:
    print(f"Paquete: {result['name']}")
    print(f"  Última versión: {result['latest_version']}")
    print(f"  Fecha de publicación: {result['publication_date']}")
    print(f"  Descargas el {date}: {result['downloads']}")
    print(f"  Versiones disponibles: {', '.join(result['versions'])}")
    print()
