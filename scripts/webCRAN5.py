import requests

# Función para obtener todas las versiones de un paquete
def get_package_versions(package_name):
    url = f"https://crandb.r-pkg.org/{package_name}/all"
    response = requests.get(url)
    if response.status_code == 200:
        package_info = response.json()
        timeline = package_info.get("timeline", {})
        return [{"version": version, "date": date} for version, date in timeline.items()]
    else:
        return []

# Lista de paquetes a consultar
packages = ["vaccineff", "serofoi", "sivirep"]

# Consultar datos para cada paquete
for package in packages:
    print(f"\nPaquete: {package}")
    versions = get_package_versions(package)
    if versions:
        for version_info in versions:
            print(f"  Versión: {version_info['version']}")
            print(f"  Fecha de publicación: {version_info['date']}")
    else:
        print("  No se encontraron versiones publicadas.")
