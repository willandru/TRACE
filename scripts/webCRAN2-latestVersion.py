import requests

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
        return {"error": f"Failed to fetch data for package: {package_name}"}

# Ejemplo
package_name = "ggplot2"
info = get_package_info(package_name)
print(info)
