import requests

def get_package_versions(package_name):
    url = f"https://crandb.r-pkg.org/{package_name}/all"
    response = requests.get(url)
    
    if response.status_code == 200:
        package_info = response.json()
        versions = list(package_info.get("timeline", {}).keys())
        return {"package": package_name, "versions": versions}
    else:
        return {"error": f"Failed to fetch versions for package: {package_name}"}

# Ejemplo
versions_info = get_package_versions("ggplot2")
print(versions_info)
