import requests
import pandas as pd

# List of packages to query
packages = [
    "readepi", "ColOpenData", "cleanepi", "simulist", "linelist", "epiparameter",
    "sivirep", "episoap", "cfr", "serofoi", "epiCo", "iraca", "epichains",
    "superspreading", "finalsize", "epidemics", "vaccineff"
]
# Function to get metadata (version and publication date) from CRAN DB
def get_package_metadata(package_name):
    url = f"https://crandb.r-pkg.org/{package_name}/all"
    try:
        response = requests.get(url)
        response.raise_for_status()
        data = response.json()
        if 'timeline' in data:
            timeline = data['timeline']
            versions = list(timeline.keys())
            dates = list(timeline.values())
            return pd.DataFrame({"Package": [package_name] * len(versions), "Version": versions, "Date": dates})
        else:
            return pd.DataFrame({"Package": package_name, "Version": [None], "Date": [None]})
    except requests.exceptions.RequestException:
        return pd.DataFrame({"Package": package_name, "Version": [None], "Date": [None]})

# Function to get daily downloads using the "last-month" endpoint
def get_daily_downloads(package_name):
    try:
        # Use the 'last-month' endpoint for the package
        url = f"https://cranlogs.r-pkg.org/downloads/daily/last-month/{package_name}"
        response = requests.get(url)
        response.raise_for_status()  # Raise error for invalid responses
        data = response.json()
        if "downloads" in data[0]:
            downloads = data[0]["downloads"]
            return pd.DataFrame(downloads)
        else:
            return pd.DataFrame({"Package": [package_name], "Date": [None], "Downloads": [None]})
    except requests.exceptions.RequestException as e:
        print(f"Error while fetching data for {package_name}: {e}")
        return pd.DataFrame({"Package": [package_name], "Date": [None], "Downloads": [None]})

# Create an empty DataFrame to store metadata for all packages
metadata_all = pd.DataFrame()

# Collect metadata for all packages
for pkg in packages:
    print(f"Downloading metadata for package: {pkg}")
    
    # Get metadata (version and publication date)
    metadata_df = get_package_metadata(pkg)
    
    # Append metadata to the main DataFrame
    metadata_all = pd.concat([metadata_all, metadata_df], ignore_index=True)

# Show metadata for all packages
print("\n**Metadata (Versions and Dates of Publication)**")
print(metadata_all)

# Collect download data for all packages
for pkg in packages:
    print(f"\nDownloading data for package: {pkg}")
    
    # Get daily downloads using the "last-month" endpoint
    downloads_df = get_daily_downloads(pkg)
    
    if not downloads_df.empty:
        print(f"**Download Data (Last Month)** for {pkg}")
        print(downloads_df)
    else:
        print(f"No download data found for package {pkg}.")
