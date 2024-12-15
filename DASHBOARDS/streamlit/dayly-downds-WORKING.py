import requests
import pandas as pd

# List of packages to query
packages = ["vaccineff", "serofoi", "sivirep"]

# Define the date range
start_date = "2024-11-05"
end_date = "2024-12-14"

# Function to fetch download data for a package from a specific date range
def get_download_data(package_name, start_date, end_date):
    url = f"https://cranlogs.r-pkg.org/downloads/daily/{start_date}:{end_date}/{package_name}"
    try:
        response = requests.get(url)
        response.raise_for_status()
        data = response.json()
        if "downloads" in data[0]:
            return pd.DataFrame(data[0]["downloads"])
        else:
            return None
    except requests.exceptions.RequestException:
        return None

# Function to fetch download data using the "last-month" endpoint
def get_download_data_last_month(package_name):
    url = f"https://cranlogs.r-pkg.org/downloads/daily/last-month/{package_name}"
    try:
        response = requests.get(url)
        response.raise_for_status()
        data = response.json()
        if "downloads" in data[0]:
            return pd.DataFrame(data[0]["downloads"])
        else:
            return None
    except requests.exceptions.RequestException as e:
        print(f"Error while fetching data for {package_name}: {e}")
        return None

# Collect download data for each package
for package in packages:
    print(f"\nDownloading data for package: {package}")
    
    # Try to get data for the specific date range first
    data = get_download_data(package, start_date, end_date)
    
    # Check if data was fetched using the specific date range
    if data is not None and not data.empty:
        print(f"Using specific date range ({start_date} to {end_date}) for {package}:")
        print(data)
    else:
        print(f"No data found for {package} for the specific date range. Using last-month data.")
        
        # If the specific date range doesn't work, fallback to last-month
        data_last_month = get_download_data_last_month(package)
        if data_last_month is not None and not data_last_month.empty:
            print(f"Using last-month data for {package}:")
            print(data_last_month)
        else:
            print(f"No download data found for {package} even with last-month.")
