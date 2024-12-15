import requests
import pandas as pd


import matplotlib.pyplot as plt
import seaborn as sns

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


def collect_download_data(packages, start_date, end_date):
    all_data = {}  # Dictionary to store data for each package, keyed by package name

    for package in packages:
        print(f"\nDownloading download data for package: {package}")

        # Attempt to fetch data for the specified date range
        data = get_download_data(package, start_date, end_date)
        data_last_month = None

        if data is not None and not data.empty:
            print(f"\n**Download Data (Specific Date Range: {start_date} to {end_date}) for {package}:**")
            print(data)

            # Check if both start_date and end_date exist in the data
            if pd.to_datetime(start_date) in pd.to_datetime(data['day']).values and pd.to_datetime(end_date) in pd.to_datetime(data['day']).values:
                print(f"Both start_date ({start_date}) and end_date ({end_date}) are present in the data for {package}.")
            else:
                print(f"start_date ({start_date}) or end_date ({end_date}) is not present in the data for {package}. Downloading last-month data.")
                data_last_month = get_download_data_last_month(package)
                print(data_last_month)
        else:
            print(f"No data found for {package} for the specific date range. Using last-month data.")
            data_last_month = get_download_data_last_month(package)

        # Combine specific range data with last-month data if necessary
        if data is not None and data_last_month is not None:
            combined_data = pd.concat([data, data_last_month], ignore_index=True)
        elif data is not None:
            combined_data = data
        elif data_last_month is not None:
            combined_data = data_last_month
        else:
            combined_data = pd.DataFrame()  # Empty DataFrame if no data

        # Add combined data to the dictionary if not empty
        if not combined_data.empty:
            # Ensure the data has a 'day' column
            combined_data = combined_data.drop_duplicates()  # Remove duplicates
            all_data[package] = combined_data.set_index('day')['downloads']

    # Combine all data into a single DataFrame with each package as a column
    if all_data:
        final_data = pd.DataFrame(all_data)
        final_data.reset_index(inplace=True)  # Reset index to include 'day' as a column
        final_data.rename(columns={"index": "day"}, inplace=True)  # Ensure 'day' is named explicitly
        # Filter by the specified date range
        final_data['day'] = pd.to_datetime(final_data['day'])  # Ensure day is a datetime type
        final_data = final_data[(final_data['day'] >= pd.to_datetime(start_date)) & 
                                (final_data['day'] <= pd.to_datetime(end_date))]

        # Fill NaN values with 0
        final_data.fillna(0, inplace=True)



        print("\n**Final Combined Data with Each Package as a Column:**")
        print(final_data)
        return final_data
    else:
        print("No data collected for any package.")
        return pd.DataFrame()  # Return empty DataFrame if no data



# Function to collect metadata for all packages
def collect_metadata_for_packages(packages):
    # Create an empty DataFrame to store metadata for all packages
    metadata_all = pd.DataFrame()

    # Collect metadata for all packages
    for pkg in packages:
        print(f"\nFetching metadata for package: {pkg}")
        
        # Get metadata (version and dates)
        metadata_df = get_package_metadata(pkg)
        
        # Append metadata to the main DataFrame
        metadata_all = pd.concat([metadata_all, metadata_df], ignore_index=True)

    # Return the collected metadata
    return metadata_all




# List of packages to query
packages = [ "readepi", "ColOpenData", "sivirep", "vaccineff"]

# Define the date range
start_date = "2024-10-05"
end_date = "2024-11-14"



# Function to plot daily downloads for each package
def plot_daily_downloads(packages, start_date, end_date):
    # Create an empty DataFrame to store all download data
    all_download_data = pd.DataFrame()

    # Collect download data for each package
    for package in packages:
        print(f"\nDownloading data for package: {package}")
        
        # Try to get data for the specific date range first
        data = get_download_data(package, start_date, end_date)

        # If no data found for the date range, try the "last-month" fallback
        if data is None or data.empty:
            print(f"No data found for {package} for the specific date range. Using last-month data.")
            data = get_download_data_last_month(package)

        # If data is still available, append it to the all_download_data DataFrame
        if data is not None and not data.empty:
            data['Package'] = package  # Add the package name as a column to identify the package
            
            # Create a new column with the formatted date (Month-Day format)
            data['Formatted_Date'] = pd.to_datetime(data['day']).dt.strftime('%b-%d')
            
            all_download_data = pd.concat([all_download_data, data], ignore_index=True)
        else:
            print(f"No download data found for {package} even with last-month.")

    # Plotting the data if we have any valid download data
    if not all_download_data.empty:
        plt.figure(figsize=(12, 6))
        sns.lineplot(x='Formatted_Date', y='downloads', hue='Package', data=all_download_data, marker='o')
        plt.title('Daily Downloads for Packages', fontsize=16)
        plt.xlabel('Date', fontsize=12)
        plt.ylabel('Downloads', fontsize=12)
        plt.xticks(rotation=90)  # Make x-axis labels vertical (90 degrees)
        plt.tight_layout()
        plt.legend(title='Package')
        plt.show()
    else:
        print("No valid download data available to plot.")




# Example usage
plot_daily_downloads(packages, start_date, end_date)
