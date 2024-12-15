import requests
import pandas as pd


import matplotlib.pyplot as plt
import seaborn as sns
import itertools

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
            return pd.DataFrame({
                "package": [package_name] * len(versions), 
                "version": versions, 
                "first_cran_date": dates
            })
        else:
            return pd.DataFrame({
                "package": [package_name], 
                "version": [None], 
                "first_cran_date": [None]
            })
    except requests.exceptions.RequestException:
        return pd.DataFrame({
            "package": [package_name], 
            "version": [None], 
            "first_cran_date": [None]
        })


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


# Function to collect daily download data for all packages

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

    #Combinar todos los datos en un solo DataFrame, cada paquete como una columna

    if all_data:
        final_data = pd.DataFrame(all_data)
        final_data.reset_index(inplace=True)  # Reset index to include 'day' as a column
        final_data.rename(columns={"index": "day"}, inplace=True)  # Ensure 'day' is named explicitly
        
        # Ensure 'day' is a datetime type
        final_data['day'] = pd.to_datetime(final_data['day'])
        
        # Generate a full range of dates from start_date to end_date
        full_date_range = pd.date_range(start=pd.to_datetime(start_date), end=pd.to_datetime(end_date))
        
        # Reindex the DataFrame to ensure all dates are included
        final_data = final_data.set_index('day').reindex(full_date_range, fill_value=0).reset_index()
        
        # Rename the reindexed date column back to 'day'
        final_data.rename(columns={"index": "day"}, inplace=True)

        # Ensure no NaN values remain
        final_data.fillna(0, inplace=True)

        print("\n**Final Combined Data with Each Package as a Column:**")
        print(final_data)
        return final_data
    else:
        print("No data collected for any package.")
        return pd.DataFrame()  # Return empty DataFrame if no data





 

def plot_daily_downloads_with_metadata(dataframe, metadata):
    plt.figure(figsize=(16, 6))  # Set the figure size

    # Ensure metadata contains required columns
    if 'package' not in metadata.columns or 'first_cran_date' not in metadata.columns:
        raise ValueError("Metadata DataFrame must contain 'package' and 'first_cran_date' columns.")

    # Convert 'day' and 'first_cran_date' to datetime.date for comparison
    dataframe['day'] = pd.to_datetime(dataframe['day']).dt.date
    metadata['first_cran_date'] = pd.to_datetime(metadata['first_cran_date']).dt.date

    # Create a manual color cycle
    color_cycle = itertools.cycle(plt.cm.tab10.colors)  # Using Matplotlib's tab10 colormap

    # Track whether the release marker legend has been added
    release_marker_added = False

    # Plot daily downloads for each package
    for column in dataframe.columns[1:]:  # Skip the 'day' column
        # Find the publication date for the package from metadata
        publication_date = metadata.loc[metadata['package'] == column, 'first_cran_date']
        if not publication_date.empty:
            pub_date = publication_date.values[0]  # Get the date as a pandas datetime.date
            
            # Check if pub_date is within the range of dataframe['day']
            if pub_date in dataframe['day'].values:
                # Split the DataFrame into two parts: before and after the release date
                before_release = dataframe[dataframe['day'] < pub_date]
                after_release = dataframe[dataframe['day'] >= pub_date]

                # Add a zero point at the day of release to the before_release line
                if not before_release.empty:
                    new_row = pd.DataFrame({'day': [pub_date], column: [0]})
                    before_release = pd.concat([before_release, new_row], ignore_index=True)

                # Get the next color in the cycle
                color = next(color_cycle)

                # Plot the first part (before the release) with transparency
                plt.plot(before_release['day'], before_release[column], color=color, alpha=0.3)

                # Plot the second part (after the release) with full opacity
                plt.plot(after_release['day'], after_release[column], color=color, label=f"{column}")

                # Plot the release date as a red marker
                if not release_marker_added:
                    plt.scatter(pub_date, 0, color='red', zorder=5, s=100, edgecolor='black', label="_Primer Release")  # Hidden label
                    release_marker_added = True
                else:
                    plt.scatter(pub_date, 0, color='red', zorder=5, s=100, edgecolor='black')

                # Plot the solid vertical line connecting the x-axis to the first downloads
                first_download_value = after_release.loc[after_release['day'] == pub_date, column].values[0]
                plt.vlines(pub_date, ymin=0, ymax=first_download_value, colors=color, linestyles='solid', label=None)
            else:
                # If pub_date is not in the range, plot normally
                color = next(color_cycle)
                plt.plot(dataframe['day'], dataframe[column], color=color, label=column)
        else:
            # If no publication date, plot normally
            color = next(color_cycle)
            plt.plot(dataframe['day'], dataframe[column], color=color, label=column)

    # Add "Primer Release" label explicitly at the end
    plt.scatter([], [], color='red', s=100, label="Primer Release")  # Dummy point for legend

    # Customization
    plt.title("Daily Downloads for Packages with CRAN Publication Dates", fontsize=16)
    plt.xlabel("Date", fontsize=12)
    plt.ylabel("Downloads", fontsize=12)
    
    # Move legend outside of the plot
    plt.legend(title=None, fontsize=10, loc="center left", bbox_to_anchor=(1, 0.5))  # Outside legend

    plt.grid(True, linestyle='--', alpha=0.6)
    plt.xticks(rotation=45)
    plt.tight_layout(rect=[0, 0, 0.8, 1])  # Adjust layout to make space for the legend
    plt.show()





# List of packages to query
packages = [ "readepi", "ColOpenData", "sivirep", "vaccineff", "epichains"]

# Define the date range
start_date = "2024-05-15"
end_date = "2024-12-02"
metadata = collect_metadata_for_packages(packages)
print(metadata.columns)

df = collect_download_data(packages, start_date, end_date)
# Assuming the final DataFrame from collect_download_data is stored in `df`
plot_daily_downloads_with_metadata(df, metadata)

