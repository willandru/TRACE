import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd


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




import itertools
import matplotlib.pyplot as plt

import itertools
import matplotlib.pyplot as plt
import pandas as pd

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



##CUMULATIVE DAILYDOWNLOADS PLOT ------------------------------------------------------

import matplotlib.pyplot as plt
import pandas as pd

def plot_cumulative_downloads(dataframe):
    plt.figure(figsize=(16, 6))  # Set the figure size

    # Convert 'day' to datetime.date for plotting
    dataframe['day'] = pd.to_datetime(dataframe['day']).dt.date

    # Calculate cumulative sum for each package
    cumulative_data = dataframe.copy()
    cumulative_data.iloc[:, 1:] = cumulative_data.iloc[:, 1:].cumsum()

    # Set the color palette to "Set2"
    colormap = plt.colormaps['Set2']  # Updated to use the new API
    colors = colormap.colors[:len(cumulative_data.columns[1:])]

    # Plot cumulative downloads for each package
    for idx, column in enumerate(cumulative_data.columns[1:]):  # Skip the 'day' column
        plt.plot(cumulative_data['day'], cumulative_data[column], label=f"{column}", color=colors[idx])

    # Customization
    plt.title("Cumulative Daily Downloads for Packages", fontsize=16)
    plt.xlabel("Date", fontsize=12)
    plt.ylabel("Cumulative Downloads", fontsize=12)

    # Place legend below the plot in horizontal layout
    plt.legend(title=None, fontsize=10, loc="upper center", bbox_to_anchor=(0.5, -0.4), ncol=4, frameon=False)

    plt.grid(True, linestyle='--', alpha=0.6)
    plt.xticks(rotation=45)
    plt.tight_layout(rect=[0, 0, 1, 0.8])  # Adjust layout to make space below the plot
    plt.show()



## HEAT MAP PLOT ------------------------------------------------------


from datetime import datetime, timedelta
import pandas as pd
import requests

def get_download_data_last_30_days(package_name):
    """
    Fetch download data for the last 30 days for a specific package.
    """
    # Calcular las fechas de los últimos 30 días
    end_date = datetime.now().date()
    start_date = end_date - timedelta(days=30)

    print(f"Fetching download data for package '{package_name}' from {start_date} to {end_date}")

    # Intentar obtener datos para el rango específico
    data = get_download_data(package_name, start_date, end_date)

    if data is not None and not data.empty:
        print("Download data fetched successfully for the specified date range.")
        return data
    else:
        # Si no hay datos en el rango, intentar con el endpoint 'last-month'
        print("No data for the specified date range. Using last-month endpoint.")
        data_last_month = get_download_data_last_month(package_name)
        if data_last_month is not None and not data_last_month.empty:
            # Filtrar únicamente los últimos 30 días en caso de datos adicionales
            data_last_month['day'] = pd.to_datetime(data_last_month['day']).dt.date
            data_filtered = data_last_month[data_last_month['day'] > start_date]
            print("Download data fetched successfully using last-month endpoint.")
            return data_filtered
        else:
            print(f"No data found for package '{package_name}' in the last-month endpoint either.")
            return pd.DataFrame(columns=['day', 'downloads'])





import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

def plot_last_30_days_calendar_heatmap(dataframe):
    """
    Create a calendar-style heatmap for the downloads of a single package for the last 30 days.
    """
    # Asegurarse de que los datos contengan las columnas necesarias
    if 'day' not in dataframe.columns or 'downloads' not in dataframe.columns:
        raise ValueError("DataFrame must contain 'day' and 'downloads' columns.")
    
    # Preparar los datos
    dataframe['day'] = pd.to_datetime(dataframe['day'])
    dataframe['weekday'] = dataframe['day'].dt.weekday  # Lunes = 0, Domingo = 6
    dataframe['week'] = dataframe['day'].dt.isocalendar().week  # Número de la semana

    # Ajustar la semana base para alinear en la matriz
    dataframe['week'] -= dataframe['week'].min()

    # Crear la matriz tipo calendario
    calendar_matrix = dataframe.pivot(index='week', columns='weekday', values='downloads')

    # Completar los valores faltantes con 0
    calendar_matrix = calendar_matrix.replace(np.nan, 0)

    # Plotear el heatmap
    plt.figure(figsize=(10, 6))
    sns.heatmap(calendar_matrix, cmap="YlGnBu", annot=True, fmt=".0f", linewidths=0.5, 
                cbar_kws={'label': 'Downloads'}, square=True)

    # Configurar etiquetas de los ejes
    plt.xticks(ticks=np.arange(7) + 0.5, labels=['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'], fontsize=10)
    plt.yticks(ticks=np.arange(len(calendar_matrix.index)) + 0.5, 
               labels=[f"Week {int(week)+1}" for week in calendar_matrix.index], fontsize=10, rotation=0)

    # Configuración del título
    plt.title("Downloads (Last 30 Days)", fontsize=14)
    plt.xlabel("Day of Week", fontsize=12)
    plt.ylabel("Week", fontsize=12)

    plt.tight_layout()
    plt.show()















# List of packages to query
packages = [ "readepi", "ColOpenData", "sivirep", "vaccineff", "epichains"]

# Define the date range
start_date = "2024-08-15"
end_date = "2024-12-02"
#metadata = collect_metadata_for_packages(packages)
#print(metadata.columns)

#df = collect_download_data(packages, start_date, end_date)
# Assuming the final DataFrame from collect_download_data is stored in `df`
#plot_daily_downloads_with_metadata(df, metadata)
#plot_cumulative_downloads(df)


df2 = get_download_data_last_30_days("vaccineff")
print(df2)

plot_last_30_days_calendar_heatmap(df2)