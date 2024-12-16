import streamlit as st
import requests
import pandas as pd
import plotly.express as px
from datetime import datetime

import matplotlib.pyplot as plt
import seaborn as sns
import itertools

import matplotlib.dates as mdates

from matplotlib.cm import get_cmap







# Declaracion de Funciones ----------------------------------------------------------------------------------------------
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
                "cran_release_date": dates
            })
        else:
            return pd.DataFrame({
                "package": [package_name], 
                "version": [None], 
                "cran_release_date": [None]
            })
    except requests.exceptions.RequestException:
        return pd.DataFrame({
            "package": [package_name], 
            "version": [None], 
            "cran_release_date": [None]
        })


#VERIFICAR QUE LOS PAQUETES ESTEN EN CRAN
# Función para verificar si un paquete está en CRAN
def is_package_in_cran(package_name):
    try:
        url = f"https://crandb.r-pkg.org/{package_name}/all"
        response = requests.get(url, timeout=5)
        return response.status_code == 200  # El paquete está en CRAN si la respuesta es 200
    except requests.RequestException:
        return False  # Retornar False si ocurre algún error




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

    # Combine all the data into a single DataFrame, each package as a column
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
        
        # Rename the reindexed date column back to 'Date'
        final_data.rename(columns={"index": "Date"}, inplace=True)

        # Ensure no NaN values remain
        final_data.fillna(0, inplace=True)
        
        # Reset index to start from 1 and rename it to 'Day'
        final_data.index = final_data.index + 1  # Make index start from 1
        final_data.index.name = 'Day'  # Name the index as 'Day'

        print("\n**Final Combined Data with Each Package as a Column:**")
        print(final_data)
        return final_data
    else:
        print("No data collected for any package.")
        return pd.DataFrame()  # Return empty DataFrame if no data


# Función para obtener los primeros 10 colores hardcodeados en HEX
def get_first_colors_hardcoded():
    """
    Returns a predefined list of 10 HEX colors from the 'Set2' colormap.
    
    Returns:
    list: List of HEX color strings.
    """
    return ['#66c2a5',  '#fc8d62', '#8da0cb', '#e78ac3', 
            '#a6d854', '#ffd92f', '#e5c494', '#66c2a5', '#b3b3b3']

def calculate_accumulated_days_and_growth_rate(dataframe):
    """
    Adds `dias` (accumulated days) and `tasa_crecimiento` (growth rate) columns 
    for each package, and renames original download columns.

    Parameters:
    dataframe (pd.DataFrame): Original DataFrame with download data.

    Returns:
    pd.DataFrame: Updated DataFrame with renamed and calculated columns.
    """
    dataframe = dataframe.copy()  # Avoid modifying the original DataFrame
    dataframe['Date'] = pd.to_datetime(dataframe['Date'])  # Ensure Date column

    result = pd.DataFrame({'Date': dataframe['Date']})  # Start with Date column

    for package in dataframe.columns[1:]:
        # Rename original download column
        result[f"descargas_{package}"] = dataframe[package]

        # Calculate accumulated days
        dias_acumulados = (dataframe['Date'] - dataframe['Date'].min()).dt.days
        dias_acumulados = dias_acumulados.where(dataframe[package] != 0, 0)  # Set to 0 before release

        # Avoid division by zero and calculate growth rate
        valid_days = dias_acumulados.replace(0, 1)
        tasa_crecimiento = dataframe[package] / valid_days
        tasa_crecimiento = tasa_crecimiento.where(dataframe[package] != 0, 0)  # Set growth rate to 0

        # Add calculated columns to result
        result[f"dias_{package}"] = dias_acumulados
        result[f"tasa_crecimiento_{package}"] = tasa_crecimiento

    return result
















## PLOT 1
def plot_daily_downloads_with_metadata(dataframe, metadata, color_palette):
    fig, ax = plt.subplots(figsize=(20, 10))  # Crear la figura y los ejes

    # Asegurarse de que metadata contiene las columnas requeridas
    if 'package' not in metadata.columns or 'cran_release_date' not in metadata.columns:
        raise ValueError("Metadata DataFrame must contain 'package' and 'cran_release_date' columns.")

    # Convertir 'Date' y 'cran_release_date' a formato datetime.date para comparación
    dataframe['Date'] = pd.to_datetime(dataframe['Date']).dt.date
    metadata['cran_release_date'] = pd.to_datetime(metadata['cran_release_date']).dt.date

    # Graficar descargas diarias para cada paquete
    for column in dataframe.columns[1:]:  # Omitir la columna 'Date'
        color = color_palette.get(column, "black")  # Mapear color basado en el paquete

        # Buscar la fecha de publicación para el paquete en metadata
        publication_date = metadata.loc[metadata['package'] == column, 'cran_release_date']
        pub_date = publication_date.values[0] if not publication_date.empty else None

        if pub_date:
            # Dividir los datos en dos partes: antes y después de la fecha de lanzamiento
            before_release = dataframe[dataframe['Date'] < pub_date]
            after_release = dataframe[dataframe['Date'] >= pub_date]

            # Añadir un punto cero en la fecha de lanzamiento a la línea de before_release
            if not before_release.empty:
                new_row = pd.DataFrame({'Date': [pub_date], column: [0]})
                before_release = pd.concat([before_release, new_row], ignore_index=True)

            # Graficar partes antes y después del lanzamiento
            ax.plot(before_release['Date'], before_release[column], color=color, alpha=0)
            ax.plot(after_release['Date'], after_release[column], color=color, label=f"{column}")

            # Marcar fecha de lanzamiento
            if not after_release.empty and pub_date in after_release['Date'].values:
                first_download_value = after_release.loc[after_release['Date'] == pub_date, column].values[0]
                ax.scatter(pub_date, 0, color='red', zorder=5, s=100, edgecolor='black')
                ax.vlines(pub_date, ymin=0, ymax=first_download_value, colors=color, linestyles='solid')
        else:
            # Si no hay fecha de publicación, graficar normalmente
            ax.plot(dataframe['Date'], dataframe[column], color=color, label=column)

    # Añadir "Primer Release" como marcador
    ax.scatter([], [], color='red', s=100, edgecolor='black', label="Primer Release")

    # Personalización del gráfico
    ax.set_title("Descargas Diarias de Paquetes en CRAN", fontsize=26, fontweight='bold', pad=20)
    ax.set_xlabel("Fecha", fontsize=20, fontweight='bold', labelpad=25)
    ax.set_ylabel("Descargas", fontsize=20, fontweight='bold', labelpad=20)

    # Formato del eje x
    num_days = (dataframe['Date'].max() - dataframe['Date'].min()).days
    interval = 1 if num_days <= 15 else 2 if num_days <= 60 else 5
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%b-%d'))
    ax.xaxis.set_major_locator(mdates.DayLocator(interval=interval))
    ax.xaxis.set_minor_locator(mdates.DayLocator(interval=1))

    plt.xticks(rotation=90, fontsize=18)
    plt.yticks(fontsize=18)

    # Leyenda y ajustes finales
    handles, labels = ax.get_legend_handles_labels()
    if "Primer Release" in labels:
        # Reorganizar "Primer Release" al final
        handles.append(handles.pop(labels.index("Primer Release")))
        labels.append(labels.pop(labels.index("Primer Release")))
    ax.legend(handles, labels, title=None, fontsize=19, loc="lower center", bbox_to_anchor=(0.5, -0.4), ncol=4)

    ax.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()  # Ajustar el diseño del gráfico
    
    st.pyplot(fig)

















## PLOT 2
def plot_cumulative_downloads(dataframe, color_palette):
    """
    Plots cumulative downloads for each package, adding a red circle for the first release date
    and improving aesthetics to match other plots.
    """
    # Crear una figura y ejes para el gráfico
    fig, ax = plt.subplots(figsize=(20, 10))  # Ajustar tamaño para mejor visibilidad

    # Convertir 'Date' a datetime.date para graficar
    dataframe['Date'] = pd.to_datetime(dataframe['Date']).dt.date

    # Calcular la suma acumulativa para cada paquete
    cumulative_data = dataframe.copy()
    cumulative_data.iloc[:, 1:] = cumulative_data.iloc[:, 1:].cumsum()

    # Configuración dinámica del intervalo de la cuadrícula vertical
    num_days = (dataframe['Date'].max() - dataframe['Date'].min()).days
    interval = 1 if num_days <= 15 else 2 if num_days <= 60 else 5

    # Graficar descargas acumulativas para cada paquete
    for column in cumulative_data.columns[1:]:  # Omitir la columna 'Date'
        color = color_palette.get(column, "black")  # Obtener color desde la paleta

        # Obtener la fecha de la primera descarga no nula
        first_release_date = cumulative_data.loc[cumulative_data[column] > 0, 'Date'].min()

        # Graficar la línea de descargas acumulativas
        ax.plot(cumulative_data['Date'], cumulative_data[column], 
                label=column.replace("descargas_", ""), color=color, linewidth=2)

        # Añadir el punto rojo con borde negro en la fecha del primer release
        if pd.notnull(first_release_date):
            ax.scatter(first_release_date, cumulative_data.loc[cumulative_data['Date'] == first_release_date, column].iloc[0],
                       color='red', edgecolor='black', s=150, zorder=5, label="_RedCircle")

    # Añadir "Primer Release" a la leyenda con un marcador rojo
    ax.scatter([], [], color='red', edgecolor='black', s=150, label="Primer Release")

    # Personalización del gráfico
    ax.set_title("Descargas Acumulativas Diarias para Paquetes", fontsize=26, fontweight='bold', pad=20)
    ax.set_xlabel("Fecha", fontsize=20, fontweight='bold', labelpad=25)
    ax.set_ylabel("Descargas Acumulativas", fontsize=20, fontweight='bold', labelpad=20)

    # Formatear eje X en estilo `Nov-15`
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%b-%d'))
    ax.xaxis.set_major_locator(mdates.DayLocator(interval=interval))
    ax.xaxis.set_minor_locator(mdates.DayLocator(interval=1))
    plt.xticks(rotation=90, fontsize=18)
    plt.yticks(fontsize=18)

    # Cuadrícula para mayor legibilidad
    ax.grid(axis='x', which='major', linestyle='--', alpha=0.6)
    ax.grid(axis='y', which='major', linestyle='--', alpha=0.6)

    # Mover la leyenda debajo del gráfico
    handles, labels = ax.get_legend_handles_labels()
    ax.legend(handles, labels, title=None, fontsize=19, loc="lower center", bbox_to_anchor=(0.5, -0.4), ncol=4)

    # Ajustar diseño para dar espacio debajo del gráfico
    plt.tight_layout()
    st.pyplot(fig)























def plot_growth_rate(dataframe, metadata, color_palette):
    """
    Plots the growth rate for each package using precomputed growth rates in the DataFrame,
    splitting the line into transparent (before release), solid (after release),
    and adding a solid vertical line on the release date.

    Parameters:
    dataframe (pd.DataFrame): DataFrame with 'Date' and `tasa_crecimiento_*` columns.
    metadata (pd.DataFrame): Metadata table containing 'package' and 'cran_release_date'.
    color_palette (dict): Dictionary mapping package names to colors.
    """
    fig, ax = plt.subplots(figsize=(20, 10))  # Larger figure size for better readability

    # Determinar el intervalo para las líneas de la cuadrícula vertical
    num_days = (dataframe['Date'].max() - dataframe['Date'].min()).days
    interval = 1 if num_days <= 15 else 2 if num_days <= 60 else 5

    # Iterar por cada columna de tasa de crecimiento
    for column in dataframe.columns:
        if "tasa_crecimiento" in column:  # Identificar las columnas de tasa de crecimiento
            package_name = column.replace("tasa_crecimiento_", "")  # Extraer nombre del paquete
            color = color_palette.get(package_name, "black")  # Color del paquete

            # Buscar la fecha más temprana de lanzamiento en la metadata
            release_date_row = metadata.loc[metadata['package'] == package_name, 'cran_release_date']
            if not release_date_row.empty:
                release_date = pd.Timestamp(release_date_row.min())  # Convertir a Timestamp
            else:
                continue  # Si no hay fecha de lanzamiento, saltar

            # Dividir los datos en dos partes: antes y después del lanzamiento
            before_release = dataframe[dataframe['Date'] < release_date]
            after_release = dataframe[dataframe['Date'] >= release_date]

            # Plot de la parte antes del lanzamiento con transparencia
            ax.plot(before_release['Date'], before_release[column], 
                    color=color, alpha=0, linewidth=2, label=None)

            # Plot de la parte después del lanzamiento (línea sólida)
            ax.plot(after_release['Date'], after_release[column], 
                    color=color, linewidth=2, label=package_name)

            # Validar si la fecha de lanzamiento está en los datos para el círculo rojo
            if release_date in after_release['Date'].values:
                tasa_crecimiento_first_day = after_release.loc[after_release['Date'] == release_date, column].iloc[0]

                # Añadir círculo rojo en el eje X
                ax.scatter(release_date, 0, color='red', edgecolor='black', 
                           s=150, zorder=5, label="_RedCircle")

                # Dibujar línea vertical sólida hasta el primer valor de tasa de crecimiento
                ax.vlines(release_date, ymin=0, ymax=tasa_crecimiento_first_day, 
                          color=color, linestyle='-', linewidth=3, zorder=4)

    # Añadir "Primer Release" a la leyenda
    ax.scatter([], [], color='red', edgecolor='black', s=150, label="Primer Release")

    # Personalización del gráfico
    ax.set_title("Tasa de Crecimiento de Descargas de Paquetes", fontsize=26, fontweight='bold', pad=20)
    ax.set_xlabel("Fecha", fontsize=20, fontweight='bold', labelpad=25)
    ax.set_ylabel("Tasa de Crecimiento (Descargas / Días)", fontsize=20, fontweight='bold', labelpad=20)

    # Formatear eje X con `Nov-15` y ajustar el tamaño de los ticks
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%b-%d'))
    ax.xaxis.set_major_locator(mdates.DayLocator(interval=interval))
    ax.xaxis.set_minor_locator(mdates.DayLocator())
    plt.xticks(rotation=90, fontsize=18)
    plt.yticks(fontsize=18)

    # Cuadrícula y apariencia
    ax.grid(axis='x', which='major', linestyle='--', alpha=0.6)
    ax.grid(axis='y', which='major', linestyle='--', alpha=0.6)

    # Mover la leyenda debajo del gráfico
    handles, labels = ax.get_legend_handles_labels()
    ax.legend(handles, labels, title=None, fontsize=19, loc="lower center", bbox_to_anchor=(0.5, -0.4), ncol=4)

    # Ajustar layout
    plt.tight_layout()
    st.pyplot(fig)








def update_colors(packages, current_colors):
    """
    Display dynamic buttons for each package, allowing users to select colors
    and update the color list.

    Parameters:
    - packages (list): List of package names.
    - current_colors (dict): Dictionary mapping package names to their current colors.

    Returns:
    - dict: Updated dictionary of package colors.
    """
    updated_colors = current_colors.copy()  # Start with the current color mapping

    st.write("### Configuración de Colores")

    with st.container():
        cols = st.columns(len(packages))  # Create one column per package button
        for col, package in zip(cols, packages):
            # Render the package button with its current color
            button_style = f"background-color: {updated_colors[package]}; color: white; border-radius: 5px; padding: 8px;"
            if col.button(package):
                with st.container():
                    st.write(f"### Paquete seleccionado: {package}")

                    # Generate dropdown for selecting new color
                    color_options = list(get_default_colors(packages).values())  # Use dynamic default colors
                    selected_color = st.selectbox(
                        f"Seleccionar color para {package}",
                        options=color_options,
                        index=color_options.index(updated_colors[package]),
                        key=f"color_select_{package}"
                    )
                    # Update the selected package color
                    updated_colors[package] = selected_color

                    # Show the updated color immediately
                    st.markdown(
                        f'<div style="background-color: {selected_color}; color: white; border-radius: 5px; padding: 8px;">Color Actualizado: {selected_color}</div>',
                        unsafe_allow_html=True
                    )
    return updated_colors


























#### FIN DECLARACION DE FUNCIONES**********************************************************************************
### **************************************************************************************************
### **************************************************************************************************
### **************************************************************************************************















## INICIO WEB PAGE

# Configurar la página
st.set_page_config(
    page_title="Dashboard Dinámico",
    page_icon=":bar_chart:",
    layout="wide"
)




#ENCABEZADO--------------------------------------------------------------------------------------------------------------

# Encabezado simple
#st.write("## TÍTULO")

# CSS para el encabezado con franja de color
st.markdown("""
    <style>
    .header {
        background-color: #21303C; /* Cambia el color de fondo aquí */
        padding: 20px; /* Espaciado interno */
        border-radius: 8px; /* Bordes redondeados opcionales */
        display: flex; /* Para alinear texto e imágenes en la misma fila */
        align-items: center; /* Centrar contenido verticalmente */
        justify-content: space-between; /* Distribuir contenido horizontalmente */
        color: white; /* Color del texto */
    }
    .header img {
        height: 50px; /* Altura de las imágenes */
        margin-right: 15px; /* Espaciado entre imágenes y texto */
    }
    .header h1 {
        font-size: 24px; /* Tamaño del texto */
        margin: 0; /* Elimina el margen por defecto */
    }
    </style>
""", unsafe_allow_html=True)

# HTML para el encabezado
st.markdown("""
    <div class="header">
        <img src="https://epiverse-trace.github.io/blueprints/logo_white.svg" alt="Logo izquierdo">
        <h1>Mi Título Personalizado</h1>
         
    </div>
""", unsafe_allow_html=True)


#####  FIN ENCABEZADO ----------------------------------------------------------------------------------------
















# BARRA DE BUSQUEDA -------------------------------------------------------------------------------------------------------

# CSS para centrar y reducir el ancho del input
st.markdown(
    """
    <style>
    /* Contenedor principal del input */
    div[data-testid="stTextInput"] {
        width: 50%; /* Ancho del input */
        margin: 20px auto; /* Centrado con margen superior/inferior */
    }
    
    /* Input real */
    div[data-testid="stTextInput"] input {
        font-size: 16px; /* Tamaño de letra */
        padding: 10px; /* Espaciado interno */
        border: 2px solid #21303C; /* Borde */
        border-radius: 8px; /* Bordes redondeados */
        background-color: #21303C; /* Fondo oscuro */
        color: #F7F7F7; /* Color del texto */
        caret-color: #87BD28; /* Color del cursor */
        text-align: center; /* Centrar el texto */
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); /* Sombra ligera */
    }

    /* Efecto cuando el input está enfocado */
    div[data-testid="stTextInput"] input:focus {
        outline: none; /* Quitar borde azul predeterminado */
        border-color: #87BD28; /* Color del borde enfocado */
        box-shadow: 0px 0px 8px rgba(135, 189, 40, 0.5); /* Sombra verde */
    }

    /* Placeholder personalizado */
    div[data-testid="stTextInput"] input::placeholder {
        color: #BFBFBF; /* Color gris claro para el placeholder */
    }

    /* Sugerencias de autocompletado */
    input:-webkit-autofill, 
    input:-webkit-autofill:hover, 
    input:-webkit-autofill:focus, 
    input:-webkit-autofill:active {
        -webkit-box-shadow: 0 0 0px 1000px #35373A inset !important; /* Fondo oscuro */
        -webkit-text-fill-color: #F7F7F7 !important; /* Color del texto */
        caret-color: #87BD28 !important; /* Cursor */
    }

    /* Estilo de la lista de sugerencias */
    input::-webkit-input-placeholder { /* Placeholder */
        color: #C0C0C0; /* Color gris claro */
    }

    /* Dropdown suggestions on input */
    datalist option {
        background: #3B3D3F; /* Fondo sugerencias */
        color: #F7F7F7; /* Color texto */
        padding: 5px;        
    }
    </style>
    """,
    unsafe_allow_html=True,
)




# CSS para modificar únicamente el ancho de los widgets de fecha
st.markdown("""
    <style>
    div[data-testid="stDateInput"] {
        width: 250px !important; /* Define el ancho deseado aquí */
    }
    </style>
    """, unsafe_allow_html=True)

#_--------------------------------------------------------------------------------------










# Entrada de paquetes con estilo centrado y ancho reducido
input_packages = st.text_input("", "vaccineff, sivirep, serofoi", placeholder="Buscar paquetes...")
# Procesar paquetes
packages = list(dict.fromkeys(pkg.strip() for pkg in input_packages.split(",") if pkg.strip()))

# Filtrar paquetes que están en CRAN
if packages:
    cran_packages = [pkg for pkg in packages if is_package_in_cran(pkg)]
    packages = cran_packages  # Actualizar la variable 'packages' con los paquetes válidos

# Generar la paleta de colores predefinida una sola vez (mapear nombres de paquetes a colores)
if packages:
    colors = get_first_colors_hardcoded()  # Usar colores hardcodeados
    color_palette = {pkg: colors[i] for i, pkg in enumerate(packages)}
else:
    color_palette = {}







# PART 1 ---------------------------------------------------------------------------------------------------------------

# Panel superior: Fecha y gráficos
with st.container():
    ## VERTICAL SPACE
    st.markdown("<div style='margin-top: 100px;'></div>", unsafe_allow_html=True)  

    col1, col2 = st.columns([1, 2])

    # Columna izquierda: Fechas
    with col1:
        # Selección de fechas
        st.markdown("<h3 style=' font-size:24px; font-weight: bold;'>Selección de fechas</h3>",unsafe_allow_html=True)
        start_date = st.date_input("Fecha de Inicio", value=datetime(2024, 11, 13))
        end_date = st.date_input("Fecha de Fin", value=datetime(2024, 12, 13))

        ## VERTICAL SPACE
        st.markdown("<div style='margin-top: 50px;'></div>", unsafe_allow_html=True)  

        if packages and start_date and end_date:
            # Recopilar metadata
            metadata = collect_metadata_for_packages(packages)

            # Mostrar metadata debajo de las fechas
            st.markdown("<h3 style=' font-size:24px; font-weight: bold;'>Versiones y Fechas en CRAN</h3>",unsafe_allow_html=True)
            st.dataframe(metadata if metadata is not None else "No hay metadata.")
            

            ## Agregar tabla del dataframe final debajo de metadata
            all_data = collect_download_data(
                packages, start_date.strftime("%Y-%m-%d"), end_date.strftime("%Y-%m-%d")
            )
            if not all_data.empty:
                st.markdown("<h3 style=' font-size:24px; font-weight: bold;'>Descargas Diarias</h3>", unsafe_allow_html=True)
                st.dataframe(all_data)
            else:
                st.markdown("<p style='color: red;'>No se encontraron datos para los paquetes seleccionados.</p>", unsafe_allow_html=True)

            ## VERTICAL SPACE
            st.markdown("<div style='margin-top: 50px;'></div>", unsafe_allow_html=True)

            updated_df = calculate_accumulated_days_and_growth_rate(all_data)
            
            

    # Columna derecha: Gráficos
    with col2:
        if packages and start_date and end_date:
            if not all_data.empty:
                # Generar el primer gráfico
                plot_daily_downloads_with_metadata(all_data, metadata, color_palette)

                # Generar el segundo gráfico
                ## VERTICAL SPACE
                st.markdown("<div style='margin-top: 50px;'></div>", unsafe_allow_html=True)
                plot_cumulative_downloads(all_data, color_palette)

                ## VERTICAL SPACE
                st.markdown("<div style='margin-top: 50px;'></div>", unsafe_allow_html=True)
                plot_growth_rate(updated_df,metadata, color_palette)
            else:
                st.write("No se encontraron datos para los paquetes seleccionados en el rango de fechas.")















# Separador ----------------------------------------------------------------------------------------------------------------------
st.markdown("---")

## PARTE 2
## PARTE 2
# Obtener los colores iniciales
default_colors = get_first_colors_hardcoded()

# Crear un diccionario inicial de colores por paquete
color_mapping = {package: default_colors[i % len(default_colors)] for i, package in enumerate(packages)}

# Crear la interfaz dinámica
st.markdown(
    """
    <style>
    .dropdown-container {
        display: flex; 
        align-items: center; 
        margin-bottom: 10px;
    }
    .dropdown-package {
        width: 200px;
        height: 35px;
        padding: 5px;
        border: 1px solid #ccc;
        border-radius: 5px;
        font-size: 14px;
        appearance: none;
        text-align: center;
        color: white;
        font-weight: bold;
        cursor: pointer;
    }
    </style>
    """,
    unsafe_allow_html=True,
)

st.markdown("### Selección de colores dinámicos para los paquetes:")

# Generate the dropdowns for each package
for package in packages:
    # Get the current color for the package
    current_color = color_mapping[package]

    # Render a dropdown for the package
    st.markdown(
        f"""
        <div class="dropdown-container">
            <select id="dropdown-{package}" class="dropdown-package" style="background-color: {current_color};" onchange="updateDropdown(this, '{package}')">
                {"".join([f'<option value="{color}" {"selected" if color == current_color else ""} style="background-color: {color}; color: white;">{package}</option>' for color in default_colors])}
            </select>
        </div>
        """,
        unsafe_allow_html=True,
    )

# JavaScript to dynamically update the dropdown background color
st.markdown(
    """
    <script>
    function updateDropdown(dropdown, packageName) {
        const selectedColor = dropdown.value;
        dropdown.style.backgroundColor = selectedColor;
        dropdown.style.color = "white";
    }
    </script>
    """,
    unsafe_allow_html=True,
)
