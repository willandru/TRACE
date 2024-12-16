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


# Función para obtener los primeros 10 colores hardcodeados en HEX
def get_first_colors_hardcoded():
    """
    Returns a predefined list of 10 HEX colors from the 'Set2' colormap.
    
    Returns:
    list: List of HEX color strings.
    """
    return ['#66c2a5', '#66c2a5', '#fc8d62', '#8da0cb', '#e78ac3', 
            '#a6d854', '#ffd92f', '#e5c494', '#b3b3b3', '#b3b3b3']





















## PLOT 1

## PLOT 1

def plot_daily_downloads_with_metadata(dataframe, metadata, color_palette):
    fig, ax = plt.subplots(figsize=(20, 10))  # Crear la figura y los ejes
    #fig, ax = plt.subplots(figsize=(20, 10))  # Ancho=20, Alto=10

    # Asegurarse de que metadata contiene las columnas requeridas
    if 'package' not in metadata.columns or 'cran_release_date' not in metadata.columns:
        raise ValueError("Metadata DataFrame must contain 'package' and 'cran_release_date' columns.")

    # Convertir 'day' y 'cran_release_date' a formato datetime.date para comparación
    dataframe['day'] = pd.to_datetime(dataframe['day']).dt.date
    metadata['cran_release_date'] = pd.to_datetime(metadata['cran_release_date']).dt.date

    # Graficar descargas diarias para cada paquete
    for column in dataframe.columns[1:]:  # Omitir la columna 'day'
        color = color_palette.get(column, "black")  # Mapear color basado en el paquete

        # Buscar la fecha de publicación para el paquete en metadata
        publication_date = metadata.loc[metadata['package'] == column, 'cran_release_date']
        pub_date = publication_date.values[0] if not publication_date.empty else None

        if pub_date:
            # Dividir los datos en dos partes: antes y después de la fecha de lanzamiento
            before_release = dataframe[dataframe['day'] < pub_date]
            after_release = dataframe[dataframe['day'] >= pub_date]

            # Añadir un punto cero en la fecha de lanzamiento a la línea de before_release
            if not before_release.empty:
                new_row = pd.DataFrame({'day': [pub_date], column: [0]})
                before_release = pd.concat([before_release, new_row], ignore_index=True)

            # Graficar partes antes y después del lanzamiento
            ax.plot(before_release['day'], before_release[column], color=color, alpha=0)
            ax.plot(after_release['day'], after_release[column], color=color, label=f"{column}")

            # Marcar fecha de lanzamiento
            if not after_release.empty and pub_date in after_release['day'].values:
                first_download_value = after_release.loc[after_release['day'] == pub_date, column].values[0]
                ax.scatter(pub_date, 0, color='red', zorder=5, s=100, edgecolor='black')
                ax.vlines(pub_date, ymin=0, ymax=first_download_value, colors=color, linestyles='solid')
        else:
            # Si no hay fecha de publicación, graficar normalmente
            ax.plot(dataframe['day'], dataframe[column], color=color, label=column)

    # Añadir "Primer Release" como marcador
    ax.scatter([], [], color='red', s=100, edgecolor='black', label="Primer Release")

    # Personalización del gráfico
    ax.set_title("Descargas Diarias de Paquetes en CRAN", fontsize=26, fontweight='bold', pad=20)

    ax.set_xlabel("Fecha", fontsize=20, fontweight='bold', labelpad=25)  # Ajusta el valor de labelpad

    ax.set_ylabel("Descargas", fontsize=20, fontweight='bold', labelpad=20)
    

    # Formato del eje x
    num_days = (dataframe['day'].max() - dataframe['day'].min()).days
    interval = 1 if num_days <= 15 else 2 if num_days <= 60 else 5
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%b-%d'))
    ax.xaxis.set_major_locator(mdates.DayLocator(interval=interval))
    ax.xaxis.set_minor_locator(mdates.DayLocator(interval=1))
    #plt.xticks(rotation=90)
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
    # Crear una figura y ejes para el gráfico
    fig, ax = plt.subplots(figsize=(16, 6))

    # Convertir 'day' a datetime.date para graficar
    dataframe['day'] = pd.to_datetime(dataframe['day']).dt.date

    # Calcular la suma acumulativa para cada paquete
    cumulative_data = dataframe.copy()
    cumulative_data.iloc[:, 1:] = cumulative_data.iloc[:, 1:].cumsum()

    # Graficar descargas acumulativas para cada paquete
    for column in cumulative_data.columns[1:]:  # Omitir la columna 'day'
        # Obtener el color desde la paleta o asignar un color predeterminado (negro)
        color = color_palette.get(column, "black")
        ax.plot(cumulative_data['day'], cumulative_data[column], label=f"{column}", color=color)

    # Personalización del gráfico
    ax.set_title("Cumulative Daily Downloads for Packages", fontsize=16)
    ax.set_xlabel("Date", fontsize=12)
    ax.set_ylabel("Cumulative Downloads", fontsize=12)

    # Ajustar la leyenda
    handles, labels = ax.get_legend_handles_labels()
    ax.legend(
        handles,
        labels,
        title=None,
        fontsize=10,
        loc="lower center",
        bbox_to_anchor=(0.5, -0.4),
        ncol=4,
        frameon=True,  # Añadir borde
        edgecolor="black",  # Borde negro
    )

    # Personalizar el cuadro de la leyenda
    legend = ax.get_legend()
    legend.get_frame().set_edgecolor("black")  # Borde negro
    legend.get_frame().set_alpha(0.2)  # Transparencia del cuadro (0.0 a 1.0)

    # Configuración adicional
    ax.grid(True, linestyle='--', alpha=0.5)
    
    # Configuración dinámica del eje x
    num_days = (dataframe['day'].max() - dataframe['day'].min()).days
    if num_days <= 15:
        interval = 1
    elif num_days <= 60:
        interval = 2
    else:
        interval = 5

    # Formatear las etiquetas del eje x
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%b-%d'))
    ax.xaxis.set_major_locator(mdates.DayLocator(interval=interval))
    ax.xaxis.set_minor_locator(mdates.DayLocator(interval=1))
    plt.xticks(rotation=90)

    # Ajustar diseño para espacio debajo del gráfico
    plt.tight_layout(rect=[0, 0, 1, 1])

    # Renderizar el gráfico en Streamlit
    st.pyplot(fig)










### PLOT 3



def plot_growth_rate(dataframe, metadata, color_palette):
    fig, ax = plt.subplots(figsize=(16, 6))  # Crear la figura y los ejes

    # Asegurarse de que metadata contiene las columnas requeridas
    if 'package' not in metadata.columns or 'cran_release_date' not in metadata.columns:
        raise ValueError("Metadata DataFrame must contain 'package' and 'cran_release_date' columns.")

    # Convertir columnas a datetime
    dataframe['day'] = pd.to_datetime(dataframe['day']).dt.date
    metadata['cran_release_date'] = pd.to_datetime(metadata['cran_release_date']).dt.date

    # Calcular la tasa de crecimiento para cada paquete
    for column in dataframe.columns[1:]:  # Omitir la columna 'day'
        # Obtener la fecha de lanzamiento
        pub_date = metadata.loc[metadata['package'] == column, 'cran_release_date']
        if pub_date.empty:
            continue  # Saltar si no hay fecha de publicación
        pub_date = pub_date.values[0]

        # Calcular días desde la fecha de lanzamiento

        # Asegurar que 'day' es de tipo datetime.date
        dataframe['day'] = pd.to_datetime(dataframe['day']).dt.date  
        # Calcular días en CRAN (resta entre fechas y extraer días)
        dataframe['days_in_cran'] = (dataframe['day'] - pub_date).apply(lambda x: x.days)

        dataframe['growth_rate'] = dataframe[column] / dataframe['days_in_cran'].replace(0, 1)  # Evitar división por cero

        # Filtrar para días válidos (>= 0)
        valid_data = dataframe[dataframe['days_in_cran'] >= 0]

        # Graficar la tasa de crecimiento
        color = color_palette.get(column, "black")
        ax.plot(valid_data['day'], valid_data['growth_rate'], label=column, color=color)

    # Personalización del gráfico
    ax.set_title("Tasa de Crecimiento de Descargas de Paquetes en CRAN", fontsize=20)
    ax.set_xlabel("Fecha", fontsize=16, labelpad=10)
    ax.set_ylabel("Tasa de Crecimiento (Descargas / Días en CRAN)", fontsize=16, labelpad=10)
    plt.xticks(rotation=90, fontsize=14)
    plt.yticks(fontsize=14)
    ax.legend(fontsize=12, loc="upper left", bbox_to_anchor=(1, 1))

    ax.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()
    st.pyplot(fig)


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
    st.markdown("<div style='margin-top: 50px;'></div>", unsafe_allow_html=True)  

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
            #st.write("Versiones y Fechas en CRAN")
            #text-align: center;
            st.markdown("<h3 style=' font-size:24px; font-weight: bold;'>Versiones y Fechas en CRAN</h3>",unsafe_allow_html=True)

            st.dataframe(metadata if metadata is not None else "No hay metadata.")

            ## VERTICAL SPACE
        st.markdown("<div style='margin-top: 50px;'></div>", unsafe_allow_html=True)

    # Columna derecha: Gráficos
    with col2:
        if packages and start_date and end_date:
            # Recopilar datos de descargas
            all_data = collect_download_data(
                packages, start_date.strftime("%Y-%m-%d"), end_date.strftime("%Y-%m-%d")
            )

            if not all_data.empty:
                # Generar el primer gráfico
                plot_daily_downloads_with_metadata(all_data, metadata, color_palette)

                # Generar el segundo gráfico
                st.write("Gráfico acumulado de descargas diarias para los paquetes seleccionados:")
                plot_cumulative_downloads(all_data, color_palette)

                st.write("Gráfico de Tasa de Crecimiento para los paquetes seleccionados:")
                #plot_growth_rate(all_data, metadata, color_palette)

            else:
                st.write("No se encontraron datos para los paquetes seleccionados en el rango de fechas.")


















# Separador ----------------------------------------------------------------------------------------------------------------------
st.markdown("---")
















# PART 2 -------------------------------------------------------------------------------------------------------------

if packages:
    # Create dynamic buttons
    with st.container():
        cols = st.columns(len(packages))
        for col, package in zip(cols, packages):
            if col.button(package):
                with st.container():
                    st.write(f"### Paquete seleccionado: {package}")
                    st.write(f"**Metadata del paquete:** {package}")
                    st.write(f"**Datos del paquete:** Aquí se mostrarán los datos para {package}")
