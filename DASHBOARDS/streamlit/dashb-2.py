import streamlit as st
import requests
import pandas as pd
import matplotlib.pyplot as plt
import itertools
from datetime import datetime

# Configurar la página
st.set_page_config(
    page_title="Dashboard Dinámico",
    page_icon=":bar_chart:",
    layout="wide"
)

# Función para obtener datos de descargas de CRAN
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
    except requests.RequestException:
        return None

# Función para obtener metadatos de un paquete desde CRAN DB
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
    except requests.RequestException:
        return pd.DataFrame({
            "package": [package_name],
            "version": [None],
            "first_cran_date": [None]
        })

# Función para graficar las descargas diarias
def plot_daily_downloads(dataframe):
    plt.figure(figsize=(10, 4))
    dataframe['day'] = pd.to_datetime(dataframe['day']).dt.date
    for column in dataframe.columns[1:]:
        plt.plot(dataframe['day'], dataframe[column], label=column)
    plt.title("Descargas Diarias")
    plt.xlabel("Fecha")
    plt.ylabel("Descargas")
    plt.legend()
    plt.grid(True, linestyle="--", alpha=0.6)
    st.pyplot(plt)

# ENCABEZADO -----------------------------------------------------------------------------------------------------------
# Encabezado simple
# st.write("## TÍTULO")

# BARRA DE BUSQUEDA ----------------------------------------------------------------------------------------------------
st.markdown(
    """
    <style>
    div[data-testid="stTextInput"] {
        width: 50%; 
        margin: 0 auto;
    }
    div[data-testid="stTextInput"] input {
        text-align: center;
    }
    </style>
    """,
    unsafe_allow_html=True,
)
input_packages = st.text_input("", "vaccineff, sivirep, serofoi", placeholder="Buscar paquetes...")
packages = list(dict.fromkeys(pkg.strip() for pkg in input_packages.split(",") if pkg.strip()))

# Filtrar paquetes que están en CRAN
if packages:
    cran_packages = [pkg for pkg in packages if get_package_metadata(pkg).empty is False]
    packages = cran_packages

# PARTE 1 --------------------------------------------------------------------------------------------------------------
with st.container():
    col1, col2 = st.columns([1, 2])

    # Columna izquierda: Fecha
    with col1:
        start_date = st.date_input("Fecha de Inicio", value=datetime(2024, 1, 1))
        end_date = st.date_input("Fecha de Fin", value=datetime(2024, 12, 31))
        st.write("Texto Dinámico")

    # Columna derecha: Gráfico
    with col2:
        

# Separador ------------------------------------------------------------------------------------------------------------
st.markdown("---")

# PARTE 2 --------------------------------------------------------------------------------------------------------------
if packages:
    with st.container():
        cols = st.columns(len(packages))
        selected_package = None
        for col, package in zip(cols, packages):
            if col.button(package, key=f"btn-{package}"):
                selected_package = package

    if selected_package:
        st.write(f"Paquete seleccionado: {selected_package}")
        df = get_download_data(selected_package, start_date.strftime("%Y-%m-%d"), end_date.strftime("%Y-%m-%d"))
        if df is not None:
            plot_daily_downloads(df)
        else:
            st.write("No se encontraron datos para este paquete.")
