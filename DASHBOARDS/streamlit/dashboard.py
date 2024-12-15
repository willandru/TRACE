import streamlit as st
import requests
import pandas as pd
import plotly.express as px
from datetime import datetime




# Configurar la página
st.set_page_config(
    page_title="Dashboard Dinámico",
    page_icon=":bar_chart:",
    layout="wide"
)




#ENCABEZADO--------------------------------------------------------------------------------------------------------------

# Encabezado simple
#st.write("## TÍTULO")






# BARRA DE BUSQUEDA -------------------------------------------------------------------------------------------------------

# CSS para centrar y reducir el ancho del input
st.markdown(
    """
    <style>
    div[data-testid="stTextInput"] {
        width: 50%; /* Ajusta el ancho aquí */
        margin: 0 auto; /* Centra el input */
    }
    div[data-testid="stTextInput"] input {
        text-align: center; /* Centra el texto dentro del cuadro */
    }
    </style>
    """,
    unsafe_allow_html=True,
)
# Entrada de paquetes con estilo centrado y ancho reducido
input_packages = st.text_input("", "vaccineff, sivirep, serofoi", placeholder="Buscar paquetes...")


# Procesar paquetes
packages = list(dict.fromkeys(pkg.strip() for pkg in input_packages.split(",") if pkg.strip()))


#VERIFICAR QUE LOS PAQUETES ESTEN EN CRAN

# Función para verificar si un paquete está en CRAN
def is_package_in_cran(package_name):
    try:
        url = f"https://crandb.r-pkg.org/{package_name}/all"
        response = requests.get(url, timeout=5)
        return response.status_code == 200  # El paquete está en CRAN si la respuesta es 200
    except requests.RequestException:
        return False  # Retornar False si ocurre algún error

# Filtrar paquetes que están en CRAN
if packages:
    cran_packages = [pkg for pkg in packages if is_package_in_cran(pkg)]
    packages = cran_packages  # Actualizar la variable 'packages' con los paquetes válidos







# PARTE 1 ---------------------------------------------------------------------------------------------------------------

# Panel superior: Fecha y gráficos
with st.container():
    col1, col2 = st.columns([1, 2])
    
    # Columna izquierda
    with col1:
        start_date = st.date_input("Fecha de Inicio", value=None)
        end_date = st.date_input("Fecha de Fin", value=None)
        st.write("Texto Dinámico")
    
    # Columna derecha: Gráficos
    with col2:
        st.write("Gráfica 1")
        st.write("Gráfica 2")






# Separador -------------------------------------
st.markdown("---")










# PARTE 2 -------------------------------------------------------------------------------------------------------------
# Fondo del menú dinámico
if packages:  # Verificar que haya paquetes válidos
    with st.container():
        cols = st.columns(len(packages))  # Generar columnas dinámicamente
        selected_package = None
        for col, package in zip(cols, packages):
            if col.button(package, key=f"btn-{package}"):
                selected_package = package

    # Panel principal
    if selected_package:
        st.write(f"Paquete seleccionado: {selected_package}")
