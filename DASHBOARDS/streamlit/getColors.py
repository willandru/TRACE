import matplotlib.pyplot as plt
from matplotlib.colors import rgb2hex  # Importar la función para convertir a HEX

def get_first_colors_hex(cmap_name, num_colors=10):
    """
    Returns the first `num_colors` from a given colormap in HEX format.
    
    Parameters:
    cmap_name (str): Name of the colormap.
    num_colors (int): Number of colors to extract.
    
    Returns:
    list: List of HEX color strings.
    """
    cmap = plt.get_cmap(cmap_name)
    return [rgb2hex(cmap(i / (num_colors - 1))) for i in range(num_colors)]

def plot_colormap_colors(cmap_name, num_colors=10):
    """
    Plots the first `num_colors` from a given colormap.
    
    Parameters:
    cmap_name (str): Name of the colormap.
    num_colors (int): Number of colors to plot.
    """
    colors = get_first_colors_hex(cmap_name, num_colors)
    
    fig, ax = plt.subplots(figsize=(8, 2))
    for i, color in enumerate(colors):
        ax.add_patch(plt.Rectangle((i, 0), 1, 1, color=color))
    ax.set_xlim(0, num_colors)
    ax.set_ylim(0, 1)
    ax.axis('off')
    plt.title(f'First {num_colors} Colors of {cmap_name}', fontsize=14)
    plt.show()
    
    # Print colors in HEX format
    print("Hexadecimal Colors:")
    for i, color in enumerate(colors):
        print(f"{i+1}: {color}")

# Example usage with "Set2"
colors_hex = get_first_colors_hex("Set2", 10)
print("Colores en HEX:", colors_hex)
plot_colormap_colors("Set2", 10)
