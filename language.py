#
# Language mapping from world symbols to English words.
#
import json

COLORS_FILE = '../aileen-instructor/resources/colors.json'

shape_table = {
    's_box': 'box',
    's_cone': 'cone',
    's_sphere': 'ball',
    's_cylinder': 'cylinder'
}

texture_table = {
    's_smooth': 'smooth',
    's_rough': 'rough'
}

color_table = {}

def load_colors():
    with open(COLORS_FILE) as f:
        colors = json.load(f)
    for color_name in colors:
        color_table[color_name] = []
        for rgb_list in colors[color_name]:
            rgb_key = " ".join(str(c) for c in rgb_list)
            color_table[rgb_key] = color_name

def get_shape_name(shape_symbol):
    return shape_table.get(shape_symbol, 'unknown')

def get_texture_name(texture_symbol):
    return texture_table.get(texture_symbol, 'unknown')

def get_color_name(rgb_values):
    if len(color_table) < 1:
        load_colors()
    rgb_key = " ".join(str(c) for c in rgb_values)
    return color_table.get(rgb_key, 'unknown')

