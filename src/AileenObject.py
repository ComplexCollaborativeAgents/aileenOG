import json
import os
import random
import sys
import uuid

COLOR_FILE_NAME = 'colors.json'

class AileenObject:

    shape_set= {'box', 'ball', 'cylinder'}
    texture_set = {'smooth', 'rough'}

    def get_colors(self):
        root_dir = os.path.dirname(os.path.abspath(__file__))
        color_file = os.path.join(root_dir, '..', 'resources', COLOR_FILE_NAME)
        with open(color_file) as f:
            colors = json.load(f)
        return colors

    def __init__(self):
        rgb_data = self.get_colors()
        self.object_id = str(uuid.uuid4())
        self.texture = random.choice(tuple(self.texture_set))
        self.shape = random.choice(tuple(self.shape_set))
        self.color = random.choice(rgb_data.keys())
        self.rgb = random.choice(rgb_data[self.color])
        self.object = None

        if self.shape == 'box':
            self.object = Box()
        elif self.shape == 'ball':
            self.object = Ball()
        elif self.shape == 'cylinder':
            self.object = Cylinder()
        elif self.shape == 'cone':
            self.object = Cone()
        self.object.object_id = self.object_id
#        self.object.translation = self.object_id

        self.object.color = [c/255.0 for c in self.rgb]

    def get_yaml(self):
        '''
        For a given object, generate the YAML code that defines it
        for Webots.
        '''
        return self.object.get_yaml()

class Box():

    translation = [0.771, 0.45, -0.199]
    color = [0, 0, 1]
    object_id = ''
    name = 'box-'
    size = [0.1, 0.1, 0.1]
    mass = 0.2

    def get_yaml(self):
        yaml = 'Solid {\n'
        yaml += '  translation {} {} {}\n'.format(*self.translation)
        yaml += '  name "{}"\n'.format(self.name + self.object_id)
        yaml += '  children [\n'
        yaml += '    Shape {\n'
        yaml += '      appearance PBRAppearance {\n'
        yaml += '        emissiveColor {} {} {}\n'.format(*self.color)
        yaml += '      }\n'
        yaml += '      geometry Box {\n'
        yaml += '        size {} {} {}\n'.format(*self.size)
        yaml += '      }\n'
        yaml += '      castShadows FALSE\n'
        yaml += '    }\n'
        yaml += '  ]\n'
        yaml += '  boundingObject Box {\n'
        yaml += '    size {} {} {}\n'.format(*self.size)
        yaml += '  }\n'
        yaml += '  physics Physics {\n'
        yaml += '    mass {}\n'.format(self.mass)
        yaml += '  }\n'
        yaml += '}\n'

        return yaml

class Ball:
    translation = [0.425854, 0.55, 0.174748]
    name = 'ball'
    object_id = ''
    color = [255, 255, 0]
    radius = 0.05
    mass = 0.0
    centerOfMass = [0.0, 0.0, 0.0]

    def get_yaml(self):
        yaml = 'Ball {\n'
        yaml += '  translation {} {} {}\n'.format(*self.translation)
#        yaml += '  name "{}"\n'.format(self.name + self.object_id)
        yaml += '  color {} {} {}\n'.format(*self.color)
        yaml += '  radius {}\n'.format(self.radius)
        yaml += '  centerOfMass [\n'
        yaml += '    {} {} {}\n'.format(*self.centerOfMass)
        yaml += '  ]\n'
        yaml += '}\n'
        return yaml

class Cylinder:
    translation = [0.424854, 0.55, -0.19475]
    color = [255, 255, 0]
    radius = 0.05
    height = 0.1
    name = 'Cylinder'
    mass = 0.2
    object_id = ''

    def get_yaml(self):
        yaml = 'Solid {\n'
        yaml += '  translation {} {} {}\n'.format(*self.translation)
        yaml += '  name "{}"\n'.format(self.name + self.object_id)
        yaml += '  children [\n'
        yaml += '    Shape {\n'
        yaml += '      appearance PBRAppearance {\n'
        yaml += '        emissiveColor {} {} {}\n'.format(*self.color)
        yaml += '      }\n'
        yaml += '      geometry Cylinder {\n'
        yaml += '        height {}\n'.format(self.height)
        yaml += '        radius {}\n'.format(self.radius)
        yaml += '      }\n'
        yaml += '      castShadows FALSE\n'
        yaml += '    }\n'
        yaml += '  ]\n'
        yaml += '  boundingObject Cylinder {\n'
        yaml += '    height {}\n'.format(self.height)
        yaml += '    radius {}\n'.format(self.radius)
        yaml += '  }\n'
        yaml += '  physics Physics {\n'
        yaml += '    mass {}\n'.format(self.mass)
        yaml += '  }\n'
        yaml += '}\n'
        return yaml

class Cone:
    translation = [0.424854, 0.55, -0.19475]
    rotation = [3.5177, 1, -1.49831, 0.275356]
    radius = 0.05
    color = [255, 255, 0]
    height = 0.1
    name = 'Cone'

    def get_yaml(self):
        yaml = 'Solid {\n'
        yaml += '  translation {} {} {}\n'.format(*self.translation)
#        yaml += '  rotation {} {} {} {}\n'.format(*self.rotation)
        yaml += '  name "{}"\n'.format(self.name)
        yaml += '  children [\n'
        yaml += '    Shape {\n'
        yaml += '      appearance PBRAppearance {\n'
        yaml += '        emissiveColor {} {} {}\n'.format(*self.color)
        yaml += '      }\n'
        yaml += '      geometry Cylinder {\n'
        yaml += '        height {}\n'.format(self.height)
        yaml += '        bottomRadius {}\n'.format(self.radius)
        yaml += '      }\n'
        yaml += '      castShadows FALSE\n'
        yaml += '    }\n'
        yaml += '  ]\n'
        yaml += '  boundingObject Cylinder {\n'
        yaml += '    height {}\n'.format(self.height)
        yaml += '    bottomRadius {}\n'.format(self.radius)
        yaml += '  }\n'
        yaml += '}\n'
        return yaml

