import json
import os
import random
import sys
import uuid

COLOR_FILE_NAME = 'colors.json'

class AileenObject:

    shape_set= {'box', 'ball', 'cylinder', 'cone'}
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

        self.object.emissiveColor = self.rgb

    def get_yaml(self):
        '''For a given object, generate the YAML code that defines it'''


class Box:
    translation = []
    rotation = []
    name = "box"
    size = []
    contactMaterial = "default"
    appearance = {}
    physics = 0
    castShadows = True
    emissiveColor = ()

class Ball:
    translation = []
    rotation = []
    name = "ball"
    color = []
    radius = 0.0
    mass = 0.0
    centerOfMass = []
    linearDamping = 0.17
    angularDamping = 0.33
    emissiveColor = ()

class Cylinder:
    bottomRadius = 0.05
    height = 0.1
    emissiveColor = ()

class Cone:
    bottomRadius = 0.05
    height = 0.1
    emissiveColor = ()

