import random
import sys
import uuid

class AileenObject:

    shape_set= {'box', 'ball', 'cylinder', 'cone'}
    texture_set = {'smooth', 'rough'}
    color_set = {'yellow', 'red', 'green', 'blue'}
    rgb_data = {
        'yellow': [
            (255, 255, 0),
            (238, 238, 0),
            (205, 205, 0),
            (255, 236, 139)
         ],
        'red': [
            (255, 0, 0),
            (238, 0, 0),
            (205, 0, 0),
            (139, 0, 0)
         ],
        'green': [
            (0, 128, 0),
            (0, 238, 0),
            (0, 205, 0),
            (50, 205, 50)
         ],
        'blue': [
            (0, 191, 255),
            (0, 178, 238),
            (0, 154, 205),
            (30, 144, 255)
         ]
    }

    def __init__(self):
        self.object_id = str(uuid.uuid4())
        self.texture = random.choice(tuple(self.texture_set))
        self.shape = random.choice(tuple(self.shape_set))
        self.color = random.choice(tuple(self.color_set))
        self.rgb = random.choice(self.rgb_data[self.color])
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

