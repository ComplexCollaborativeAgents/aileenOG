import json
import os
import uuid
from random import choice

import constants
from log_config import logging

class AileenObject:

    test_id = None # class variable used for unit test

    def __init__(self, shape, color, translation=[0, 0, 0], height_y=constants.OBJECT_STANDARD_HEIGHT,
                 width_x=constants.OBJECT_STANDARD_WIDTH_X, width_z=constants.OBJECT_STANDARD_WIDTH_Z):

        self._shape = shape
        self._color = color
        self._height_y = height_y
        self._width_x = width_x
        self._width_z = width_z
        self._translation = translation
        self._name = "{}".format(uuid.uuid4())
        self._language = None
        logging.debug("[aileen_object] :: created a new object")

    def get_object_description(self):
        description = "Solid {\n"
        description += "   translation {} {} {}\n".format(self._translation[0],
                                                   self._translation[1],
                                                   self._translation[2])
        description += "   children [\n"
        description += "       Shape {\n"
        description += "          appearance PBRAppearance {\n"
        description += "          baseColor {} {} {}\n".format(self._color[0],
                                                               self._color[1],
                                                               self._color[2])
        description += "          metalness 0\n"
        description += "          emissiveColor {} {} {}\n".format(self._color[0],
                                                                   self._color[1],
                                                                   self._color[2])
        description += "        }\n"
        description += "        geometry {}\n".format(self.get_geometry_description())
        description += "        castShadows FALSE\n"
        description += "        }\n"
        description += "    ]\n"
        description += "    name \"{}\"\n".format(self._name)
        description += "   boundingObject {}".format(self.get_bounding_object_description())
        description += "   physics Physics {\n}"
        description += "}"

        logging.debug("[aileen_object] :: added string {}".format(description))

        return description

    def get_bounding_object_description(self):
        description = "Box {\n"
        description += "     size {} {} {}\n".format(self._width_x, self._height_y, self._width_z)
        description += "   }\n"
        return description

    def get_geometry_description(self):
        description = "{} {{\n".format(self._shape.title())

        if self._shape == "cone":
            description += "          bottomRadius {}\n".format(self._width_x / 2)
            description += "          height {}\n".format(self._height_y)
            description += "        }"
            return description

        if self._shape == "box":
            description += "          size {} {} {}\n".format(self._width_x, self._height_y, self._width_z)
            description += "        }"
            return description

        if self._shape == "cylinder":
            description += "          radius {}\n".format(self._width_x / 2)
            description += "          height {}\n".format(self._height_y)
            description += "        }"
            return description

        if self._shape == "sphere":
            description += "          radius {}\n".format(self._width_x / 2)
            description += "          subdivision 3\n"
            description += "        }"
            return description

    def set_translation(self, position_vector):
        logging.debug("[aileen_object] :: setting translation of object to {}".format(position_vector))
        self._translation = position_vector

    def set_language(self, word_list):
        self._language = word_list

    @staticmethod
    def get_random_color():
        colors = AileenObject.get_colors().keys()
        return choice(colors)

    @staticmethod
    def get_color_vector_sample(color_symbol):
        return choice(AileenObject.get_colors()[color_symbol])

    @staticmethod
    def get_colors():
        root_dir = os.path.dirname(os.path.abspath(__file__))
        color_file = os.path.join(root_dir, '..', 'resources', constants.COLOR_FILE_NAME)
        with open(color_file) as f:
            colors = json.load(f)
        return colors

    @staticmethod
    def get_random_shape():
        return choice(constants.SHAPE_SET)

    @staticmethod
    def generate_random_object():
        scene_object_color = AileenObject.get_random_color()
        scene_object_color_vector = AileenObject.get_color_vector_sample(scene_object_color)
        scene_object_shape = AileenObject.get_random_shape()
        # Fix the results for unit testing.
        if AileenObject.test_id == 1:
            scene_object_color = 'blue'
            scene_object_color_vector = [0, 0, 1]
            scene_object_shape = 'cylinder'
        elif AileenObject.test_id == 2:
            scene_object_color = 'blue'
            scene_object_color_vector = [0, 0, 1]
            scene_object_shape = 'box'
        scene_object = AileenObject(shape=scene_object_shape,
                                    color=scene_object_color_vector)
        if AileenObject.test_id != None:
            scene_object._name = AileenObject.test_id
            AileenObject.test_id += 1
        scene_object._language = [scene_object_color, scene_object_shape]
        return scene_object


