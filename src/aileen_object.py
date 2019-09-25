from log_config import logging
import os
import json
from random import choice
import constants
import uuid


class AileenObject:

    def __init__(self, shape, color, translation, height_y=0.1, width_x=0.1, width_z=0.1):
        self._shape = shape
        self._color = color
        self._height_y = height_y
        self._width_x = width_x
        self._width_z = width_z
        self._translation = translation
        self._name = "o{}".format(uuid.uuid4())
        self._language = None #a list of words that can be used to describe the object
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
        description += "    name \"{}\"".format(self._name)
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

    @staticmethod
    def get_random_color():
        colors = AileenObject.get_colors().keys()
        print colors
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


