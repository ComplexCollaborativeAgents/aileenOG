import json
import os
import uuid
from random import choice
from collections import namedtuple

import settings
from log_config import logging

Color = namedtuple('Color', ['name', 'rgb'])

class AileenObject:

    def __init__(self, shape, color, translation=[0, 0, 0], height_y=settings.OBJECT_STANDARD_HEIGHT,
                 width_x=settings.OBJECT_STANDARD_WIDTH_X, width_z=settings.OBJECT_STANDARD_WIDTH_Z):

        self._shape = shape
        self._color = color
        self._height_y = height_y
        self._width_x = width_x
        self._width_z = width_z
        self._translation = translation
        self._name = "object{}".format(AileenObject.randomizer.uuid4().node)
        self._language = None
        logging.debug("[aileen_object] :: created a new object")

    def __eq__(self, other):
        return self._shape == other._shape and self._color.name == other._color.name

    def __ne__(self, other):
        return not self.__eq__(other)

    def get_object_description(self):
        description = "Solid {\n"
        description += "   translation {} {} {}\n".format(self._translation[0],
                                                   self._translation[1],
                                                   self._translation[2])
        description += "   children [\n"
        description += "       Shape {\n"
        description += "          appearance PBRAppearance {\n"
        description += "          baseColor {} {} {}\n".format(self._color.rgb[0],
                                                               self._color.rgb[1],
                                                               self._color.rgb[2])
        description += "          metalness 0\n"
        description += "          emissiveColor {} {} {}\n".format(self._color.rgb[0],
                                                                   self._color.rgb[1],
                                                                   self._color.rgb[2])
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
    def get_colors():
        with open(settings.COLOR_PATH) as f:
            colors = json.load(f)
        return colors

    @staticmethod
    def generate_random_object():
        scene_object_color = AileenObject.randomizer.get_random_color()
        scene_object_color_vector = AileenObject.randomizer.get_color_vector_sample(scene_object_color)
        color = Color(scene_object_color, scene_object_color_vector)
        scene_object_shape = AileenObject.randomizer.get_random_shape()
        scene_object = AileenObject(shape=scene_object_shape,
                                    color=color)
        scene_object._language = [scene_object_color, scene_object_shape]
        return scene_object

    # Put randomization code in separate class so that it can be overridden.
    class Randomizer:

        def get_random_color(self):
            colors = AileenObject.get_colors().keys()
            return choice(colors)

        def get_color_vector_sample(self,color_symbol):
            return choice(AileenObject.get_colors()[color_symbol])

        def get_random_shape(self):
            return choice(settings.SHAPE_SET)

        def uuid4(self):
            return uuid.uuid4()

    randomizer = Randomizer()  # Allows unit test to override
