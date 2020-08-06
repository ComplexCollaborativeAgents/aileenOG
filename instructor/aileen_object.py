import json
import uuid
from collections import namedtuple
from random import choice
import settings
from log_config import logging

Color = namedtuple('Color', ['name', 'rgb'])
Size = namedtuple('Size', ['name', 'xyz'])

class AileenObject:

    #def __init__(self, shape, color, translation=[0, 0, 0], height_y=settings.OBJECT_STANDARD_HEIGHT,
    #             width_x=settings.OBJECT_STANDARD_WIDTH_X, width_z=settings.OBJECT_STANDARD_WIDTH_Z):
    def __init__(self, shape, color, size, translation=[0, 0, 0]):
        self._shape = shape
        self._color = color
        self._size = size
        self._height_y = self._size.xyz[1]
        self._width_x = self._size.xyz[0]
        self._width_z = self._size.xyz[2]
        self._translation = translation
        self._name = "object{}".format(AileenObject.randomizer.uuid4())
        self._language = None
        self._connector_dim = None
        logging.debug("[aileen_object] :: created a new object")

    def __eq__(self, other):
        return self._shape == other._shape and self._color.name == other._color.name and self._size.name == other._size.name

    def __ne__(self, other):
        return not self.__eq__(other)

    def get_object_description(self):
        description = "Solid {\n"
        description += "   recognitionColors {} {} {}\n".format(self._color.rgb[0],
                                                                self._color.rgb[1],
                                                                self._color.rgb[2])
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
        description += "        Connector {\n"
        description += '          type "passive"\n'
        description += "          distanceTolerance .1\n"
        description += "          axisTolerance .5\n"
        description += "          rotationTolerance 0\n"
        description += "          numberOfRotations 0\n"
        description += "          rotation 1 0 0 -1.57\n"
        description += "          translation 0 {} 0\n".format(self._connector_dim)
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
            self._connector_dim = self._height_y/2
            return description

        if self._shape == "box":
            description += "          size {} {} {}\n".format(self._width_x, self._height_y, self._width_z)
            description += "        }"
            self._connector_dim = self._height_y/2
            return description

        if self._shape == "cylinder":
            description += "          radius {}\n".format(self._width_x / 2)
            description += "          height {}\n".format(self._height_y)
            description += "        }"
            self._connector_dim = self._height_y/2
            return description

        if self._shape == "sphere":
            description += "          radius {}\n".format(self._width_x / 2)
            description += "          subdivision 3\n"
            description += "        }"
            self._connector_dim = self._width_x/2
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
    def get_sizes():
        with open(settings.SIZE_PATH) as f:
            sizes = json.load(f)
        return sizes

    @staticmethod
    def generate_random_object():
        scene_object_color = AileenObject.randomizer.get_random_color()
        scene_object_color_vector = AileenObject.randomizer.get_color_vector_sample(scene_object_color)
        color = Color(scene_object_color, scene_object_color_vector)

        scene_object_shape = AileenObject.randomizer.get_random_shape()

        scene_object_size = AileenObject.randomizer.get_random_size()
        scene_object_color_vector = AileenObject.randomizer.get_size_vector_sample(scene_object_size)
        size = Size(scene_object_size, scene_object_color_vector)

        scene_object = AileenObject(shape=scene_object_shape,
                                    color=color,
                                    size=size)
        scene_object._language = [scene_object_color, scene_object_shape, size.name]
        return scene_object

    @staticmethod
    def generate_object(description):
        color = description.get('color', AileenObject.randomizer.get_random_color())
        scene_object_color_vector = description.get('rgb', AileenObject.randomizer.get_color_vector_sample(color))
        color = Color(color, scene_object_color_vector)
        shape = description.get('shape', AileenObject.randomizer.get_random_shape())
        size = description.get('size', AileenObject.randomizer.get_random_size())
        size_vector = description.get('xyz', AileenObject.randomizer.get_size_vector_sample(size))
        size = Size(size, size_vector)
        scene_object = AileenObject(shape=shape, color=color, size=size)
        scene_object._language = [color.name, shape, size.name]
        return scene_object


    @staticmethod
    def generate_random_objects(n):
        """Generate n random unique objects."""
        colors = AileenObject.get_colors().keys()
        shapes = settings.SHAPE_SET
        sizes = AileenObject.get_sizes().keys()
        objects = []
        if n > len(colors) * len(shapes) * len(sizes):
            logging.error("[aileen_object] :: Can't generate more than {} unique random objects".format(
                len(colors) * len(shapes) * len(sizes)))
            return objects

        while n:
            o = AileenObject.generate_random_object()
            if o not in objects:
                objects.append(o)
                n -= 1
        return objects

    @staticmethod
    def generate_distractors(target, n):
        """Generate distractors. A distractor is an object that does not have the same color or shape as the target
        object(s).

        Parameters
        ----------
        target : Union[AileenObject, list[AileenObject]]
        n : int
        """
        distractors = []
        while n:
            distractor = AileenObject.generate_random_object()
            if (isinstance(target, list) and distractor not in target) or \
                    (not isinstance(target, list) and distractor != target):
                distractors.append(distractor)
                n -= 1

        return distractors

    # Put randomization code in separate class so that it can be overridden.
    class Randomizer:

        def get_random_color(self):
            colors = AileenObject.get_colors().keys()
            return choice(colors)

        def get_color_vector_sample(self, color_symbol):
            return choice(AileenObject.get_colors()[color_symbol])

        def get_size_vector_sample(self, size_name):
            return choice(AileenObject.get_sizes()[size_name])

        def get_random_size(self):
            sizes = AileenObject.get_sizes().keys()
            return choice(sizes)

        def get_random_shape(self):
            return choice(settings.SHAPE_SET)

        def uuid4(self):
            return uuid.uuid4().node

    randomizer = Randomizer()  # Allows unit test to override
