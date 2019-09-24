from aileen_object import AileenObject
from aileen_scene import AileenScene
from random import uniform, choice
import constants
from log_config import logging
import os
import json

class AileenVisualWordLesson:

    def __init__(self):
        self._scene = AileenScene()
        self._language = {}
        self._colors = self.get_colors()
        self._shapes = constants.SHAPE_SET

    def generate_lesson(self):
        lesson = {}
        self.generate_scene()

        lesson['scene'] = self._scene.generate_scene_description()
        lesson['interaction'] = self._language

        return lesson


    def generate_scene(self):
        logging.debug("[aileen_visual_word_lesson] :: generating a new scene for visual word learning")
        scene_object_color = choice(self._colors.keys())
        scene_object_color_vector = choice(self._colors[scene_object_color])
        scene_object_shape = choice(self._shapes)
        scene_object = AileenObject(shape=scene_object_shape,
                                    color=scene_object_color_vector)
        scene_object.set_translation(self.get_random_position_on_table())
        self._scene.add_object(scene_object)

        self._language['language'] = "{} {}".format(scene_object_color, scene_object_shape)


    @staticmethod
    def get_random_position_on_table():
        position = [uniform(constants.OBJECT_POSITION_MIN_X, constants.OBJECT_POSITION_MAX_X),
                    uniform(constants.OBJECT_POSITION_MIN_Y, constants.OBJECT_POSITION_MAX_Y),
                    uniform(constants.OBJECT_POSITION_MIN_Z, constants.OBJECT_POSITION_MAX_Z)]
        return position

    @staticmethod
    def get_colors():
        root_dir = os.path.dirname(os.path.abspath(__file__))
        color_file = os.path.join(root_dir, '..', 'resources', constants.COLOR_FILE_NAME)
        with open(color_file) as f:
            colors = json.load(f)
        return colors


if __name__ == '__main__':
    lesson1 = AileenVisualWordLesson()
    print(lesson1.generate_lesson())
