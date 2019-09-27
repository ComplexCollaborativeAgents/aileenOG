from log_config import logging

from aileen_object import AileenObject
from aileen_scene import AileenScene
import constants
import os, json, sys
from random import choice
from configuration import Configuration
from shapely.geometry import box
from language_generator import LanguageGenerator

try:
    from qsrlib_io.world_trace import Object_State, World_State
    from qsrlib.qsr_realization import compute_region_for_relations, sample_position_from_region
except:
    logging.fatal("[input_writer] :: cannot find spatial reasoning library")
    exit()


class AileenSpatialWordLesson:

    def __init__(self):
        self._spatial_configurations_set = AileenSpatialWordLesson.get_spatial_configurations_set()
        self._spatial_configuration = choice(self._spatial_configurations_set.keys())
        self._spatial_configuration_def = self._spatial_configurations_set[self._spatial_configuration]
        self._scene = AileenScene()
        self._scene = AileenScene()
        self._language = None
        pass

    def generate_lesson(self):
        lesson = {}
        self.generate_scene()
        lesson['scene'] = self._scene.generate_scene_description()
        lesson['interaction'] = self._language
        return lesson

    def generate_scene(self):
        world = World_State(0.0)
        table = box(constants.OBJECT_POSITION_MIN_X,
                    constants.OBJECT_POSITION_MIN_Z,
                    constants.OBJECT_POSITION_MAX_X,
                    constants.OBJECT_POSITION_MAX_Z)

        logging.debug("[aileen_spatial_word_lesson] :: generating a new scene for spatial word learning")
        position1 = AileenScene.get_random_position_on_table()
        scene_object1 = AileenObject.generate_random_object_at(position1)

        position2 = AileenScene.get_random_position_on_table()
        scene_object2 = AileenObject.generate_random_object_at(position2)

        translation = AileenScene.get_position_in_spatial_configuration(scene_object1,
                                                                        scene_object2,
                                                                        self._spatial_configuration_def)

        scene_object2.set_translation(translation)

        self._scene.add_object(scene_object1)
        self._scene.add_object(scene_object2)

        self._language = LanguageGenerator.generate_language_for_spatial_relation(scene_object1,
                                                                                  self._spatial_configuration,
                                                                                  scene_object2)

    @staticmethod
    def get_spatial_configurations_set():
        root_dir = os.path.dirname(os.path.abspath(__file__))
        spatial_configuration_file = os.path.join(root_dir, '..', 'resources',
                                                  constants.SPATIAL_CONFIGURATION_FILE_NAME)
        with open(spatial_configuration_file) as f:
            spatial_configurations = json.load(f)
        return spatial_configurations


if __name__ == '__main__':
    lesson1 = AileenSpatialWordLesson()
    print(lesson1.generate_lesson())
