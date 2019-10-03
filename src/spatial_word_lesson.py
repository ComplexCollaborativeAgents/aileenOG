import json
import os
from random import choice

import constants
from aileen_object import AileenObject
from aileen_scene import AileenScene
from language_generator import LanguageGenerator
from log_config import logging


class SpatialWordLesson:

    def __init__(self):
        self._spatial_configurations_set = SpatialWordLesson.get_spatial_configurations_set()
        self._spatial_configuration = choice(self._spatial_configurations_set.keys())
        self._spatial_configuration_def = self._spatial_configurations_set[self._spatial_configuration]
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
        logging.debug("[aileen_spatial_word_lesson] :: generating a new scene for spatial word learning")
        scene_object1 = AileenObject.generate_random_object()
        scene_object2 = AileenObject.generate_random_object()

        translation1, translation2 = AileenScene.place_objects_in_configuration(scene_object1, scene_object2,
                                                                                self._spatial_configuration_def)

        scene_object1.set_translation(translation1)
        scene_object2.set_translation(translation2)

        self._scene.add_object(scene_object1)
        self._scene.add_object(scene_object2)

        self._language = LanguageGenerator.generate_language_for_spatial_relation(arg1=scene_object1,
                                                                                  arg2=scene_object2,
                                                                                  relation=self._spatial_configuration)

    @staticmethod
    def get_spatial_configurations_set():
        root_dir = os.path.dirname(os.path.abspath(__file__))
        spatial_configuration_file = os.path.join(root_dir, '..', 'resources',
                                                  constants.SPATIAL_CONFIGURATION_FILE_NAME)
        with open(spatial_configuration_file) as f:
            spatial_configurations = json.load(f)
        return spatial_configurations

    @staticmethod
    def administer_curriculum(world_server, agent_server):
        while True:
            raw_input("Press any key to generate the next spatial word lesson...")

            lesson = SpatialWordLesson().generate_lesson()

            scene_acknowledgement = world_server.set_scene(
                {'configuration': lesson['scene'], 'label': lesson['interaction']})
            logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))

            language_acknowledgement = agent_server.process_language(lesson['interaction'])
            logging.info("[aileen_instructor] :: received from agent {}".format(language_acknowledgement))


if __name__ == '__main__':
    lesson1 = SpatialWordLesson()
    print(lesson1.generate_lesson())
