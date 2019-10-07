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
        self._scene_objects = {}
        self._scene = AileenScene()
        self._language = None

    def generate_lesson(self):
        self.generate_setup()
        self.generate_scene()
        lesson = {
            'scene': self._scene.generate_scene_description(),
            'interaction': {
                'language': self._language
            }
        }
        return lesson

    def generate_setup(self):
        logging.debug("[action_word_lesson] :: generate the setup for the new lesson")
        objects = self._spatial_configuration_def[constants.SPATIAL_DEF_OBJECTS]
        if len(objects) > 0:
            for obj in objects:
                self._scene_objects[obj] = AileenObject.generate_random_object()
        self._language = LanguageGenerator.generate_language_from_template(self._scene_objects, self._spatial_configuration_def[constants.SPATIAL_DEF_LANGUAGE_TEMPLATE])


    def generate_scene(self):
        logging.debug("[aileen_spatial_word_lesson] :: generating a new scene for spatial word learning")
        translations = AileenScene.place_objects_in_configuration(self._scene_objects, self._spatial_configuration_def[constants.SPATIAL_DEF_DEFINITION])
        for scene_object_name in self._scene_objects.keys():
            scene_object = self._scene_objects[scene_object_name]
            scene_object.set_translation(translations[scene_object_name])
            self._scene.add_object(scene_object)

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
                {'configuration': lesson['scene'], 'label': lesson['interaction']['language']})
            logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))

            language_acknowledgement = agent_server.process_language(lesson['interaction'])
            logging.info("[aileen_instructor] :: received from agent {}".format(language_acknowledgement))


if __name__ == '__main__':
    lesson1 = SpatialWordLesson().generate_lesson()
    print(lesson1)

