import json
import os
from random import choice

import settings
from aileen_object import AileenObject, Color


from aileen_scene import AileenScene
from language_generator import LanguageGenerator
from log_config import logging
from collections import OrderedDict


class SpatialWordLesson:

    def __init__(self, configuration=None):
        self._spatial_configurations_set = SpatialWordLesson.get_spatial_configurations_set()
        if configuration:
            self._spatial_configuration = configuration
        else:
            self._spatial_configuration = SpatialWordLesson.randomizer.random_spatial_configuration(
                self._spatial_configurations_set.keys())
        self._spatial_configuration_def = self._spatial_configurations_set[self._spatial_configuration]
        self._scene_objects = OrderedDict()
        self._scene = AileenScene()
        self._language = None

    def generate_lesson(self, objects=None, distractors=0):
        self.generate_setup(objects)

        positions = [o.get('position', None) for o in objects]
        if not all(positions):
            positions = []

        self.generate_scene(positions, distractors)
        lesson = {
            'scene': self._scene.generate_scene_description(),
            'interaction': {
                'signal': 'verify',
                'content': self._language
            }
        }
        return lesson

    def generate_setup(self, object_descriptions):
        logging.debug("[action_word_lesson] :: generate the setup for the new lesson")
        objects = self._spatial_configuration_def[settings.SPATIAL_DEF_OBJECTS]
        if object_descriptions:
            for o, desc in zip(objects, object_descriptions):
                self._scene_objects[o] = AileenObject.generate_object(desc)
        elif len(objects) > 0:
            objs = AileenObject.generate_random_objects(len(objects))
            for o, obj in zip(objs, objects):
                self._scene_objects[obj] = o
        self._language = LanguageGenerator.generate_language_from_template(self._scene_objects,
                                                                           self._spatial_configuration_def[
                                                                               settings.SPATIAL_DEF_LANGUAGE_TEMPLATE])

    def generate_scene(self, positions, distractors):
        logging.debug("[aileen_spatial_word_lesson] :: generating a new scene for spatial word learning")

        if len(positions) == len(self._scene_objects):
            for o, p in zip(self._scene_objects.values(), positions):
                o.set_translation(p)
                self._scene.add_object(o)
        elif len(self._scene_objects) == 2:
            translations = AileenScene.place_two_objects_in_configuration(
                target_object_name=self._scene_objects.items()[0][0],
                reference_object_name=self._scene_objects.items()[1][0],
                scene_objects=self._scene_objects,
                configuration_definition=self._spatial_configuration_def[settings.SPATIAL_DEF_DEFINITION])
            for scene_object_name in self._scene_objects.keys():
                scene_object = self._scene_objects[scene_object_name]
                scene_object.set_translation(translations[scene_object_name])
                self._scene.add_object(scene_object)
        elif len(self._scene_objects) == 3:
            print(self._scene_objects.items()[0][0], self._scene_objects.items()[1][0])
            translations = AileenScene.place_three_objects_in_configuration(
                target_object_name=self._scene_objects.items()[2][0],
                first_reference_object_name=self._scene_objects.items()[0][0],
                second_reference_object_name=self._scene_objects.items()[1][0],
                scene_objects=self._scene_objects,
                configuration_definition=self._spatial_configuration_def[settings.SPATIAL_DEF_DEFINITION])
            for scene_object_name in self._scene_objects.keys():
                scene_object = self._scene_objects[scene_object_name]
                scene_object.set_translation(translations[scene_object_name])
                self._scene.add_object(scene_object)

        for distractor in AileenObject.generate_distractors(self._scene_objects.values(), distractors):
            distractor.set_translation(AileenScene.randomizer.get_random_position_on_table())
            self._scene.add_object(distractor)

    @staticmethod
    def get_spatial_configurations_set():
        root_dir = os.path.dirname(os.path.abspath(__file__))
        spatial_configuration_file = os.path.join(root_dir, 'resources',
                                                  settings.SPATIAL_CONFIGURATION_FILE_NAME)
        with open(spatial_configuration_file) as f:
            spatial_configurations = json.load(f)
        return spatial_configurations

    def evaluate_agent_response(self, agent_response):
        if agent_response['status'] == 'success':
            return {'signal': 'correct'}

    @staticmethod
    def administer_curriculum(world_server, agent_server):
        while True:
            raw_input("Press any key to generate the next spatial word lesson...")

            lesson_object = SpatialWordLesson()
            lesson = lesson_object.generate_lesson(distractors=0)


            scene_acknowledgement = world_server.set_scene(
                {'configuration': lesson['scene'], 'label': lesson['interaction']['content']})
            logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))

            agent_response = agent_server.process_interaction(lesson['interaction'])
            logging.info("[aileen_instructor] :: received from agent {}".format(agent_response))

            evaluation = lesson_object.evaluate_agent_response(agent_response)
            agent_response = agent_server.process_interaction(evaluation)
            logging.info("[aileen_instructor] :: provided feedback to agent")

    class Randomizer:

        def random_spatial_configuration(self, configurations):
            return choice(configurations)

    randomizer = Randomizer()


if __name__ == '__main__':
    lesson1 = SpatialWordLesson().generate_lesson()
    print(lesson1)
