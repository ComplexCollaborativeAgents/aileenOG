import json
import os
from random import choice

import settings
from aileen_object import AileenObject
from copy import deepcopy


from aileen_scene import AileenScene
from language_generator import LanguageGenerator
from log_config import logging
from collections import OrderedDict


class SpatialWordLesson:

    def __init__(self, is_positive, signal, description, distractors, content):
        self._spatial_configurations_set = SpatialWordLesson.get_spatial_configurations_set()

        self._description = description
        configuration = None
        if self._description:
            configuration = description.get("relation", None)
        if configuration:
            self._spatial_configuration = configuration
        else:
            self._spatial_configuration = SpatialWordLesson.randomizer.random_spatial_configuration(
                self._spatial_configurations_set.keys())

        self._is_positive = is_positive
        self._spatial_configuration_def = self._spatial_configurations_set[self._spatial_configuration]



        if not is_positive:
            other_spatial_configs = deepcopy(self._spatial_configurations_set.keys())
            other_spatial_configs.remove(self._spatial_configuration)
            self._spatial_configuration_negative = SpatialWordLesson.randomizer.random_spatial_configuration(
                other_spatial_configs)
            self._spatial_configuration_def_negative = self._spatial_configurations_set[self._spatial_configuration_negative]

        self._scene_objects = OrderedDict()
        self._scene = AileenScene()
        self._language = None

        self._signal = signal


        self._distractors = distractors
        self._content = content

    def generate_lesson(self):
        objects = None
        if self._description:
            objects = self._description.get("objects", None)
        if objects is None:
            objects = []
        self.generate_setup(objects)


        positions = [o.get('position', None) for o in objects]
        if not all(positions):
            positions = []

        self.generate_scene(positions)
        lesson = {
            'scene': self._scene.generate_scene_world_config(),
            'interaction': {
                'signal': self._signal,
                'content': self._content if self._content is not None else self._language
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
        if self._is_positive:
            self._language = LanguageGenerator.generate_language_from_template(self._scene_objects,
                                                                               self._spatial_configuration_def[
                                                                                   settings.SPATIAL_DEF_LANGUAGE_TEMPLATE])
        else:
            self._language = LanguageGenerator.generate_language_from_template(self._scene_objects,
                                                                               self._spatial_configuration_def_negative[settings.SPATIAL_DEF_LANGUAGE_TEMPLATE])

    def generate_scene(self, positions):
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

        for distractor in AileenObject.generate_distractors(self._scene_objects.values(), self._distractors):
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
        if self._is_positive:
            if agent_response['status'] == 'success':
                return {'signal': 'correct', 'score': 1}
            else:
                return {'signal': 'incorrect', 'score': 0}
        else:
            if agent_response['status'] == 'failure':
                return {'signal': 'correct', 'score': 1}
            else:
                return {'signal': 'incorrect', 'score': 0}

    def administer_lesson(self, world, agent):
        lesson = self.generate_lesson()
        scene_acknowledgement = world.set_scene(
            {'configuration': lesson['scene'], 'label': lesson['interaction']['content']})
        agent_response = agent.process_interaction(lesson['interaction'])
        evaluation = self.evaluate_agent_response(agent_response)
        score = evaluation['score']
        agent_response = agent.process_interaction(evaluation)
        return score

    @staticmethod
    def administer_curriculum(world_server, agent_server):
        while True:
            raw_input("Press any key to generate the next spatial word lesson...")

            lesson_object = SpatialWordLesson(is_positive=True,
                                              signal="inform",
                                              description=None,
                                              distractors=None,
                                              content=None)
            lesson = lesson_object.generate_lesson()


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
