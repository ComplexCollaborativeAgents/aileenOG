from log_config import logging
import os
import settings
import json
from random import choice
from aileen_object import AileenObject
from aileen_scene import AileenScene
from spatial_word_lesson import SpatialWordLesson
from language_generator import LanguageGenerator
from collections import OrderedDict
from instructor import gui


class ActionWordLesson:

    def __init__(self, is_positive, signal, description, distractors, content):
        self._is_positive = is_positive
        self._signal = signal
        self._description = description
        self._distractors = distractors
        self._content = content

        self._action_definitions_set = ActionWordLesson.get_action_definition_set()
        if self._description:
            self._action = self._description['action']
        else:
            self._action = ActionWordLesson.randomizer.random_action(self._action_definitions_set.keys())
        self._action_definition = self._action_definitions_set[self._action]
        self._initial_scene = AileenScene()
        self._scene_objects = OrderedDict()
        self._scene_relations = {}
        self._trace_action_list = self._action_definition[settings.ACTION_DEF_TRACE_ACTIONS]
        self._lesson_state = settings.ACTION_LESSON_STATE_START
        self._action_trace_index = 0
        self._language = None
        self.generate_setup()

    def generate_setup(self):
        logging.debug("[action_word_lesson] :: generating the initial scene for action word learning")
        objects = self._action_definition[settings.ACTION_DEF_OBJECTS]

        if self._description:
            for o, obj in zip(self._description['objects'], objects):
                self._scene_objects[obj] = AileenObject.generate_object(o)
        else:
            if len(objects) > 0:
                objs = AileenObject.generate_random_objects(len(objects))
                for o, obj in zip(objs, objects):
                    self._scene_objects[obj] = o

        relations = self._action_definition[settings.ACTION_DEF_RELATIONS]
        if relations is not None and len(relations) > 0:
            for rel in relations:
                self._scene_relations[rel] = SpatialWordLesson.get_spatial_configurations_set()[rel]

        language_template = self._action_definition[settings.ACTION_DEF_LANGUAGE]
        self._language = LanguageGenerator.generate_language_from_template(self._scene_objects, language_template)
        logging.debug("[action_word_lesson] :: generated language for action: {}".format(self._language))

    def generate_initial_state(self):
        initial_state_description = self._action_definition[settings.ACTION_DEF_INIT_CONFIG]
        print len(initial_state_description)
        if len(initial_state_description) < 1 and len(self._scene_objects) <= 2:
            for scene_object_name in self._scene_objects.keys():
                position = AileenScene.randomizer.get_random_position_on_table()
                away_from_edge = bound(position)
                self._scene_objects[scene_object_name].set_translation(away_from_edge)
                self._initial_scene.add_object(self._scene_objects[scene_object_name])
        else:
            if len(initial_state_description) <= 2 and len(self._scene_objects) <= 2:
                positions = AileenScene.place_two_objects_in_configuration(
                    target_object_name=self._scene_objects.items()[0][0],
                    reference_object_name=self._scene_objects.items()[1][0],
                    scene_objects=self._scene_objects,
                    configuration_definition=initial_state_description)
                for object_name in positions.keys():
                    scene_object = self._scene_objects[object_name]
                    position = positions[object_name]
                    away_from_edge = bound(position)
                    scene_object.set_translation(away_from_edge)
                    self._initial_scene.add_object(scene_object)
            else:
                logging.error("[action_word_lesson] :: don't know how to interpret the initial state description")

    def advance_lesson_state(self):
        if self._lesson_state == settings.ACTION_LESSON_STATE_START:
            self._lesson_state = settings.ACTION_LESSON_STATE_TRACE
            return
        if self._lesson_state == settings.ACTION_LESSON_STATE_TRACE:
            self._lesson_state = settings.ACTION_LESSON_STATE_END
            return
        if self._lesson_state == settings.ACTION_LESSON_STATE_END:
            self._lesson_state = settings.ACTION_LESSON_STATE_COMPLETE
            return

    def get_segment_for_lesson_start(self):
        self.generate_initial_state()
        segment = {
            'scene': self._initial_scene.generate_scene_world_config(),
            'interaction': {
                'signal': self._signal,
                'marker': settings.ACTION_LESSON_STATE_START,
                'content': self._content if self._content is not None else self._language
            }
        }
        self.advance_lesson_state()
        return segment

    def get_structure_for_pick_up_action(self, trace_action):
        action_dict = {'name': 'pick-up'}
        scene_object = self._scene_objects[trace_action['argument']]
        action_dict['uuid'] = scene_object._name
        return action_dict

    def get_structure_for_place_action(self, trace_action):
        action_dict = {'name': 'place'}
        scene_object1 = self._scene_objects[trace_action['argument1']]
        scene_object2 = self._scene_objects[trace_action['argument2']]
        relation_def = SpatialWordLesson.get_spatial_configurations_set()[trace_action['relation']]
        relation_qsr = relation_def[settings.SPATIAL_DEF_DEFINITION]
        logging.debug("[action_word_lesson] :: attempting to place objects in configuration {}".format(relation_qsr))
        try:
            position = AileenScene.place_object_in_configuration_with(target_object_name=trace_action['argument1'],
                                                                      reference_object_name=trace_action['argument2'],
                                                                      scene_objects=self._scene_objects,
                                                                      configuration_definition=relation_qsr)
            action_dict['location'] = position
            return action_dict
        except ValueError:
            logging.error("[action_word_lesson] :: bad lesson")

    def get_segment_for_lesson_action_trace(self):
        trace_action = self._trace_action_list[self._action_trace_index]
        if trace_action['name'] == 'pick-up':
            action_dict = self.get_structure_for_pick_up_action(trace_action)
        if trace_action['name'] == 'place':
            action_dict = self.get_structure_for_place_action(trace_action)
        segment = {
            'action': action_dict,
            'interaction': {
                'marker': settings.ACTION_LESSON_STATE_TRACE
            }
        }
        self._action_trace_index += 1
        if self._action_trace_index >= len(self._trace_action_list):
            self.advance_lesson_state()
        return segment

    def get_segment_for_lesson_end(self):
        segment = {
            'interaction': {'marker': settings.ACTION_LESSON_STATE_END}
        }
        self.advance_lesson_state()
        return segment

    def get_next_segment(self):
        if self._lesson_state == settings.ACTION_LESSON_STATE_START:
            return self.get_segment_for_lesson_start()
        if self._lesson_state == settings.ACTION_LESSON_STATE_TRACE:
            return self.get_segment_for_lesson_action_trace()
        if self._lesson_state == settings.ACTION_LESSON_STATE_END:
            self.get_segment_for_lesson_end()

    def deliver_action_lesson_segment(self, world_server, agent_server):
        if self._lesson_state == settings.ACTION_LESSON_STATE_START:
            logging.debug("[action_word_lesson] :: setting up the initial state configuration of action")
            segment = self.get_next_segment()
            gui.log("[instructor] {}: {}: {}".format(segment['interaction']['signal'],
                                                     segment['interaction']['content'],
                                                     segment['interaction']['marker']))
            scene_acknowledgement = world_server.set_scene(
                {'configuration': segment['scene'],
                 'label': "{}: {}: {}".format(segment['interaction']['signal'], segment['interaction']['content'],
                                              segment['interaction']['marker'])})
            agent_response = agent_server.process_interaction(segment['interaction'])
            gui.log("[agent] {}".format(agent_response))
            return agent_response

        if self._lesson_state == settings.ACTION_LESSON_STATE_TRACE:
            logging.debug("[action_word_lesson] :: providing the next step in action trace")
            segment = self.get_next_segment()
            logging.debug("[action_word_lesson] :: received action trace {}".format(segment))
            gui.log("[instructor] {}".format(segment['interaction']))
            agent_response = None
            if segment['action'] is not None:
                scene_acknowledgement = world_server.apply_action(segment['action'])
                agent_response = agent_server.process_interaction(segment['interaction'])
                gui.log("[agent] {}".format(agent_response))
            return agent_response

        if self._lesson_state == settings.ACTION_LESSON_STATE_END:
            logging.debug("[action_word_lesson] :: communicating the terminal state configuration of action")
            segment = self.get_next_segment()
            gui.log("[instructor] end of lesson")
            agent_response = agent_server.process_interaction({'marker':'end'})
            gui.log("[agent] {}".format(agent_response))
            return agent_response

        if self._lesson_state == settings.ACTION_LESSON_STATE_BAD:
            logging.debug("[action_word_lesson] :: communicating that the generated action trace is bad")
            # interaction_acknowledgement = agent_server.process_language({'marker':'bad'})

    def deliver_action_reaction_test(self, world_server, agent_server):
        logging.debug("[action_word_lesson] :: testing action learning with a comprehension test")
        segment = self.get_segment_for_lesson_start()
        segment['interaction']['signal'] = 'react'
        scene_acknowledgement = world_server.set_scene(
            {'configuration': segment['scene'],
             'label': "{}:{}".format(segment['interaction']['signal'], segment['interaction']['content'])})
        agent_response = agent_server.process_interaction(segment['interaction'])

    @staticmethod
    def administer_curriculum(world_server, agent_server):
        while True:
            raw_input("Press any key to generate the next action word lesson...")
            lesson = ActionWordLesson()
            # lesson.deliver_action_reaction_test(world_server, agent_server)
            logging.info("[action_word_lesson] :: generated a lesson for new action word")
            while lesson._lesson_state is not settings.ACTION_LESSON_STATE_COMPLETE:
                raw_input("Press any key to deliver the next action lesson segment...")
                lesson.deliver_action_lesson_segment(world_server, agent_server)
                logging.debug("[action_word_lesson] :: action lesson state is: {}".format(lesson._lesson_state))

    @staticmethod
    def get_action_definition_set():
        root_dir = os.path.dirname(os.path.abspath(__file__))
        action_definition_file = os.path.join(root_dir, 'resources',
                                              settings.ACTION_DEFINITION_FILE_NAME)
        with open(action_definition_file) as f:
            action_definitions = json.load(f)
        return action_definitions

    class Randomizer:

        def random_action(self, actions):
            return choice(actions)

    randomizer = Randomizer()


def bound(position):
    """
    Ensure that position is not close to the edge so that there is space for another object to be placed next to it.
    """
    [x, y, z] = position
    z = max(z, settings.OBJECT_POSITION_MIN_Z + settings.OBJECT_POSITION_DELTA)
    z = min(z, settings.OBJECT_POSITION_MAX_Z - settings.OBJECT_POSITION_DELTA)
    return [x, y, z]
