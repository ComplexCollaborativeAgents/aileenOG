
from log_config import logging
import os
import constants
import json
from random import choice
from aileen_object import AileenObject
from aileen_scene import AileenScene
from spatial_word_lesson import SpatialWordLesson
from language_generator import LanguageGenerator


class ActionWordLesson:

    def __init__(self):
        self._action_definitions_set = ActionWordLesson.get_action_definition_set()
        self._action = choice(self._action_definitions_set.keys())
        self._action_definition = self._action_definitions_set[self._action]
        self._initial_scene = AileenScene()
        self._scene_objects = {}
        self._scene_relations = {}
        self._trace_action_list = self._action_definition[constants.ACTION_DEF_TRACE_ACTIONS]
        self._lesson_state = constants.ACTION_LESSON_STATE_START
        self._action_trace_index = 0
        self._language = None
        self.generate_setup()

    def generate_setup(self):
        logging.debug("[action_word_lesson] :: generating the initial scene for action word learning")
        objects = self._action_definition[constants.ACTION_DEF_OBJECTS]
        if len(objects) > 0:
            for obj in objects:
                self._scene_objects[obj] = AileenObject.generate_random_object()

        relations = self._action_definition[constants.ACTION_DEF_RELATIONS]
        if relations is not None and len(relations) > 0:
            for rel in relations:
                self._scene_relations[rel] = SpatialWordLesson.get_spatial_configurations_set()[rel]

        language_template = self._action_definition[constants.ACTION_DEF_LANGUAGE]
        self._language = LanguageGenerator.generate_language_from_template(self._scene_objects, language_template)
        logging.debug("[action_word_lesson] :: generated language for action: {}".format(self._language))


    def generate_initial_state(self):
        initial_state_description = self._action_definition[constants.ACTION_DEF_INIT_CONFIG]
        if len(initial_state_description) < 1 and len(self._scene_objects) <= 2:
            for scene_object_name in self._scene_objects.keys():
                position = AileenScene.get_random_position_on_table()
                self._scene_objects[scene_object_name].set_translation(position)
                self._initial_scene.add_object(self._scene_objects[scene_object_name])

    def advance_lesson_state(self):
        if self._lesson_state == constants.ACTION_LESSON_STATE_START:
            self._lesson_state = constants.ACTION_LESSON_STATE_TRACE
            return
        if self._lesson_state == constants.ACTION_LESSON_STATE_TRACE:
            self._lesson_state = constants.ACTION_LESSON_STATE_END
            return
        if self._lesson_state == constants.ACTION_LESSON_STATE_END:
            self._lesson_state = constants.ACTION_LESSON_STATE_COMPLETE
            return


    def get_segment_for_lesson_start(self):
        self.generate_initial_state()
        segment = {
            'scene': self._initial_scene.generate_scene_description(),
            'interaction': {
                'marker': constants.ACTION_LESSON_STATE_START,
                'language': self._language
            }
        }
        return segment

    def get_structure_for_pick_up_action(self, trace_action):
        action_dict = {'name':'pick-up'}
        scene_object = self._scene_objects[trace_action['argument']]
        action_dict['uuid'] = scene_object._name
        return action_dict

    def get_structure_for_place_action(self, trace_action):
        action_dict = {'name': 'place'}
        scene_object1 = self._scene_objects[trace_action['argument1']]
        scene_object2 = self._scene_objects[trace_action['argument2']]
        relation_def = SpatialWordLesson.get_spatial_configurations_set()[trace_action['relation']]
        relation_qsr = relation_def[constants.SPATIAL_DEF_DEFINITION]
        logging.debug("[action_word_lesson] :: attempting to place objects in configuration {}".format(relation_qsr))
        try:
            position = AileenScene.place_object_in_configuration_with(target_object_name=trace_action['argument1'],
                                                                      scene_objects=self._scene_objects,
                                                                      configuration_def=relation_qsr)
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
            'interaction': "none"
        }
        self._action_trace_index += 1
        if self._action_trace_index >= len(self._trace_action_list):
            self.advance_lesson_state()
        return segment

    def get_segment_for_lesson_end(self):
        segment = {
            'interaction':{'marker': constants.ACTION_LESSON_STATE_END,
                           'interaction': ""}
        }
        self.advance_lesson_state()
        return segment

    def deliver_action_lesson_segment(self, world_server, agent_server):
        if self._lesson_state == constants.ACTION_LESSON_STATE_START:
            logging.debug("[action_word_lesson] :: setting up the initial state configuration of action")
            segment = self.get_segment_for_lesson_start()
            scene_acknowledgement = world_server.set_scene(
                {'configuration': segment['scene'], 'label': "{}".format(self._language)})
            interaction_acknowledgement = agent_server.process_language(segment['interaction'])
            self.advance_lesson_state()
            return

        if self._lesson_state == constants.ACTION_LESSON_STATE_TRACE:
            logging.debug("[action_word_lesson] :: providing the next step in action trace")
            segment = self.get_segment_for_lesson_action_trace()
            logging.debug("[action_word_lesson] :: received action trace {}".format(segment))
            if segment['action'] is not None:
                scene_acknowledgement = world_server.apply_action(segment['action'])
                interaction_acknowledgement = agent_server.process_language(segment['interaction'])
            return

        if self._lesson_state == constants.ACTION_LESSON_STATE_END:
            logging.debug("[action_word_lesson] :: communicating the terminal state configuration of action")
            segment = self.get_segment_for_lesson_end()
            interaction_acknowledgement = agent_server.process_language(segment['interaction'])
            return

        if self._lesson_state == constants.ACTION_LESSON_STATE_BAD:
            logging.debug("[action_word_lesson] :: communicating that the generated action trace is bad")
            interaction_acknowledgement = agent_server.process_language({'marker':'bad'})


    @staticmethod
    def administer_curriculum(world_server, agent_server):
        while True:
            raw_input("Press any key to generate the next action word lesson...")
            lesson = ActionWordLesson()
            logging.info("[action_word_lesson] :: generated a lesson for new action word")
            while lesson._lesson_state is not constants.ACTION_LESSON_STATE_COMPLETE:
                raw_input("Press any key to deliver the next action lesson segment...")
                lesson.deliver_action_lesson_segment(world_server, agent_server)
                logging.debug("[action_word_lesson] :: action lesson state is: {}".format(lesson._lesson_state))


    @staticmethod
    def get_action_definition_set():
        root_dir = os.path.dirname(os.path.abspath(__file__))
        action_definition_file = os.path.join(root_dir, "..", 'resources',
                                              constants.ACTION_DEFINITION_FILE_NAME)
        with open(action_definition_file) as f:
            action_definitions = json.load(f)
        return action_definitions


if __name__ == '__main__':
    pass
