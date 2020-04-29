import argparse
import xmlrpclib

import settings
from instructor.action_word_lesson import ActionWordLesson
from log_config import logging

from spatial_word_lesson import SpatialWordLesson
from action_word_lesson import ActionWordLesson
from instructor.curriculum import Curriculum
import json


def create_connection_with_aileen_world():
    url = 'http://{}:{}'.format(settings.WORLD_HOST, settings.WORLD_PORT)
    server = xmlrpclib.ServerProxy(url)
    logging.info("[aileen_instructor] :: created a connection with the world server at: {}".format(url))
    return server


def create_connection_with_aileen_agent():
    url = 'http://{}:{}'.format(settings.AGENT_HOST, settings.AGENT_PORT)
    server = xmlrpclib.ServerProxy(url)
    logging.info("[aileen_instructor] :: created a connection with the agent: {}".format(url))
    return server


def parse():
    parser = argparse.ArgumentParser(description='Aileen instructor.')
    parser.add_argument('--train-vision', action='store_true', help='Run vision training scripts')
    parser.add_argument('--json', help='Use curriculum from JSON file')
    return parser.parse_args()

if __name__ == '__main__':
    arguments = parse()
    world_server = create_connection_with_aileen_world()
    agent_server = create_connection_with_aileen_agent()

    if arguments.train_vision:
        # run vision training scripts
        print ('Generating images that will train vision system.')
        from generate_training_images import TrainingImage
        TrainingImage.generate_scenes(world_server, agent_server)
        
    elif arguments.json:
        with open(arguments.json, 'r') as f:
            curriculum = Curriculum(json.load(f))
        for lesson in curriculum:
            if lesson['type'] == 'action-word':
                lesson_object = lesson['object']
                while lesson_object._lesson_state is not settings.ACTION_LESSON_STATE_COMPLETE:
                    raw_input("Press any key to deliver the next action lesson segment...")
                    agent_response = lesson_object.deliver_action_lesson_segment(world_server, agent_server)
                evaluation = lesson['object'].evaluate_agent_response(agent_response)
                agent_response = agent_server.process_interaction(evaluation)
                logging.info("[aileen_instructor] :: provided feedback to agent")
            else:
                raw_input("Press any key to generate the next lesson...")
                scene_acknowledgement = world_server.set_scene(
                    {'configuration': lesson['scene'], 'label': lesson['interaction']['content']})
                logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))
                agent_response = agent_server.process_interaction(lesson['interaction'])
                logging.info("[aileen_instructor] :: received from agent {}".format(agent_response))
                evaluation = lesson['object'].evaluate_agent_response(agent_response)
                agent_response = agent_server.process_interaction(evaluation)
                logging.info("[aileen_instructor] :: provided feedback to agent")
    else:
        #VisualWordLesson.administer_curriculum(world_server, agent_server)
        # SpatialWordLesson.administer_curriculum(world_server, agent_server)
        ActionWordLesson.administer_curriculum(world_server, agent_server)
