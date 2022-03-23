import argparse
import xmlrpclib

import settings
from instructor.action_word_lesson import ActionWordLesson
from log_config import logging

from spatial_word_lesson import SpatialWordLesson
from visual_word_lesson import VisualWordLesson
from action_word_lesson import ActionWordLesson
from instructor.curriculum import Curriculum
import json
from threading import Thread
from instructor import gui


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
    parser.add_argument('--clean', action='store_true', help='Clean table')
    parser.add_argument('--json', help='Use curriculum from JSON file')
    parser.add_argument('--test-action', help='Test available actions', action='store_true')
    parser.add_argument('--test-sizes', help='Test different sizes', action='store_true')
    parser.add_argument('--replay', help="Debugging exact scne")
    return parser.parse_args()

def check_visibility(agent):
    iwriter = agent._input_writer
    data = iwriter.request_server_for_current_state_image()
    return data['save']

def run_curriculum(json_path):
    with open(json_path, 'r') as f:
        curriculum = Curriculum(json.load(f))
    for lesson_config in curriculum:
        if lesson_config['type'] == 'action-word':
            lesson_object = lesson_config['object']
            lesson = lesson_object.generate_lesson()
            while lesson_object._lesson_state is not settings.ACTION_LESSON_STATE_COMPLETE:
                raw_input("Press any key to deliver the next action lesson segment...")
                agent_response = lesson_object.deliver_action_lesson_segment(world_server, agent_server)
                if agent_response['status'] == 'failure':
                    break
            evaluation = lesson_object.evaluate_agent_response(agent_response)
            agent_response = agent_server.process_interaction(evaluation)
            logging.info("[aileen_instructor] :: provided feedback to agent " + str(agent_response))
        else:
            lesson_object = lesson_config['object']
            raw_input("Press any key to generate the next lesson...")
            lesson = lesson_object.generate_lesson()
            qualify = check_visibility(agent_server)
            count = 2
            while ~qualify:
                logging.info("[aileen_instructor] :: Previous scene contains invisible objects, retry to place objects:{}". format(count))
                lesson = lesson_object.generate_lesson()
                qualify = check_visibility(agent_server)
                count += 1

            scene_acknowledgement = world_server.set_scene(
                {'configuration': lesson['scene'], 'label': lesson['interaction']['content']})
            logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))
            gui.log('[instructor] {}: {}'.format(lesson['interaction']['signal'], lesson['interaction']['content']))
            agent_response = agent_server.process_interaction(lesson['interaction'])
            logging.info("[aileen_instructor] :: received from agent {}".format(agent_response))
            evaluation = lesson_object.evaluate_agent_response(agent_response)
            if 'status' in agent_response:
                response = agent_response['status']
            if 'language' in agent_response:
                response = agent_response['language']
            gui.log('Agent: ' + response + '. ' + str(
                agent_response['create_concept_count']) + ' new concept(s) created' + ' and ' + str(
                agent_response['store_instance_count']) + ' example(s) stored.')

            agent_response = agent_server.process_interaction(evaluation)
            logging.info("[aileen_instructor] :: provided feedback to agent")



def run_test_curriculum(json_path):

    with open(json_path, 'r') as f:
        lesson = json.load(f)


    scene_acknowledgement = world_server.set_scene(
        {'configuration': lesson['scene'], 
         'label': lesson['interaction']['content']})


    # logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))

    gui.log('[instructor] {}: {}'.format(lesson['interaction']['signal'], lesson['interaction']['content']))
    agent_response = agent_server.process_interaction(lesson['interaction'])

    # logging.info("[aileen_instructor] :: received from agent {}".format(agent_response))
    # evaluation = lesson_object.evaluate_agent_response(agent_response)
    # if 'status' in agent_response:
    #     response = agent_response['status']
    # if 'language' in agent_response:
    #     response = agent_response['language']
    # gui.log('Agent: ' + response + '. ' + str(
    #     agent_response['create_concept_count']) + ' new concept(s) created' + ' and ' + str(
    #     agent_response['store_instance_count']) + ' example(s) stored.')

    # agent_response = agent_server.process_interaction(evaluation)
    logging.info("[aileen_instructor] :: Finished debugging lesson.")





if __name__ == '__main__':
    arguments = parse()
    world_server = create_connection_with_aileen_world()
    agent_server = create_connection_with_aileen_agent()

    if arguments.train_vision:
        # run vision training scripts
        print('Generating images that will train vision system.')
        from generate_training_images import TrainingImage

        TrainingImage.generate_scenes(world_server, agent_server)

    elif arguments.replay:

        curriculum_thread = Thread(target=run_test_curriculum, args=(arguments.replay,))
        curriculum_thread.daemon = True
        curriculum_thread.start()
        gui.run()

    elif arguments.clean:
        print('Cleaning table.')
        from generate_training_images import TrainingImage

        TrainingImage.clean_scenes(world_server)

    elif arguments.json:
        curriculum_thread = Thread(target=run_curriculum, args=(arguments.json,))
        curriculum_thread.daemon = True
        curriculum_thread.start()
        gui.run()
    elif arguments.test_action:
        """
        Insert action testing here
            - find object
            - go to
            - pick up
            - put behind
            - drop
        """
        pass
    elif arguments.test_sizes:
        scene = AileenScene()

    else:
        VisualWordLesson.administer_curriculum(world_server, agent_server)
        # SpatialWordLesson.administer_curriculum(world_server, agent_server)
        # ActionWordLesson.administer_curriculum(world_server, agent_server)
