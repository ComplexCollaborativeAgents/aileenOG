import xmlrpclib
import sys

from log_config import logging

from visual_word_lesson import VisualWordLesson
from generate_training_images import TrainingImage
from spatial_word_lesson import SpatialWordLesson
from action_word_lesson import ActionWordLesson
import settings

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


if __name__ == '__main__':
    world_server = create_connection_with_aileen_world()
    agent_server = create_connection_with_aileen_agent()

    if len(sys.argv) > 1:
        if str(sys.argv[1]).__contains__('--train-vision'):
            # run vision training scripts
            print ('Generating images that will train vision system.')
            TrainingImage.generate_scenes(world_server, agent_server)
    else:
        VisualWordLesson.administer_curriculum(world_server, agent_server)
        # SpatialWordLesson.administer_curriculum(world_server, agent_server)
        # ActionWordLesson.administer_curriculum(world_server, agent_server)


