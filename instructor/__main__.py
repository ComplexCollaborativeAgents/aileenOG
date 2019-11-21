import xmlrpclib
from log_config import logging
from configuration import Configuration

from visual_word_lesson import VisualWordLesson
from spatial_word_lesson import SpatialWordLesson
from action_word_lesson import ActionWordLesson


def create_connection_with_aileen_world():
    url = 'http://{}:{}'.format(Configuration.config['Servers']['world_host'],
                                Configuration.config['Servers']['world_port'])
    server = xmlrpclib.ServerProxy(url)
    logging.info("[aileen_instructor] :: created a connection with the world server at: {}".format(url))
    return server


def create_connection_with_aileen_agent():
    url = 'http://{}:{}'.format(Configuration.config['Servers']['agent_host'],
                                Configuration.config['Servers']['agent_port'])
    server = xmlrpclib.ServerProxy(url)
    logging.info("[aileen_instructor] :: created a connection with the agent: {}".format(url))
    return server


if __name__ == '__main__':
    world_server = create_connection_with_aileen_world()
    agent_server = create_connection_with_aileen_agent()

    VisualWordLesson.administer_curriculum(world_server, agent_server)
    # SpatialWordLesson.administer_curriculum(world_server, agent_server)
    # ActionWordLesson.administer_curriculum(world_server, agent_server)

