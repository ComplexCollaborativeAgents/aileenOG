import xmlrpclib
from log_config import logging
from configuration import Configuration
import time


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
    logging.info("[aileen_instructor] :: created a connection with the world server at: {}".format(url))
    return server


if __name__ == '__main__':
    world_server = create_connection_with_aileen_world()
    agent_server = create_connection_with_aileen_agent()

    while True:
        time.sleep(0.005)

        scene = {}
        scene_acknowledgement = world_server.set_scene(scene)
        logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))

        language = {'language':'test-language'}
        language_acknowledgement = agent_server.process_language(language)
        logging.info("[aileen_instructor] :: received from agent {}".format(language_acknowledgement))
