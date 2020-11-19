import time
import xmlrpclib

import settings
from aileen_agent_server import AileenAgentServer
from log_config import logging
from soar_interface.soar_agent import SoarAgent, update
import argparse

def create_connection_with_aileen_world():
    url = 'http://{}:{}'.format(settings.WORLD_HOST, settings.WORLD_PORT)
    server = xmlrpclib.ServerProxy(url)
    logging.info("[aileen] :: created a connection with the world server at: {}".format(url))
    return server

def parse():
    parser = argparse.ArgumentParser(description='give a port for running the agent on')
    parser.add_argument('--port', help='run agent on port')
    return parser.parse_args()


if __name__ == '__main__':
    arguments = parse()
    if arguments.port:
        port = int(arguments.port)
    else:
        port = settings.AGENT_PORT

    world_server = create_connection_with_aileen_world()
    aileen_agent = SoarAgent(world_server)
    logging.info("[aileen] :: Created aileen agent")
    aileen_agent.register_output_callback(update, aileen_agent)
    aileen_agent.start()

    if settings.SOAR_DEBUG:
        aileen_agent.stop()



    aileen_agent_server = AileenAgentServer(aileen_agent, port=port)
    aileen_agent_server.run_in_background()

    while True:
        time.sleep(0.001)
