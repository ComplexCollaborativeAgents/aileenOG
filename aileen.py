import sys
sys.path.append("strands_qsr_lib/qsr_lib/src") # Add QSR Lib Submodule to path
import json
import xmlrpclib
from log_config import logging
from soar_interface import soar_agent
from soar_interface.soar_agent import update
from configuration import Configuration
from aileen_agent_server import AileenAgentServer


def create_connection_with_aileen_world():
    url = 'http://{}:{}'.format(Configuration.config['Servers']['input_host'],
                                Configuration.config['Servers']['input_port'])
    server = xmlrpclib.ServerProxy(url)
    logging.info("[aileen] :: created a connection with the world server at: {}".format(url))
    return server


if __name__ == '__main__':
    world_server = create_connection_with_aileen_world()
    aileen_agent = soar_agent.soar_agent(world_server)
    logging.info("[aileen] :: Created aileen agent")
    aileen_agent.register_output_callback(update, aileen_agent)
    aileen_agent.start()
    aileen_agent.stop()

    while(True):
        pass
