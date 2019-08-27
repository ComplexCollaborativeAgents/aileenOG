import sys
import json
import xmlrpclib
from log_config import logging
from soar_interface import soar_agent
from soar_interface.soar_agent import update


CONFIG_FILE = "config.json"
with open(CONFIG_FILE) as config_file:
    try:
        config = json.load(config_file)
    except ValueError, e:
        logging.fatal("[aileen] :: Invalid json at %s; error = %s" % (CONFIG_FILE, e))
        sys.exit()


def create_connection_with_aileen_world():
    url = 'http://{}:{}'.format(config['Servers']['input_host'], config['Servers']['input_port'])
    server = xmlrpclib.ServerProxy(url)
    logging.info("[aileen] :: created a connection with the world server at: {}".format(url))
    return server


if __name__ == '__main__':
    world_server = create_connection_with_aileen_world()

    aileen_agent = soar_agent.soar_agent(config, world_server)
    logging.info("[aileen] :: Created aileen agent")
    aileen_agent.register_output_callback(update, aileen_agent)
    aileen_agent.start()
    aileen_agent.stop()

    while(True):
        pass
