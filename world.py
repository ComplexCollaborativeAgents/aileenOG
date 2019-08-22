import json
import sys
from log_config import logging
from world_server import AileenWorldServer
from controllers.aileensupervisor import AileenSupervisor

# read in the config file
CONFIG_FILE = "config.json"

with open(CONFIG_FILE) as config_file:
    try:
        config = json.load(config_file)
    except ValueError, e:
        logging.fatal("[aileen_world] :: Invalid json at %s; error = %s" % (CONFIG_FILE, e))
        sys.exit()


def create_and_run_aileen_world_server(controller):
    server = AileenWorldServer(controller, port=config['Servers']['output_port'])
    server.run_in_background()


if __name__ == '__main__':
    aileen_supervisor = AileenSupervisor()
    #aileen_supervisor.run_in_background()
    create_and_run_aileen_world_server(aileen_supervisor)
