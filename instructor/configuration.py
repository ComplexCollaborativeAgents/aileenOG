from log_config import logging
import sys
import os
import json


class Configuration:
    ROOT_DIR = os.path.dirname(os.path.abspath(__file__))
    CONFIG_FILE = os.path.join(ROOT_DIR, 'resources', 'config.json')
    with open(CONFIG_FILE) as config_file:
        try:
            config = json.load(config_file)
        except ValueError as e:
            logging.fatal("[aileen_instructor] :: Invalid json at %s; error = %s" % (CONFIG_FILE, e))
            sys.exit()
