from log_config import logging
import sys
import os
import json


class Configuration:
    ROOT_DIR = os.path.dirname(os.path.abspath(__file__))
    CONFIG_FILE = os.path.join(ROOT_DIR, 'config.json')
    with open(CONFIG_FILE) as config_file:
        try:
            config = json.load(config_file)
        except ValueError, e:
            logging.fatal("[aileen] :: Invalid json at %s; error = %s" % (CONFIG_FILE, e))
            sys.exit()
    sys.path.append(config['Soar']['path'])
