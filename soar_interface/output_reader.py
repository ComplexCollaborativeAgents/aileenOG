from soar_agent import sml
import logging
import random
import json
import soar_agent

class output_reader(object):
    def __init__(self, soar_agent, config):
        self._soar_agent = soar_agent
        self._config = config
        self.response = None

    def read_output(self):
        pass