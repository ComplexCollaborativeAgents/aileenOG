import time
from log_config import logging

class input_writer(object):
    def __init__(self, soar_agent, config, world_server):
        self._soar_agent = soar_agent
        self._config = config
        self._input_link = soar_agent.get_input_link()
        self._world_server = world_server
        self.set_time = None
        self.timestamp = 0

        self._world_link = self._input_link.CreateIdWME("world")
        self._interaction_link = self._input_link.CreateIdWME("interaction")


    def generate_input(self):
        time.sleep(self._config['Soar']['sleep-time'])
        objects = None
        try:
            objects = self._world_server.get_all()
        except:
            logging.error("[input_writer] :: received bad response from world server")
        if objects is not None:
            logging.debug("[input_writer] :: received objects {}".format(objects))
        pass