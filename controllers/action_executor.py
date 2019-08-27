from controller import Node
from log_config import logging

ROBOT_PLATE_LOCATION = [-0.1, 0.2, 0]
TIME_STEP = 32

class ActionExecutor:
    def __init__(self, supervisor):
        self._held_node = None
        self._supervisor = supervisor

    def process_action_command(self, action):
        logging.debug("[action_executor] :: processing action {}".format(action['name']))
        if action['name'] == 'pick-up':
            return self.pick_up_object(action['id'])
        if action['name'] is 'place':
            return self.place_object(0)

    def pick_up_object(self, object_id):
        logging.debug("[action_executor] :: picking up object id {}".format(object_id))
        node = self._supervisor.getFromId(int(object_id))
        logging.debug("[action_executor} :: node's current position is")
        translation = node.getField('translation')
        translation.setSFVec3f(ROBOT_PLATE_LOCATION)
        self._held_node = node
        self._supervisor.step(TIME_STEP)
        return True

    def place_object(self, location_vector):
        translation = self._held_node.getField('translation')
        translation.setSFVec3f(location_vector)
        self._held_node = None

    def dummy(self):
        logging.debug("[action_executor] :: dummy function here")
        return