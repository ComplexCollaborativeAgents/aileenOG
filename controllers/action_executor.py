from controller import Node
from log_config import logging
import constants


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
        node = self._supervisor.getFromId(int(object_id))
        logging.debug("[action_executor] :: picking up object id {}".format(object_id))
        translation = node.getField('translation')
        translation.setSFVec3f(constants.ROBOT_PLATE_LOCATION)
        logging.debug("[action_executor] :: object {} moved to {}".format(object_id,node.getPosition()))
        self._supervisor.step(constants.TIME_STEP)
        return True, node

    def place_object(self, location_vector):
        translation = self._held_node.getField('translation')
        translation.setSFVec3f(location_vector)