from controller import Node
from log_config import logging
import constants


class ActionExecutor:
    def __init__(self, supervisor, aileen_supervisor):
        self._held_node = None
        self._supervisor = supervisor
        self._aileensupervisor = aileen_supervisor

    def process_action_command(self, action):
        logging.info("[action_executor] :: processing action {}".format(action['name']))
        if action['name'] == 'pick-up':
            return self.pick_up_object(action['id'])
        if action['name'] == 'place':
            return self.place_object(action['location'])

    def pick_up_object(self, object_id):
        node = self._supervisor.getFromId(int(object_id))
        logging.debug("[action_executor] :: picking up object id {}".format(object_id))
        translation = node.getField('translation')
        translation.setSFVec3f(constants.ROBOT_PLATE_LOCATION)
        logging.debug("[action_executor] :: object {} moved to {}".format(object_id, node.getPosition()))
        self._supervisor.step(constants.TIME_STEP)
        self._aileensupervisor.set_held_node(node)

        return True

    def place_object(self, location):
        node = self._aileensupervisor.get_held_node()
        logging.debug("[action_executor] :: currently holding node {}".format(node.getId()))
        if location == 'proxy':
            translation = node.getField('translation')
            translation.setSFVec3f(constants.TEST_LOCATION)
        else:
            translation = node.getField('translation')
            translation.setSFVec3f(location)

        self._supervisor.step(constants.TIME_STEP)
        self._aileensupervisor.set_held_node(None)
