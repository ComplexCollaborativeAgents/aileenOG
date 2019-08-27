from controller import Supervisor
from log_config import logging
from threading import Thread
from action_executor import ActionExecutor

TIME_STEP = 32


class AileenSupervisor:

    def __init__(self):
        self._supervisor = Supervisor()
        logging.info("[aileen_supervisor] :: started supervisor control of the world")

        self._action_executor = ActionExecutor(self._supervisor)
        logging.debug("[aileen_supervisor] :: enabled action simulation")

        self._camera = self._supervisor.getCamera('camera')
        self._camera.enable(TIME_STEP)
        self._camera.recognitionEnable(TIME_STEP)
        logging.info("[aileen_supervisor] :: enabled camera")
        self._supervisor.step(TIME_STEP)

        self._world_thread = None

    def run_in_background(self):
        self._world_thread = Thread(target=self.run_world_loop)
        self._world_thread.start()
        logging.info("[aileen_supervisor] :: started world thread")

    def run_world_loop(self):
        while self._supervisor.step(TIME_STEP) != -1:
            print("here")
            pass

    def get_all(self):
        logging.debug("[aileen_supervisor] :: processing get_all from client")
        root = self._supervisor.getRoot()
        children = root.getField('children')
        num_children = children.getCount()
        logging.debug("[aileen_supervisor] :: world contains {} nodes".format(num_children))
        objects = []

        for i in range(0,num_children):
            object_node = children.getMFNode(i)
            object_name = object_node.getTypeName()
            if 'Solid' in object_name:
                object_children = object_node.getField('children')
                shape_node = object_children.getMFNode(0)
                object_dict = {
                                'id': object_node.getId(),
                                'position': object_node.getPosition(),
                                'orientation': object_node.getOrientation(),
                                'bounding_object': object_node.getField('boundingObject').getSFNode().getTypeName()}
                objects.append(object_dict)

        output_dict = {'objects': objects}
        return output_dict

    def apply_action(self, action):
        logging.debug("[aileen_supervisor] :: processing apply_action from client for action {}".format(action['action']))
        acknowledgement = self._action_executor.process_action_command(action['action'])
        return True
