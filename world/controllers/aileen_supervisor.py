from threading import Thread

from controller import Supervisor

import settings

import os
import xmlrpclib
from action_executor import ActionExecutor
from world.log_config import logging

import json

class AileenSupervisor(Supervisor):

    def __init__(self):
        super(AileenSupervisor, self).__init__()

        self._root = self.getRoot()
        self._children = self._root.getField('children')

        logging.info("[aileen_supervisor] :: started supervisor control of the world")

        self._action_executor = ActionExecutor(self)
        logging.info("[aileen_supervisor] :: enabled action simulation")

        self._held_node = None

        self._camera = self.getCamera('camera')
        self._camera.enable(settings.TIME_STEP)
        logging.info("[aileen_supervisor] :: enabled camera")

        self._color_definitions = self.get_colors()

        self._world_thread = None

    def run_in_background(self):
        self._world_thread = Thread(target=self.run_world_loop)
        self._world_thread.start()
        logging.info("[aileen_supervisor] :: started world thread")

    def run_world_loop(self):
        while self.step(settings.TIME_STEP) != -1:
            pass

    def set_held_node(self, node):
        self._held_node = node
        if self._held_node is not None:
            logging.info("[action_supervisor] :: held object is {}".format(self._held_node.getId()))
        else:
            logging.info("[action_supervisor] :: no object is currently held")

    def get_held_node(self):
        return self._held_node

    def get_all(self):
        logging.debug("[aileen_supervisor] :: processing get_all from client")
        num_children = self._children.getCount()
        logging.debug("[aileen_supervisor] :: world contains {} nodes".format(num_children))
        objects = []

        for i in range(0, num_children):
            object_node = self._children.getMFNode(i)
            object_name = object_node.getTypeName()
            if 'Solid' in object_name:
                object_children = object_node.getField('children')
                object_dict = {
                    'id': object_node.getId(),
                    'position': object_node.getPosition(),
                    'bounding_box': self.computeBoundingBox(object_node),
                    'shape': self.get_object_shape(object_node),
                    'color': self.get_object_color(object_node),
                    'texture': self.get_object_texture(object_node),
                    'id_name': self.get_object_name(object_node)
                }
                if object_node == self._held_node:
                    object_dict['held'] = 'true'
                else:
                    object_dict['held'] = 'false'
                objects.append(object_dict)

        output_dict = {'objects': objects}
        return output_dict

    def get_object_name(self, object_node):
        object_id = object_node.getField('name').getSFString()
        return object_id

    def get_object_shape(self, object_node):
        children = object_node.getField('children')
        for i in range(0, children.getCount()):
            shape_node = children.getMFNode(i)
            if shape_node.getTypeName() == "Shape":
                geometry_node = shape_node.getField('geometry').getSFNode()
                geometry_string = geometry_node.getTypeName()
                label_string = "cv_{}".format(geometry_string.lower())
                return label_string

    def get_object_color(self, object_node):
        children = object_node.getField('children')
        for i in range(0, children.getCount()):
            shape_node = children.getMFNode(i)
            if shape_node.getTypeName() == "Shape":
                appearance_node = shape_node.getField('appearance').getSFNode()
                color_vector = appearance_node.getField('baseColor').getSFColor()

                for color_def in self._color_definitions.keys():
                    if color_vector in self._color_definitions[color_def]:
                        return "cv_{}".format(color_def)

                # appearance_children = appearance_node.get
                # for j in range(0, appearance_children.getCount()):
                #     color_node = appearance_children.getMFNode(j)
                #     color_vector = color_node.getSFVec2f()
                #     logging.debug(color_vector)
                #     label_string = "cv_"
                #     return label_string
        return "cv_"


    def get_object_texture(self, object_node):
        return "t_"

    def computeBoundingBox(self, object_node):
        '''
        computes the bounding box in geojson format
        https://tools.ietf.org/html/rfc7946#section-5
        '''
        centroid = object_node.getPosition()
        bounding_obj = object_node.getField('boundingObject').getSFNode()
        logging.debug("[aileen_supervisor] :: computing bounding box for {}".format(
            bounding_obj.getTypeName()))
        if (bounding_obj.getTypeName() == "Box"):
            size = bounding_obj.getField('size').getSFVec3f()
            return [centroid[0] - size[0] / 2, centroid[1] - size[1] / 2, centroid[2] - size[2] / 2,
                    centroid[0] + size[0] / 2, centroid[1] + size[1] / 2, centroid[2] + size[2] / 2]
        elif (bounding_obj.getTypeName() == "Cylinder"):
            height = bounding_obj.getField('height').getSFFloat()
            radius = bounding_obj.getField('radius').getSFFloat()
            return [centroid[0] - radius, centroid[1] - height / 2, centroid[2] - radius,
                    centroid[0] + radius, centroid[1] + height / 2, centroid[2] + radius]
        else:
            raise Exception("[aileen_supervisor] :: Unable to compute bounding box for type {}".format(bounding_obj))

    def apply_action(self, action):
        logging.debug("[aileen_supervisor] :: processing apply_action from client for action {}".format(action))
        if action is None:
            logging.error("[aileen_supervisor] :: received an empty action description. not doing anything")
            return False
        acknowledgement = self._action_executor.process_action_command(action)
        return acknowledgement

    def get_image(self):
        logging.debug("[aileen_supervisor] :: processing get_image from client")
        image_string = self._camera.getImage()
        logging.debug("[aileen_supervisor] :: got current image")
        dir_name = os.path.split(settings.CURRENT_IMAGE_PATH)[0]
        if not os.path.exists(dir_name):
            os.mkdir(dir_name)
        self._camera.saveImage(settings.CURRENT_IMAGE_PATH, 100)
        logging.debug("[aileen_supervisor] :: saved current image at {}".format(settings.CURRENT_IMAGE_PATH))

        with open(settings.CURRENT_IMAGE_PATH, "rb") as handle:
            binary_image = xmlrpclib.Binary(handle.read())
            return binary_image

    def set_scene(self, scene_objects, label):
        self.clean_scene()
        logging.debug("[aileen_supervisor] :: trying to add new objects to the scene.")
        for scene_object in scene_objects:
            self._children.importMFNodeFromString(-1, scene_object)

        self.setLabel(1, label, 0.4, 0.1, 0.1, 0x000000, 0, "Arial")
        return True

    def clean_scene(self):
        logging.debug("[aileen_supervisor] :: cleaning objects from the scene")
        num_children = self._children.getCount()

        nodes_to_remove = []

        for i in range(0, num_children):
            object_node = self._children.getMFNode(i)
            object_name = object_node.getTypeName()
            if 'Solid' in object_name:
                nodes_to_remove.append(object_node.getId())

        print len(nodes_to_remove)
        for i in range(0, len(nodes_to_remove)):
            node_id = nodes_to_remove[i]
            node = self.getFromId(node_id)
            node.remove()

    @staticmethod
    def get_colors():
        with open(settings.COLOR_PATH) as f:
            colors = json.load(f)
        return colors
