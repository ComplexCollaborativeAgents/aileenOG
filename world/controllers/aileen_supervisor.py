from threading import Thread

from controller import Supervisor
from controller import CameraRecognitionObject
from world import constants

import os
import xmlrpclib
from action_executor import ActionExecutor
from world.log_config import logging


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

        self._camera.enable(constants.TIME_STEP)
        # self._camera.recognitionEnable(constants.TIME_STEP)
        self.resX = self._camera.getWidth()
        self.resY = self._camera.getHeight()

        logging.info("[aileen_supervisor] :: enabled camera")

        self._world_thread = None

    def run_in_background(self):
        self._world_thread = Thread(target=self.run_world_loop)
        self._world_thread.start()
        logging.info("[aileen_supervisor] :: started world thread")

    def run_world_loop(self):
        while self.step(constants.TIME_STEP) != -1:
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

                world_bbox = self.computeBoundingBox(object_node)
                w_x1 = world_bbox[0]
                w_y1 = world_bbox[1]
                w_x2 = world_bbox[3]
                w_y2 = world_bbox[4]
                im_x1, im_y1 = self.coord_world2im(w_x1, w_y1)
                im_x2, im_y2 = self.coord_world2im(w_x2, w_y2)

                object_children = object_node.getField('children')
                object_dict = {
                    'id': object_node.getId(),
                    'position': object_node.getPosition(),
                    'bounding_box': self.computeBoundingBox(object_node),
                    'bounding_box_camera': [im_x1, im_y1, im_x2, im_y2],
                    'shape': self.get_object_shape(object_node),
                    'color': self.get_object_color(object_node),
                    'texture': self.get_object_texture(object_node),
                }
                if object_node == self._held_node:
                    object_dict['held'] = 'true'
                else:
                    object_dict['held'] = 'false'
                objects.append(object_dict)

        output_dict = {'objects': objects}

        return output_dict

    def get_object_shape(self, object_node):
        children = object_node.getField('children')
        for i in range(0, children.getCount()):
            shape_node = children.getMFNode(i)
            if shape_node.getTypeName() == "Shape":
                geometry_node = shape_node.getField('geometry').getSFNode()
                geometry_string = geometry_node.getTypeName()
                label_string = "s_{}".format(geometry_string.lower())
                return label_string

    def get_object_color(self, object_node):
        return "c_"

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
        dir_name = os.path.split(constants.CURRENT_IMAGE_PATH)[0]
        if not os.path.exists(dir_name):
            os.makedirs(dir_name)
        self._camera.saveImage(constants.CURRENT_IMAGE_PATH, 100)

        logging.debug("[aileen_supervisor] :: saved current image at {}".format(constants.CURRENT_IMAGE_PATH))

        with open(constants.CURRENT_IMAGE_PATH, "rb") as handle:
            binary_image = xmlrpclib.Binary(handle.read())
            return binary_image

    def set_scene(self, scene_objects, label):
        self.clean_scene()
        logging.debug("[aileen_supervisor] :: trying to add new objects to the scene.")
        for scene_object in scene_objects:
            self._children.importMFNodeFromString(-1, scene_object)

        self.setLabel(1, label, 0.4, 0.1, 0.1, 0x000000, 0, "Arial")
        return True

    @staticmethod
    def coord_im2world(x, y):

        # Due to some difficulty getting camera parameters from webots, we use a simple linear regression
        # to map im coordinates to world coordinates, and vice-versa.
        # Eventually, solve rotation matrix for camera and use transform matrix
        w_x = x * 1.3910 + 0.1639
        w_y = y * 1.3719 - 0.4558
        w_z = 0.399802  # pretty much every object has this height in the training data.
        return w_x, w_y, w_z

    @staticmethod
    def coord_world2im(x, y):
        # Due to some difficulty getting camera parameters from webots, we use a simple linear regression
        # to map im coordinates to world coordinates, and vice-versa.
        # Eventually, solve rotation matrix for camera and use transform matrix
        im_x = x * 0.7167 - 0.1165
        im_y = y * 0.7269 + 0.3323

        return im_x, im_y

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
