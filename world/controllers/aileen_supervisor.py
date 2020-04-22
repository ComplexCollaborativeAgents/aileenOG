from threading import Thread
from controller import Supervisor
import settings
from agent.vision.Detector import Detector
import os
import xmlrpclib
from action_executor import ActionExecutor
from world.log_config import logging
import json

class AileenSupervisor(Supervisor):

    def initialize_robot(self):
        logging.info('[aileen_supervisor] :: initializing robot')
        for name in settings.JOINT_NAMES:
            self._motorNodes.append(self.getMotor(name))
            self._motorSensorNodes.append(self.getPositionSensor(name+'_sensor'))
            self._motorSensorNodes[-1].enable(settings.TIME_STEP)
        logging.info('[aileen_supervisor] :: got {} motors and {} position sensors'.format(len(self._motorNodes), len(self._motorSensorNodes)))
        logging.info('[aileen_supervisor] :: moving to start position')
        self.command_pose(settings.START_LOCATION_1)
        self.wait_for_motion_complete()
        self.command_pose(settings.START_LOCATION_2)
        self.wait_for_motion_complete()
        logging.info('[aileen_supervisor] :: reached start position')

    def __init__(self):
        super(AileenSupervisor, self).__init__()

        self._root = self.getRoot()
        self._children = self._root.getField('children')
        self._numNodes = self._children.getCount()

        logging.info("[aileen_supervisor] :: started supervisor control of the world")

        self._action_executor = ActionExecutor(self)
        logging.info("[aileen_supervisor] :: enabled action simulation")

        self._held_node = None

        self._numDevices = self.getNumberOfDevices()

        self._motorNodes = list()
        self._motorSensorNodes = list()

        self._camera = self.getCamera('camera')
        self._camera.enable(settings.TIME_STEP)
        self.resX = self._camera.getWidth()
        self.resY = self._camera.getHeight()
        logging.info("[aileen_supervisor] :: enabled camera")

        self._color_definitions = self.get_colors()

        self._world_thread = None

        self.initialize_robot()

    def wait_for_motion_complete(self):
        while not self.check_in_position():
            self.step(settings.TIME_STEP)

    def check_in_position(self, motor=-1):
        if motor < 0:
            truth = list()
            for i in range(len(self._motorNodes)):
                truth.append(abs(self._motorNodes[i].getTargetPosition() - self._motorSensorNodes[i].getValue()) < settings.IN_POS_THRESH)

            return all(truth)
        else:
            return abs(self._motorNodes[motor].getTargetPosition() - self._motorSensorNodes[motor].getValue()) < settings.IN_POS_THRESH

    def command_pose(self, pose):
        for i in range(len(pose)):
            self._motorNodes[i].setPosition(pose[i])

    def run_in_background(self):
        self._world_thread = Thread(target=self.run_world_loop)
        self._world_thread.start()
        logging.info("[aileen_supervisor] :: started world thread")

    def run_world_loop(self):
        #Robot Initialization Here
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

                world_bbox = self.computeBoundingBox(object_node)
                w_x1 = world_bbox[0]
                w_y1 = world_bbox[2]
                w_x2 = world_bbox[3]
                w_y2 = world_bbox[5]

                im_x1, im_y1 = Detector.coord_world2im(w_x1, w_y1)
                im_x2, im_y2 = Detector.coord_world2im(w_x2, w_y2)

                # convert into bbox format: (top_left (x1,y1), bottom_right (x2,y2))
                width = im_x2 - im_x1
                height = im_y2 - im_y1
                im_x1 = im_x1 - width / 2.0
                im_y1 = im_y1 - height / 2.0
                im_x2 = im_x2 - width / 2.0
                im_y2 = im_y2 - height / 2.0

                object_children = object_node.getField('children')
                if object_node == self._held_node:
                    object_dict = {'id_string': "ob{}".format(str(object_node.getId())),
                                   'id': object_node.getId(),
                                   'shape': self.get_object_shape(object_node),
                                   'color': self.get_object_color(object_node),
                                   'texture': self.get_object_texture(object_node),
                                   'id_name': self.get_object_name(object_node),
                                   'held': 'true'}
                else:
                    object_dict = {'id_string': "ob{}".format(str(object_node.getId())),
                                   'id': object_node.getId(),
                                   'position': object_node.getPosition(),
                                   'bounding_box': self.computeBoundingBox(object_node),
                                   'bounding_box_camera': [im_x1, im_y1, im_x2, im_y2],
                                   'shape': self.get_object_shape(object_node),
                                   'color': self.get_object_color(object_node),
                                   'texture': self.get_object_texture(object_node),
                                   'id_name': self.get_object_name(object_node),
                                   'held': 'false'}
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
                label_string = "CV{}".format(geometry_string.title())
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
                        return "CV{}".format(color_def.title())

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
        output_dict = self.get_all()
        logging.debug("[aileen_supervisor] :: processing get_image from client")
        _ = self._camera.getImage()
        logging.debug("[aileen_supervisor] :: got current image")
        dir_name = os.path.split(settings.CURRENT_IMAGE_PATH)[0]
        if not os.path.exists(dir_name):
            os.mkdir(dir_name)
        self._camera.saveImage(settings.CURRENT_IMAGE_PATH, 100)
        logging.debug("[aileen_supervisor] :: saved current image at {}".format(settings.CURRENT_IMAGE_PATH))
        return output_dict


    def set_scene(self, scene_objects, label):
        self.clean_scene()
        logging.debug("[aileen_supervisor] :: trying to add new objects to the scene.")
        for scene_object in scene_objects:
            self._children.importMFNodeFromString(-1, scene_object)

        self.setLabel(1, label, 0.02, 0.3, 0.1, 0x000000, 0, "Arial")
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
