from threading import Thread
from controller import Supervisor, Field, Camera, CameraRecognitionObject
import settings
#from agent.vision.Detector import Detector
import os
import cv2
from scipy.stats import mode
import xmlrpclib
from action_executor import ActionExecutor
from world.log_config import logging
import json
import time
from ikpy.chain import Chain
from ikpy.link import OriginLink, URDFLink
from ikpy.utils.geometry import *
import numpy as np
import imutils
# from instructor.shape_detector import ShapeDetector


class AileenSupervisor(Supervisor):

    def __init__(self):
        super(AileenSupervisor, self).__init__()

        self._root = self.getRoot()
        self._children = self._root.getField('children')

        logging.info("[aileen_supervisor] :: started supervisor control of the world")

        self._action_executor = ActionExecutor(self)
        logging.info("[aileen_supervisor] :: enabled action simulation")
        self.save = False
        self.num_rec = 0
        self._held_node = None

        self._numDevices = self.getNumberOfDevices()
        self._motorNodes = list()
        self._motorSensorNodes = list()
        # self._connectorNode = self.getConnector('connector')
        self._connectorNode = self.getDevice('connector')
        # self._camera = self.getCamera('camera')
        self._camera = self.getDevice('camera')
        self._camera.enable(settings.TIME_STEP)
        self._camera.recognitionEnable(settings.TIME_STEP)
        self.resX = self._camera.getWidth()
        self.resY = self._camera.getHeight()
        self.flen = self._camera.getFocalLength()
        self.fov = self._camera.getFov()
        self._cameraM = [self.flen, 0.0, self.resX / 2.0, 0.0, self.flen, self.resY / 2.0, 0.0, 0.0, 1.0]
        logging.info("[aileen_supervisor] :: enabled camera")
        self._color_definitions = self.get_colors()
        self._numObj = self._camera.getRecognitionNumberOfObjects()
        self._world_thread = None

        self._ur10Chain = Chain( links=[
                            OriginLink(),
                            URDFLink(name='shoulder_pan_joint',
                                     translation_vector=[0,0,.1273],
                                     orientation=[0,0,0],
                                     rotation=[0,0,1]),
                            URDFLink(name='shoulder_lift_joint',
                                    translation_vector=[0, .220941, 0],
                                    orientation=[0, 1.57, 0],
                                    rotation=[0,1,0]),
                            URDFLink(name='elbow_joint',
                                    translation_vector=[0, -.1719, .612],
                                    orientation=[0,0,0],
                                    rotation=[0,1,0]),
                            URDFLink(name='wrist_1_joint',
                                    translation_vector=[0, 0, .5723],
                                    orientation=[0, 1.57,0],
                                    rotation=[0,1,0]),
                            URDFLink(name='wrist_2_joint',
                                    translation_vector=[0, .1149, 0],
                                    orientation=[0,0,0],
                                    rotation=[0,0,1]),
                            URDFLink(name='wrist_3_joint',
                                    translation_vector=[0,0,.1157],
                                    orientation=[0,0,0],
                                    rotation=[0,1,0]),
                            URDFLink(name='ee',
                                    translation_vector=[0, .0922,0],
                                    orientation=[0,0,1.57],
                                    rotation=[0,0,1])],
                            active_links_mask=[True, True, True, True, True, True, True, True]
                        )
        self.initialize_robot()

    def get_cameraM(self):
        logging.info('[aileen_supervisor] :: get camera matrix')
        return self._cameraM

    def get_FOV(self):
        logging.info('[aileen_supervisor] :: get camera field of view')
        return self.fov

    def get_focalL(self):
        logging.info('[aileen_supervisor] :: get focal length')
        return self.flen

    def get_resolution(self):
        logging.info('[aileen_supervisor] :: get camera resolution')
        return self.resX, self.resY

    def initialize_robot(self):
        logging.info('[aileen_supervisor] :: initializing robot')
        for name in settings.JOINT_NAMES:
            # self._motorNodes.append(self.getMotor(name))
            self._motorNodes.append(self.getDevice(name))
            # self._motorSensorNodes.append(self.getPositionSensor(name+'_sensor'))
            self._motorSensorNodes.append(self.getDevice(name + '_sensor'))
            self._motorSensorNodes[-1].enable(settings.TIME_STEP)
        logging.info('[aileen_supervisor] :: got {} motors and {} position sensors'.format(len(self._motorNodes), len(self._motorSensorNodes)))
        logging.info('[aileen_supervisor] :: moving to start position')
        self.command_pose(settings.START_LOCATION_1)
        self.wait_for_motion_complete()
        self.command_pose(settings.HOME_POSE)
        self.wait_for_motion_complete()
        logging.info('[aileen_supervisor] :: reached start position')
        T = self._ur10Chain.forward_kinematics(self.pose_to_ikpy(settings.HOME_POSE))
        self._home = T[0:3,3]
        # self._orientation = T[0:3,0:3]
        # print(self._orientation)
        self._orientation = [0, 1, 0]
        logging.info('[aileen_supervisor] :: camera recognition is {}'.format(self._camera.hasRecognition()))
        if self._camera.hasRecognition():
            self._camera.enableRecognitionSegmentation()
        logging.info('[aileen_supervisor] :: camera recognition and segmentation is {}'.format(self._camera.isRecognitionSegmentationEnabled()))
        logging.info('[aileen_supervisor] :: camera focal length is {}'.format(self.flen))
        logging.info('[aileen_supervisor] :: camera fov is {}'.format(self.fov))

    def pose_to_ikpy(self, joints):
        j = [0]
        for item in joints:
            j.append(item)
        j.append(0)
        return j

    def pose_from_ikpy(self, pose):
        return pose[1:-1]

    def find_pose(self, point):
        """
        Assumes point is only XYZ coord right now.  Assuming fixed orientation is correct.  can change in future
        point should be a list: [X, Y, Z]
        """
        #Tmat = to_transformation_matrix(point, self._orientation)
        #tpoint = self.transform_point_to_robot_frame(point)
        init_pos = self.pose_to_ikpy(self.get_current_position())
        ikpy_pose = self._ur10Chain.inverse_kinematics(target_position=point, target_orientation=self._orientation, orientation_mode="Z", initial_position=init_pos, max_iter=50000)
        return self.pose_from_ikpy(ikpy_pose)

    def go_to_point(self, point, wait=True):
        """
        point should be a list: [X, Y, Z] IN ROBOT FRAME
        """
        logging.info('current position {}'.format(self.get_current_position()))
        pose = self.find_pose(point)
        logging.info('target position {}'.format(pose))
        self.command_pose(pose)
        if wait:
            self.wait_for_motion_complete()

    def get_current_position(self):
        pose = list()
        for i in range(len(self._motorNodes)):
            pose.append(self._motorSensorNodes[i].getValue())
        return pose

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
        self.wait_for_motion_complete()
        return None

    def print_status(self):
        for i in range(len(self._motorNodes)):
            print('MOTOR {}: POS: {} TAR: {}'.format(i,self._motorSensorNodes[i].getValue(),self._motorNodes[i].getTargetPosition()))
        print('===================')

    def transform_point_to_robot_frame(self, point):
        """
        The whole webots world is rotated 90 degrees for some reason.  To avoid breaking anything, I am working around this, but this should really be rectified at some point
        """
        newpoint = [x for x in point]
        newpoint.append(1)
        pArr = np.array(newpoint)
        T1 = np.array([[1, 0, 0, 0],
                    [0, 0, -1, 0],
                    [0, 1, 0, 0],
                    [0, 0, 0, 1]])
        T2 = np.array([[0, -1, 0, 0],
                    [1, 0, 0, 0],
                    [0, 0, 1, 0],
                    [0, 0, 0, 1]])
        newpoint2 = np.matmul(T2, np.matmul(T1,pArr))
        return list(newpoint2[0:3])

    """==========================================================Motion Commands================================================="""

    def indicate_object(self, point):
        logging.info('[aileen supervisor] :: Indicating Object')
        tPoint = self.transform_point_to_robot_frame(point)
        above = [tPoint[0], tPoint[1], tPoint[2]+.2]
        down = [tPoint[0], tPoint[1], tPoint[2]+.1]
        self.go_to_point(above)
        #self.print_status()
        self.go_to_point(down)
        #self.print_status()
        self.go_to_point(above)
        #self.print_status()
        self.return_home()
        #self.print_status()
        return None

    def pick_object(self, position):
        """
            position: list [X, Y, Z] of object to pick in world frame
        """
        logging.info('[aileen supervisor] :: Picking Object')
        position = [position[0], position[1]+.051, position[2]]
        self.go_to_point(self.transform_point_to_robot_frame(position))
        logging.info('[aileen supervisor] :: Locking')
        self._connectorNode.lock()
        currJnts = self.get_current_position()
        newJnts = currJnts
        newJnts[2] -= 3.14/4
        self.command_pose(newJnts)
        logging.debug('[aileen supervisor] :: Returning Home')
        self.return_home()
        return None

    def place_object(self, target):
        logging.info('[aileen supervisor] :: Placing Object')
        above_target = [target[0], target[1]+.25, target[2]]
        target = [target[0], target[1]+.051, target[2]]
        self.go_to_point(self.transform_point_to_robot_frame(above_target))
        self.go_to_point(self.transform_point_to_robot_frame(target))
        self._connectorNode.unlock()
        currJnts = self.get_current_position()
        newJnts = currJnts
        newJnts[2] -= 3.14/4
        self.command_pose(newJnts)
        self.return_home()
        return None

    def create_trajectory(self, waypoints):
        """
            Creates a list of positions for the object to follow
        """
        traj = list()
        traj.append(waypoints[0])
        nlegs = len(waypoints) - 1

        for i in np.arange(nlegs):
            #Find vector in 3d space
            delta_vec = [waypoints[i+1][0] - waypoints[i][0], waypoints[i+1][1] - waypoints[i][1], waypoints[i+1][2] - waypoints[i][2]]
            delta_mag = ((delta_vec[0]**2) + (delta_vec[1]**2) + (delta_vec[2]**2))**0.5
            #Calculate number of points in vector
            npoints = np.floor((delta_mag/settings.INSTRUCTOR_VELOCITY)/(settings.TIME_STEP/1000.0))
            #Find motion step through space
            delta_step = delta_vec/npoints
            for i in np.arange(npoints):
                traj.append(list(traj[-1]+delta_step))
        return traj

    def animate(self, node, positions):
        trans_field = node.getField('translation')
        for point in positions:
            trans_field.setSFVec3f(point)
            #node.resetPhysics()
            self.step(settings.TIME_STEP)
        return None

    def disable_physics(self, node):

        import pdb; pdb.set_trace()
        #node.getField('physics').importMFNode('/usr/local/webots/resources/nodes/Physics.wrl')
        return None

    def enable_physics(self, node):
        node.getField('physics').importMFNode('/usr/local/webots/resources/nodes/Physics.wrl')
        return None

    def pick_object_instructor(self, node, position):
        """
            object goes up and out to hold location.  parameterized in time by instructor INSTRUCTOR_VELOCITY
        """
        logging.info('[aileen supervisor] :: Demonstrating a pick')
        self.disable_physics(node)
        way1 = position[:]
        way1[1] += .15
        traj = self.create_trajectory([position, way1, settings.INSTRUCTOR_HOLD_POSITION])
        self.animate(node, traj)
        return None

    def place_object_instructor(self, node, target):
        logging.info('[aileen supervisor] :: Demonstrating a place')
        way1 = target[:]
        way1[1] += .15
        traj = self.create_trajectory([settings.INSTRUCTOR_HOLD_POSITION, way1, target])
        self.animate(node, traj)
        return None

    def test_ikpy(self, n=5):
        #generate faux object location, move to directly above it, move down, move up, move home
        for i in range(n):
            point = [np.random.random()*(settings.OBJECT_POSITION_MAX_X-settings.OBJECT_POSITION_MIN_X) + settings.OBJECT_POSITION_MIN_X,
                    np.random.random()*(settings.OBJECT_POSITION_MAX_Y-settings.OBJECT_POSITION_MIN_Y) + settings.OBJECT_POSITION_MIN_Y,
                    np.random.random()*(settings.OBJECT_POSITION_MAX_Z-settings.OBJECT_POSITION_MIN_Z) + settings.OBJECT_POSITION_MIN_Z]
            self.indicate_object(point)

    def return_home(self):
        self.command_pose(settings.HOME_POSE)
        self.wait_for_motion_complete()

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
            logging.info("[aileen_supervisor] :: held object is {}".format(self._held_node.getId()))
        else:
            logging.info("[aileen_supervisor] :: no object is currently held")

    def get_held_node(self):
        return self._held_node

    def get_number_object(self):
        self._numObj = self._camera.getRecognitionNumberOfObjects()
        return self._numObj

    def get_all(self):
        logging.debug("[aileen_supervisor] :: processing get_all from client")
        if settings.SIMULATE_CV:
            obj_dicts = []
            num_children = self._children.getCount()
            for i in range(0, num_children):
                object_node = self._children.getMFNode(i)
                object_name = object_node.getTypeName()
                if 'Solid' in object_name:
                    id = object_node.getId()
                    wcentroid = object_node.getPosition()
                    worientation = object_node.getOrientation()
                    wbbox_node = object_node.getField("boundingObject")
                    wbbox_size = wbbox_node.getSFNode().getField('size').getSFVec3f()
                    size_type = object_node.getField('description').getSFString()
                    object_dict = {'id_string': "ob{}".format(str(id)),
                                   'id': id,
                                   'position': wcentroid,
                                   'bbsize': wbbox_size,
                                   'world_centroid': wcentroid,
                                   'world_orientation': worientation,
                                   'wbbox_size': wbbox_size,
                                   'resolution': [self.resX, self.resY],
                                   'shape': self.get_object_shape(object_node),
                                   'color': self.get_object_color(object_node),
                                   'texture': self.get_object_texture(object_node),
                                   'hasCurveContour': self.get_object_hasCurveContour(object_node),
                                   'hasEdgeContour': self.get_object_hasEdgeContour(object_node),
                                   'hasPlane': self.get_object_hasPlane(object_node),
                                   'hasRectPlane': self.get_object_hasRectPlane(object_node),
                                   'hasRoundPlane': self.get_object_hasRoundPlane(object_node),
                                   'id_name': self.get_object_name(object_node),
                                   'size_type': size_type,
                                   'held': 'false'
                                   }
                    obj_dicts.append(object_dict)
        else:
            obj_dicts = []
            num_children = self._children.getCount()
            cnt_obj = 0
            for i in range(0, num_children):
                object_node = self._children.getMFNode(i)
                object_name = object_node.getTypeName()
                if 'Solid' in object_name:
                    cnt_obj += 1

            # Test if the camera can see all the generated objects
            if self._camera.getRecognitionNumberOfObjects() == cnt_obj and cnt_obj > 0:
                self.save = True
                self.num_rec = cnt_obj
            else:
                self.save = False

            objects = self._camera.getRecognitionObjects()
            # mask_img = self._camera.getRecognitionSegmentationImage()
            if settings.REC_SEG:
                mask_img = cv2.imread(settings.CURRENT_REC_SEG_IMAGE_PATH)
            img = cv2.imread(settings.CURRENT_IMAGE_PATH)
            for object in objects:
                # webots camera recognized object is linked with the gt object by the id
                id = object.get_id()
                child = self.getFromId(id) # gt child
                wcentroid = child.getPosition()
                worientation = child.getOrientation()
                wbbox_node = child.getField("boundingObject")
                wbbox_size = wbbox_node.getSFNode().getField('size').getSFVec3f()
                size_type = child.getField('description').getSFString()
                # print('size type = ', size_type)
                # size = bounding_obj.getField('size').getSFVec3f()

                # self.get_object_vision_concept()
                # self.get_object_size_type(child)
                obj_position = object.get_position()
                obj_orientation = object.get_orientation()
                position = object.get_position_on_image()
                bbsize = object.get_size_on_image()
                cx = int(position[0])
                cy = int(position[1])
                w = int(bbsize[0])
                h = int(bbsize[1])
                tx = int(cx - w / 2)
                ty = int(cy - h / 2)
                bx = int(cx + w / 2)
                by = int(cy + h / 2)

            # gray = cv2.cvtColor(mask_img.copy(), cv2.COLOR_BGR2GRAY)
            # mask = np.zeros(gray.shape[:2], dtype="uint8")
            # contour_mask = mask.copy()
            # cv2.rectangle(mask, (int(tx), int(ty)), (int(bx), int(by)), 255, -1)
            # tmp = cv2.bitwise_and(gray, gray, mask=mask)
            # non_zero_tmp = tmp[np.nonzero(tmp)]
            # if mode(non_zero_tmp)[0]:
            #     value = mode(non_zero_tmp)[0]
            #     # print target
            #     up = value + 5
            #     low = value - 5
            #     if low < 0:
            #         low = 0
            #     loc = np.where(np.logical_and(tmp > low, tmp <= up))
            #     tmp[loc] = value
            #     tmp[np.where(tmp != value)] = 0
            #     tmp[tmp > 0] = 100
            #     blurred = cv2.GaussianBlur(tmp, (5, 5), 0)
            #     img_canny = cv2.Canny(blurred, 50, 190)

                # cv2.imshow('contour', img_canny)
                # cv2.waitKey(0)
                # if size_type == "Small":
                #
                # elif size_type == "Medium":
                #
                # else:
                # contours, hierarchy = cv2.findContours(blurred.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)


                # self.get_object_vision_concept(tmp)

                object_dict = {'id_string': "ob{}".format(str(id)),
                               'id': id,
                               'position': position,
                               'bbsize': bbsize,
                               'bounding_box_camera': [tx, ty, bx, by],
                               'world_centroid': wcentroid,
                               'world_orientation': worientation,
                               'wbbox_size': wbbox_size,
                               # 'obj_relative_position':obj_position,
                               # 'obj_relative_orientation': obj_orientation,
                               'resolution': [self.resX, self.resY],
                               'shape': self.get_object_shape(child),
                               'color': self.get_object_color(child),
                               'texture': self.get_object_texture(child),
                               'hasCurveContour': self.get_object_hasCurveContour(child),
                               'hasEdgeContour': self.get_object_hasEdgeContour(child),
                               'hasPlane': self.get_object_hasPlane(child),
                               'hasRectPlane': self.get_object_hasRectPlane(child),
                               'hasRoundPlane': self.get_object_hasRoundPlane(child),
                               'id_name': self.get_object_name(child),
                               'size_type': size_type,
                               # 'focal_length': self.flen,
                               'held': 'false'
                           }
                obj_dicts.append(object_dict)
        output_dict = {'objects': obj_dicts, 'image': '', 'save': self.save, 'obj_num': self.num_rec}
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

        # for i in range(0, children.getCount()):
        #     size_type = children.getMFNode(i)
        #     print('shape_node', size_type)
        #     return size_type

    def get_object_vision_concept(self, image):
        resized = imutils.resize(image, width=300)
        ratio = image.shape[0] / float(resized.shape[0])
        blurred = cv2.GaussianBlur(image, (5, 5), 0)
        cnts = cv2.findContours(blurred.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        cv2.imshow('contour', cnts)
        cv2.waitKey(0)
        cnts = imutils.grab_contours(cnts)
        sd = ShapeDetector()

    def get_object_hasCurveContour(self, object_node):
        children = object_node.getField('children')
        for i in range(0, children.getCount()):
            shape_node = children.getMFNode(i)
            if shape_node.getTypeName() == "Shape":
                geometry_node = shape_node.getField('geometry').getSFNode()
                geometry_string = geometry_node.getTypeName()
                # print geometry_string.title()
                if geometry_string.title() == "Box":
                    return 0
                else:
                    return 1


    def get_object_hasEdgeContour(self, object_node):
        children = object_node.getField('children')
        for i in range(0, children.getCount()):
            shape_node = children.getMFNode(i)
            if shape_node.getTypeName() == "Shape":
                geometry_node = shape_node.getField('geometry').getSFNode()
                geometry_string = geometry_node.getTypeName()
                if geometry_string.title() == 'Sphere':
                    return 0
                else:
                    return 1


    def get_object_hasPlane(self, object_node):
        children = object_node.getField('children')
        for i in range(0, children.getCount()):
            shape_node = children.getMFNode(i)
            if shape_node.getTypeName() == "Shape":
                geometry_node = shape_node.getField('geometry').getSFNode()
                geometry_string = geometry_node.getTypeName()
                if geometry_string.title() == "Sphere":
                    return 0
                else:
                    return 1


    def get_object_hasRectPlane(self, object_node):
        children = object_node.getField('children')
        for i in range(0, children.getCount()):
            shape_node = children.getMFNode(i)
            if shape_node.getTypeName() == "Shape":
                geometry_node = shape_node.getField('geometry').getSFNode()
                geometry_string = geometry_node.getTypeName()
                if geometry_string.title() == "Box":
                    return 1
                else:
                    return 0


    def get_object_hasRoundPlane(self, object_node):
        children = object_node.getField('children')
        for i in range(0, children.getCount()):
            shape_node = children.getMFNode(i)
            if shape_node.getTypeName() == "Shape":
                geometry_node = shape_node.getField('geometry').getSFNode()
                geometry_string = geometry_node.getTypeName()
                if geometry_string.title() == "Cylinder" or geometry_string.title() == "Cone":
                    return 1
                else:
                    return 0


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
            # return [centroid[0] - size[0] / 2, centroid[1] - size[1] / 2, centroid[2] - size[2] / 2,
            #         centroid[0] + size[0] / 2, centroid[1] + size[1] / 2, centroid[2] + size[2] / 2]
            return [centroid[0], centroid[1], centroid[2], size[0], size[1], size[2]]
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
        # output_dict = self.get_all()
        logging.debug("[aileen_supervisor] :: processing get_image from client")
        _ = self._camera.getImage()
        logging.debug("[aileen_supervisor] :: got current image")
        dir_name = os.path.split(settings.CURRENT_IMAGE_PATH)[0]
        if not os.path.exists(dir_name):
            os.mkdir(dir_name)
        self._camera.saveImage(settings.CURRENT_IMAGE_PATH, 100)
        logging.debug("[aileen_supervisor] :: saved current image at {}".format(settings.CURRENT_IMAGE_PATH))
        if settings.REC_SEG:
            if self._camera.hasRecognitionSegmentation():
                self._camera.saveRecognitionSegmentationImage(settings.CURRENT_REC_SEG_IMAGE_PATH, 100)
                logging.debug("[aileen_supervisor] :: saved current recognition and segmentation image at {}".format(settings.CURRENT_REC_SEG_IMAGE_PATH))
            else:
                assert self._camera.hasRecognitionSegmentation()
                logging.debug("[aileen_supervisor] :: the camera has no recognition and segmentation function")
        output_dict = self.get_all()
        if settings.GET_IMAGE_RETURNS_IMAGE_BINARY:
            with open(settings.CURRENT_IMAGE_PATH, "rb") as handle:
                binary_image = xmlrpclib.Binary(handle.read())
                output_dict['image'] = binary_image
            if settings.REC_SEG:
                with open(settings.CURRENT_REC_SEG_IMAGE_PATH, "rb") as handle:
                    binary_image = xmlrpclib.Binary(handle.read())
                    output_dict['rec_seg_image'] = binary_image

        return output_dict

    def set_scene(self, scene_objects, label):
        self.clean_scene()
        logging.debug("[aileen_supervisor] :: trying to add new objects to the scene.")
        for scene_object in scene_objects:
            self._children.importMFNodeFromString(-1, scene_object)

        self.setLabel(1, str(label), 0.02, 0.3, 0.1, 0x000000, 0, "Arial")
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

        # print len(nodes_to_remove)
        for i in range(0, len(nodes_to_remove)):
            node_id = nodes_to_remove[i]
            node = self.getFromId(node_id)
            node.remove()

    @staticmethod
    def get_colors():
        with open(settings.COLOR_PATH) as f:
            colors = json.load(f)
        return colors
