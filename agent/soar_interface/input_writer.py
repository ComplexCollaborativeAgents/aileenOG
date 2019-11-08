import time
import os
from agent.log_config import logging
import xmlrpclib
from svs_helper import SVSHelper
from agent.configuration import Configuration
from agent import constants
#from aileen_vision_module.Detector import Detector
import cv2
import numpy as np

try:
    from qsrlib.qsrlib import QSRlib, QSRlib_Request_Message
    from qsrlib_io.world_trace import Object_State, World_Trace
except:
    logging.fatal("[input_writer] :: cannot find spatial reasoning library")

class InputWriter(object):
    def __init__(self, soar_agent, world_server):
        self._soar_agent = soar_agent
        self._world_server = world_server
        self._interaction = None
        self._language = None

        if soar_agent:  # Enable stand alone testing
            self._input_link = soar_agent.get_input_link()
            self._world_link = self._input_link.CreateIdWME("world")
            self._objects_link = self._world_link.CreateIdWME("objects")
            self._interaction_link = self._input_link.CreateIdWME("interaction")
            self._language_link = self._input_link.CreateIdWME("language")

        if Configuration.config['RunParams']['cv'] == "True":
            # ToDo: Move the input to Detector to the config file.
            self.detector = Detector("aileen_vision_module/aileen.names",
                                     "aileen_vision_module/yolov3-tiny-aileen.cfg",
                                     "aileen_vision_module/yolov3-tiny-aileen_900.weights",
                                     'color')
        self._svs_objects = []

    def set_language(self, language_dictionary):
        self._language = language_dictionary

    def set_interaction(self, interaction_dictionary):
        self._interaction = interaction_dictionary

    def generate_input(self):
        time.sleep(Configuration.config['Soar']['sleep-time'])

        if self._interaction is not None:
            self.write_interaction_dictionary_to_input_link()

        if self._language is not None:
            self.write_language_to_input_link()

        if Configuration.config['RunParams']['cv'] == "True":
            binary_image = self.request_server_for_current_state_image()
            self.write_binary_image_to_file(binary_image)
            im = cv2.imdecode(np.fromstring(binary_image.data, dtype=np.uint8), 1)
            cv_detections = self.detector.run(im)
            print cv_detections

        objects_list = self.request_server_for_objects_info()
        if objects_list is not None:
            self.add_objects_to_working_memory(objects_list)
            if Configuration.config['RunParams']['svs'] == "true":
                self.add_objects_to_svs(objects_list)

        qsrs = self.create_qsrs(objects_list)

    def write_language_to_input_link(self):
        logging.debug("[input_writer] :: writing generated parse to input link")

        self.delete_all_children(self._language_link)

        ## write all parses
        parses = self._language['parse']
        parses_link = self._language_link.CreateIdWME("parses")
        for parse in parses:
            parse_link = parses_link.CreateIdWME("parse")
            item = parse[0]
            if item == 'obj': ### function is partially written, will only write obj parses to Soar
                obj_ref_link = parse_link.CreateIdWME("obj-ref")
                i = 1
                while isinstance(parse[i], list): ## property
                    property = parse[i]
                    assert property[0] == 'prop'
                    prop_link = obj_ref_link.CreateIdWME('prop')
                    prop_link.CreateStringWME('tag', property[1])
                    i = i+1
                obj_ref_link.CreateStringWME('tag', parse[i])
        self._language = None



    def write_interaction_dictionary_to_input_link(self):
        self.delete_all_children(self._interaction_link)
        signal = str(self._interaction['signal'])
        content = str(self._interaction['content'])
        self._interaction_link.CreateStringWME('signal', signal)
        self._interaction_link.CreateStringWME('content', content)
        self._interaction = None

    def process_interaction(self, interaction_dict):
        logging.debug("[input_writer] :: received training string: {}".format(interaction_dict))
        self._interaction = interaction_dict

    ## SM: both these methods need to be rewritten to maintain the list of objects properly
    def add_objects_to_svs(self, objects_list):
        logging.info("[input_writer] :: writing objects to SVS")
        for w_object in objects_list:
            object_id = "object{}".format(w_object['id'])
            if object_id in self._svs_objects:
                svs_command = SVSHelper.get_svs_command_for_change_position(object_id,
                                                                            w_object['position'])
            else:
                svs_command = SVSHelper.get_svs_command_for_add_box(object_id,
                                                                    position=w_object['position'],
                                                                    bounding_box=w_object['bounding_box'])
                self._svs_objects.append(object_id)

            self._soar_agent._agent.SendSVSInput(svs_command)
            logging.debug("[input_writer] :: updating svs with {}".format(svs_command))

    def add_objects_to_working_memory(self, objects_list):
        self.delete_all_children(self._objects_link)
        for w_object in objects_list:
            object_id = self._objects_link.CreateIdWME("object")
            object_id.CreateIntWME('id', w_object['id']),
            position_id = object_id.CreateIdWME('position')
            position_id.CreateFloatWME('x', w_object['position'][0])
            position_id.CreateFloatWME('y', w_object['position'][1])
            position_id.CreateFloatWME('z', w_object['position'][2])
            object_id.CreateStringWME('held', w_object['held'])

    def request_server_for_objects_info(self):
        try:
            objects_dict = self._world_server.get_all()
        except xmlrpclib.ProtocolError as err:
            logging.error("[input_writer] :: protocol error {}".format(err.errmsg))
            return
        except xmlrpclib.Fault as fault:
            logging.error("[input_writer] :: fault code {}; fault string{}".format(fault.faultCode, fault.faultString))
            return

        logging.debug("[input_writer] :: received objects from server {}".format(objects_dict))
        objects_list = objects_dict['objects']
        return objects_list

    def request_server_for_current_state_image(self):
        try:
            binary_image = self._world_server.get_image()
        except xmlrpclib.ProtocolError as err:
            logging.error("[input_writer] :: protocol error {}".format(err.errmsg))
            return
        except xmlrpclib.Fault as fault:
            logging.error("[input_writer] :: fault code {}; fault string{}".format(fault.faultCode, fault.faultString))
            return

        logging.debug("[input_writer] :: received binary image from server")
        return binary_image

    def write_binary_image_to_file(self, binary_image):
        dir_name = os.path.split(constants.CURRENT_IMAGE_PATH)[0]
        if not os.path.exists(dir_name):
            os.mkdir(dir_name)
        with open(constants.CURRENT_IMAGE_PATH, "wb") as handle:
            handle.write(binary_image.data)


    def delete_all_children(self, id):
        index = 0
        if id.GetNumberChildren is not None:
            for i in range(0, id.GetNumberChildren()):
                child = id.GetChild(
                    index)  # remove the 0th child several times, Soar kernel readjusts the list after an item is deletd
                if child is not None:
                    child.DestroyWME()



    def create_qsrs(self, objects):
        '''
- Ignore y position as everything is on the table (use z instead)
- While qsrlib allows includes a parameter for orientation/rotation, it is not clear it is used. It is being ignored for now.

Saving sample input from aileen world for later testing.

objects = [{'orientation': [1.0, -5.75539615965681e-17, 3.38996313371214e-17, 5.75539615965681e-17, 1.0, 2.98427949019241e-17, -3.38996313371214e-17, -2.98427949019241e-17, 1.0], 'bounding_object': 'Box', 'held': 'false', 'bounding_box': [0.721, 0.3998037998119487, -0.249, 0.8210000000000001, 0.49980379981194867, -0.14900000000000002], 'position': [0.771, 0.4498037998119487, -0.199], 'id': 397}, {'orientation': [1.0, 4.8853319907279786e-17, -4.193655877514327e-17, -4.8853319907279786e-17, 1.0, -1.80524117148876e-16, 4.193655877514327e-17, 1.80524117148876e-16, 1.0], 'bounding_object': 'Cylinder', 'held': 'false', 'bounding_box': [0.369851, 0.39992295234206066, 0.067742, 0.46985099999999996, 0.49992295234206063, 0.167742], 'position': [0.419851, 0.44992295234206064, 0.117742], 'id': 403}]
        '''
        qsrlib = QSRlib() # We don't need a new object each time
        world = World_Trace()
        for obj in objects:
            world.add_object_state_series(
                [Object_State(name=str(obj['id']),timestamp=0,
                              x=obj['position'][0],
                              y=obj['position'][2],
                              xsize=obj['bounding_box'][3]-obj['bounding_box'][0],
                              ysize=obj['bounding_box'][5]-obj['bounding_box'][2]
                )])
        qsrlib_request_message = QSRlib_Request_Message(["rcc8","cardir"], world)
        qsrlib_response_message = qsrlib.request_qsrs(req_msg=qsrlib_request_message)
        ret = {}
        for t in qsrlib_response_message.qsrs.get_sorted_timestamps():
            for k, v in zip(qsrlib_response_message.qsrs.trace[t].qsrs.keys(),
                            qsrlib_response_message.qsrs.trace[t].qsrs.values()):
                args = k.split(',')
                if args[0] not in ret:
                    ret[args[0]] = {}
                ret[args[0]][args[1]] = v.qsr
        logging.info("[input_writer] :: qsrs computed {}".format(ret))
        return ret
