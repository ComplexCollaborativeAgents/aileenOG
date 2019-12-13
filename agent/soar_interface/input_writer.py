import time
import os
from agent.log_config import logging
import xmlrpclib
from svs_helper import SVSHelper
from agent.vision.Detector import Detector
import cv2
import numpy as np
import settings
import language_helper

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
        self._concept_memory_status = None

        self._world_server = world_server

        if soar_agent:  # Enable stand alone testing
            self._input_link = soar_agent.get_input_link()
            self._world_link = self._input_link.CreateIdWME("world")
            self._objects_link = self._world_link.CreateIdWME("objects")
            self._qsrs_link = self._world_link.CreateIdWME("qsrs")

            self._interaction_link = self._input_link.CreateIdWME("interaction-link")
            self._clean_interaction_link_flag = False

            self._language_link = self._input_link.CreateIdWME("language-link")
            self._clean_language_link_flag = False

            self._concept_memory = self._input_link.CreateIdWME("concept-memory")
            self._clean_concept_memory_flag = False

            if settings.SOAR_CV:
                # ToDo: Move the input to Detector to the config file.
                self.detector = Detector("aileen_vision_module/aileen.names",
                                     "aileen_vision_module/yolov3-tiny-aileen.cfg",
                                     "aileen_vision_module/yolov3-tiny-aileen_900.weights",
                                     'color')

    def set_concept_memory_status(self, concept_memory_status_dictionary):
        self._concept_memory_status = concept_memory_status_dictionary

    def set_language(self, language_dictionary):
        self._language = language_dictionary

    def set_interaction(self, interaction_dictionary):
        self._interaction = interaction_dictionary

    def generate_input(self):
        time.sleep(settings.SOAR_SLEEP_TIME)

        if self._clean_interaction_link_flag:
            self.clean_interaction_link()

        if self._clean_language_link_flag:
            self.clean_language_link()

        if self._clean_concept_memory_flag:
            self.clean_concept_memory()

        if self._concept_memory_status is not None:
            self.write_concept_memory_status_to_input_link()

        if self._interaction is not None:
            self.write_interaction_dictionary_to_input_link()

        if self._language is not None:
            self.write_language_to_input_link()

        if settings.SOAR_CV:
            binary_image = self.request_server_for_current_state_image()
            self.write_binary_image_to_file(binary_image)
            im = cv2.imdecode(np.fromstring(binary_image.data, dtype=np.uint8), 1)
            cv_detections = self.detector.run(im)
            print cv_detections

        objects_list = self.request_server_for_objects_info()
        if objects_list is not None:
            self.add_objects_to_working_memory(objects_list)
        qsrs = self.create_qsrs(objects_list)
        self.write_qsrs_to_input_link(qsrs)
        self._soar_agent.commit()

    def write_qsrs_to_input_link(self, qsrs):
        self._soar_agent.delete_all_children(self._qsrs_link)
        for root_obj_id in qsrs:
            root_obj_qsrs = qsrs[root_obj_id]
            for target_obj_id in root_obj_qsrs:
                root_target_qsrs = root_obj_qsrs[target_obj_id]
                for qsr_type in root_target_qsrs:
                    qsr_value = root_target_qsrs[qsr_type]
                    qsr_id = self._qsrs_link.CreateIdWME('qsr')
                    qsr_id.CreateIntWME("root", int(root_obj_id))
                    qsr_id.CreateIntWME("target", int(target_obj_id))
                    qsr_id.CreateStringWME(qsr_type, qsr_value)

    def clean_concept_memory(self):
        self._soar_agent.delete_all_children(self._concept_memory)
        self._clean_concept_memory_flag = False

    def write_concept_memory_status_to_input_link(self):
        new_status_link = self._concept_memory.CreateIdWME("result")
        logging.debug("[input_writer] :: writing concept memory status to input link")

        if 'status' in self._concept_memory_status:
            new_status_link.CreateStringWME("status", self._concept_memory_status['status'])
            logging.debug("[input-writer] :: wrote status {}".format(self._concept_memory_status['status']))

        if 'concept-symbol' in self._concept_memory_status and 'gpool' in self._concept_memory_status:
            new_status_link.CreateStringWME("concept-symbol", self._concept_memory_status['concept-symbol'])
            new_status_link.CreateStringWME("gpool", str(self._concept_memory_status['gpool']))
            logging.debug("[input-writer] :: wrote concept symbol {} and gpool {}".format(self._concept_memory_status['concept-symbol'], self._concept_memory_status['gpool']))

        if 'matches' in self._concept_memory_status:
            matches_id = new_status_link.CreateIdWME('matches')
            for item in self._concept_memory_status['matches']:
                matches_id.CreateStringWME("id_name", str(item))
            logging.debug(
                "[input-writer] :: wrote matches {}".format(self._concept_memory_status['status']))

        self._concept_memory_status = None
        self._clean_concept_memory_flag = True

    def clean_language_link(self):
        self._soar_agent.delete_all_children(self._language_link)
        self._clean_language_link_flag = False

    def write_language_to_input_link(self):
        new_language_link = self._language_link.CreateIdWME("language")
        logging.debug("[input_writer] :: writing generated parse to input link")
        ## write all parses
        parses = self._language['parses']
        parses_link = new_language_link.CreateIdWME("parses")
        language_helper.translate_to_soar_structure(parses, parses_link)
        self._language = None
        self._clean_language_link_flag = True

    def clean_interaction_link(self):
        self._soar_agent.delete_all_children(self._interaction_link)
        self._clean_interaction_link_flag = False

    def write_interaction_dictionary_to_input_link(self):
        logging.debug("[input_writer] :: writing interaction to input link")
        new_interaction_link = self._interaction_link.CreateIdWME("message")
        if 'signal' in self._interaction:
            signal = str(self._interaction['signal'])
            new_interaction_link.CreateStringWME('signal', signal)
        if 'content' in self._interaction:
            content = str(self._interaction['content'])
            new_interaction_link.CreateStringWME('content', content.replace(" ", "-"))
        self._interaction = None
        self._clean_interaction_link_flag = True

    def process_interaction(self, interaction_dict):
        logging.debug("[input_writer] :: received training string: {}".format(interaction_dict))
        self._interaction = interaction_dict

    def add_objects_to_working_memory(self, objects_list):
        self._soar_agent.delete_all_children(self._objects_link)
        for w_object in objects_list:
            object_id = self._objects_link.CreateIdWME("object")
            object_id.CreateIntWME('id', w_object['id'])
            position_id = object_id.CreateIdWME('position')
            position_id.CreateFloatWME('x', w_object['position'][0])
            position_id.CreateFloatWME('y', w_object['position'][1])
            position_id.CreateFloatWME('z', w_object['position'][2])
            object_id.CreateStringWME('held', w_object['held'])
            object_id.CreateStringWME('color', w_object['color'])
            object_id.CreateStringWME('shape', w_object['shape'])
            object_id.CreateStringWME('id_string', w_object['id_string'])
            object_id.CreateStringWME('id_uuid', w_object['id_name'])

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
        dir_name = os.path.split(settings.CURRENT_IMAGE_PATH)[0]
        if not os.path.exists(dir_name):
            os.mkdir(dir_name)
        with open(settings.CURRENT_IMAGE_PATH, "wb") as handle:
            handle.write(binary_image.data)


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
