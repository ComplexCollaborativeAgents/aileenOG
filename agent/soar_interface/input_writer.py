import time
import os
from agent.log_config import logging
import xmlrpclib
from svs_helper import SVSHelper
from agent.vision.Detector import Detector
import cv2
import numpy as np
import settings
import torch
from scipy.stats import mode
from scipy.spatial import distance
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

        # if settings.SIMULATE_CV:
            self.detector = Detector()

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

        if settings.SIMULATE_CV is False:
            # these will be used to map UUID, ID, and held status to CV detections
            data = self.request_server_for_current_state_image()
            objects_list = data['objects']
            # Assumption: Both world and agent are running on the same machine and that's why the image is stored in
            #             `settings.CURRENT_IMAGE_PATH.
            # im = cv2.imdecode(np.fromfile(settings.CURRENT_IMAGE_PATH, dtype=np.uint8), 1)
            img = cv2.imread(settings.CURRENT_IMAGE_PATH)
            mask_img = cv2.imread(settings.CURRENT_REC_SEG_IMAGE_PATH)
            # print('image shape = ', np.shape(img))
            # img = np.moveaxis(img, -1, 0).astype(np.float32)
            # img = torch.from_numpy(img)
            # im = img / 255.0

            # print('world objects is:', objects_list)
            cv_detections = self.detector.run(img)
            # print('cv_detection is:', cv_detections)

            for i in range(len(objects_list)):
                w = objects_list[i]
                position = w['bbposition']
                bbsize = w['bbsize']
                tx = w['bounding_box_camera'][0]
                ty = w['bounding_box_camera'][1]
                bx = w['bounding_box_camera'][2]
                by = w['bounding_box_camera'][3]
                gray = cv2.cvtColor(mask_img, cv2.COLOR_BGR2GRAY)
                mask = np.zeros(gray.shape[:2], dtype="uint8")
                cv2.rectangle(mask, (int(tx), int(ty)), (int(bx), int(by)), 255, -1)
                tmp = cv2.bitwise_and(gray, gray, mask=mask)
                non_zero_tmp = tmp[np.nonzero(tmp)]
                if mode(non_zero_tmp)[0]:
                    value = mode(non_zero_tmp)[0]
                    # print target
                    up = value + 5
                    low = value - 5
                    if low < 0:
                        low = 0
                    loc = np.where(np.logical_and(tmp > low, tmp <= up))
                    tmp[loc] = value
                    tmp[np.where(tmp != value)] = 0
                    tmp[tmp > 0] = 100

                    # print np.shape(tmp)
                    x, y, w, h = cv2.boundingRect(tmp)

                    tx = x
                    ty = y
                    bx = x + w
                    by = y + h
                    position = [x + w/2.0, y + h/2.0]
                    bbsize = [bx - tx, by - ty]
                    objects_list[i]['bounding_box_camera'] = [tx, ty, bx, by]
                    objects_list[i]['bbposition'] = position
                    objects_list[i]['bbsize'] = bbsize

            # if len(cv_detections) > 0:
            #     detections = cv_detections['objects']
            #     for i in range(len(detections)):
            #         d = detections[i]
            #         w = objects_list[i]
            #         bbox1 = d['camera_bounding_box_mrcnn']
            #         bbox2 = w['bounding_box_camera']
            #         wtx = bbox2[0]
            #         wty = bbox2[1]
            #         wbx = bbox2[2]
            #         wby = bbox2[3]
            #
            #         dtx = bbox1[0]
            #         dty = bbox1[1]
            #         dbx = bbox1[2]
            #         dby = bbox1[3]
            #
            #         cv2.rectangle(img, (wtx, wty), (wbx, wby), (255, 0, 0), 2)
            #         cv2.rectangle(img, (dtx, dty), (dbx, dby), (0, 255, 0), 2)
            #         cv2.imshow('bbox', img)
            #         cv2.waitKey(0)

            objects_list = self.align_cv_detections_to_world(cv_detections, objects_list)
            # print('updated is:', objects_list)
            logging.debug("Aligned Detections: {}".format(objects_list))

        else:
            # data = self.request_server_for_current_state_image()
            # objects_list = data['objects']
            # objects_list = self.use_gt_world(objects_list)
            objects_list = self.request_server_for_objects_info()
            logging.debug("Groundtruth world: {}".format(objects_list))

        if objects_list is not None:
            self.add_objects_to_working_memory(objects_list)
        qsrs = self.create_qsrs(objects_list)
        self.write_qsrs_to_input_link(qsrs)
        self._soar_agent.commit()

    @staticmethod
    def align_cv_detections_to_world(cv_detections, objects_list):
        detections = cv_detections['objects']
        world = objects_list

        updated_detections = []

        mapped = np.zeros(len(world))
        for i in range(len(detections)):
            w_index = -1
            for w in world:
                w_index += 1
                if mapped[w_index]:
                    continue

                d = detections[i]
                bbox1 = d['camera_bounding_box_mrcnn']
                bbox2 = w['bounding_box_camera']
                center1 = d['camera_mrcnn_position']
                center2 = w['bbposition']

                dst = distance.euclidean(center1, center2)

                # detections should include the following fields, in addition to anything added below
                # 'shape': e.g., CVCone
                # 'camera_bounding_box_yolo': YOLO detection
                # 'camera_bounding_box_yolo_proj_to_world': World projection of YOLO detection
                # 'color': e.g., CVRed
                # 'camera_yolo_position': YOLO bounding box centroid
                # 'camera_yolo_position_proj_to_world': World projection of YOLO bounding box centroid
                # print Detector.bb_iou(bbox1, bbox2)
                # print dst
                if Detector.bb_iou(bbox1, bbox2) > .7 or dst < 4.0:
                # if dst < 4.0:
                    mapped[w_index] = 1
                    detections[i]['held'] = w['held']
                    detections[i]['id'] = w['id']
                    detections[i]['id_name'] = w['id_name']
                    detections[i]['id_string'] = w['id_string']
                    detections[i]['held'] = w['held']
                    #  The simulator groundtruth
                    detections[i]['position_simulator'] = w['position']
                    detections[i]['bounding_box_simulator'] = w['bounding_box_camera']
                    detections[i]['bbox_size_simulator'] = w['bbsize']
                    detections[i]['orientation'] = w['world_orientation']
                    detections[i]['wbbox_size'] = w['wbbox_size']
                    detections[i]['wbbox_position'] = w['world_centroid']
                    #  The detector output
                    #detections[i]['position'] = detections[i]['camera_mrcnn_position']
                    detections[i]['bounding_box'] = detections[i]['camera_bounding_box_mrcnn']
                    detections[i]['position'] = detections[i]['wbbox_position']

                    #  Extra attributes
                    detections[i]['hasPlane'] = w['hasPlane']
                    detections[i]['hasRectPlane'] = w['hasRectPlane']
                    detections[i]['hasRoundPlane'] = w['hasRoundPlane']
                    detections[i]['hasCurveContour'] = w['hasCurveContour']
                    detections[i]['hasEdgeContour'] = w['hasEdgeContour']

                    if settings.DETECTOR_MODE == 2:
                        detections[i]['cluster_id'] = detections[i]['cluster_id']

                    ##  SM: if settings have preload visual concepts on, just use information from the world to ensure 100% detection
                    if settings.AGENT_VISUAL_CONCEPTS_PARAM == 'soar' and settings.AGENT_PRELOAD_VISUAL_CONCEPTS_PARAM == 'true':
                        detections[i]['color'] = w['color']
                        detections[i]['shape'] = w['shape']

                    updated_detections.append(detections[i])
                    break

        return updated_detections

    @staticmethod
    def use_gt_world(objects_list):
        world = objects_list
        detections = objects_list
        updated_detections = []
        for i in range(len(world)):
            detections[i]['id'] = world[i]['id']
            detections[i]['id_name'] = world[i]['id_name']
            detections[i]['id_string'] = world[i]['id_string']
            detections[i]['held'] = world[i]['held']
            #  The simulator groundtruth
            # detections[i]['position'] = world[i]['bbposition']
            detections[i]['position'] = world[i]['world_centroid']
            detections[i]['bounding_box'] = world[i]['bounding_box_camera']

            detections[i]['orientation'] = world[i]['world_orientation']
            detections[i]['wbbox_size'] = world[i]['world_bbox_size']
            detections[i]['wbbox_position'] = world[i]['world_centroid']
            # #  The detector output
            # detections[i]['position'] = world[i]['bbposition']
            # detections[i]['bounding_box'] = world[i]['bounding_box_camera']

            #  Extra attributes
            detections[i]['hasPlane'] = world[i]['hasPlane']
            detections[i]['hasRectPlane'] = world[i]['hasRectPlane']
            detections[i]['hasRoundPlane'] = world[i]['hasRoundPlane']
            detections[i]['hasCurveContour'] = world[i]['hasCurveContour']
            detections[i]['hasEdgeContour'] = world[i]['hasEdgeContour']

            if settings.DETECTOR_MODE == 2:
                detections[i]['cluster_id'] = world[i]['cluster_id']

            ##  SM: if settings have preload visual concepts on, just use information from the world to ensure 100% detection
            if settings.AGENT_VISUAL_CONCEPTS_PARAM == 'soar' and settings.AGENT_PRELOAD_VISUAL_CONCEPTS_PARAM == 'true':
                detections[i]['color'] = world[i]['color']
                detections[i]['shape'] = world[i]['shape']

            updated_detections.append(detections[i])

        return updated_detections

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
        logging.debug("[input_writer] :: writing concept memory status to input link for keys {}".format(self._concept_memory_status.keys()))

        if 'status' in self._concept_memory_status:
            new_status_link.CreateStringWME("status", self._concept_memory_status['status'])
            logging.debug("[input-writer] :: wrote status {}".format(self._concept_memory_status['status']))

        if 'gpool' in self._concept_memory_status:
            new_status_link.CreateStringWME("gpool", str(self._concept_memory_status['gpool']))
            logging.debug("[input-writer] :: gpool {}".format(self._concept_memory_status['gpool']))

        if 'matches' in self._concept_memory_status:
            matches_id = new_status_link.CreateIdWME('matches')
            logging.debug("[input-writer] :: {}".format(self._concept_memory_status))
            if 'matches' in self._concept_memory_status:
                if self._concept_memory_status['matches'] is not None and len(self._concept_memory_status['matches']) > 0:
                    for match in self._concept_memory_status['matches']:
                        if match is not None:
                            logging.debug("[input_writer] :: match {}".format(match))
                            match_id = matches_id.CreateIdWME("match")
                            match_id.CreateStringWME("first", str(match[0]))
                            match_id.CreateStringWME("second", str(match[1]))
                            match_id.CreateStringWME("third", str(match[2]))
                            logging.debug(
                                        "[input-writer] :: wrote matches {}".format(self._concept_memory_status['status']))
        if 'projections' in self._concept_memory_status.keys():
            projects_id = new_status_link.CreateIdWME('projections')
            filtered = self.filter_cis_for_next_state(self._concept_memory_status['projections'])
            logging.debug("[input-writer] :: filtered cis {}".format(filtered))
            for item in filtered:
                if item is not None:
                    logging.debug("[input_writer] :: match {}".format(item))
                    item_id = projects_id.CreateIdWME("project")
                    item_id.CreateStringWME("first", str(item[0]))
                    item_id.CreateStringWME("second", str(item[1]))
                    if len(item) == 3:
                        item_id.CreateStringWME("third", str(item[2]))
                    logging.debug("[input-writer] :: wrote projects {}".format(self._concept_memory_status['status']))

        self._concept_memory_status = None
        self._clean_concept_memory_flag = True

    def filter_cis_for_next_state(self, candidate_inferences):
        for ci in candidate_inferences:
            if ci[0] == 'startsAfterEndingOf':
                episode_identifier = ci[1]
                candidate_inferences.remove(ci)

        filtered = []
        for ci in candidate_inferences:
            if ci[1] == episode_identifier:
                if ci[0] == 'aileenTerminalTransition':
                    filtered.append(['type','terminal_state'])
                else:
                    filtered.append(ci[2])
        return filtered


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
            new_interaction_link.CreateStringWME('content', content.replace('-', ' '))
        if 'marker' in self._interaction:
            marker = str(self._interaction['marker'])
            new_interaction_link.CreateStringWME('marker', marker)
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
            if 'position' in w_object:
                position_id = object_id.CreateIdWME('position')
                position_id.CreateFloatWME('x', w_object['position'][0])
                position_id.CreateFloatWME('y', w_object['position'][1])
                position_id.CreateFloatWME('z', w_object['position'][2])
            if 'bounding_box' in w_object:
                size_id = object_id.CreateIdWME('size_bb')
                # size_id.CreateFloatWME('xsize', w_object['bounding_box'][2]-w_object['bounding_box'][0])
                # size_id.CreateFloatWME('zsize', w_object['bounding_box'][5]-w_object['bounding_box'][2])
                # size_id.CreateFloatWME('ysize', w_object['bounding_box'][3]-w_object['bounding_box'][1])
                size_id.CreateFloatWME('xsize', w_object['wbbox_size'][0])
                size_id.CreateFloatWME('zsize', w_object['wbbox_size'][2])
                size_id.CreateFloatWME('ysize', w_object['wbbox_size'][1])
                object_id.CreateStringWME('size', self.size_from_bounding_box(w_object['bounding_box']))
            object_id.CreateStringWME('held', w_object['held'])
            object_id.CreateStringWME('color', str(w_object['color']))
            object_id.CreateStringWME('shape', w_object['shape'])
            object_id.CreateIntWME('hasPlane', w_object['hasPlane'])
            object_id.CreateIntWME('hasRectPlane', w_object['hasRectPlane'])
            object_id.CreateIntWME('hasRoundPlane', w_object['hasRoundPlane'])
            object_id.CreateIntWME('hasEdgeContour', w_object['hasEdgeContour'])
            object_id.CreateIntWME('hasCurveContour', w_object['hasCurveContour'])
            object_id.CreateIntWME('id_string', w_object['id'])
            object_id.CreateStringWME('id_uuid', w_object['id_name'])

    def request_server_for_objects_info(self):
        try:
            objects_dict = self._world_server.get_all()
            print('objects dict = ', objects_dict)
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
            data = self._world_server.get_image()
        except xmlrpclib.ProtocolError as err:
            logging.error("[input_writer] :: protocol error {}".format(err.errmsg))
            return
        except xmlrpclib.Fault as fault:
            logging.error("[input_writer] :: fault code {}; fault string{}".format(fault.faultCode, fault.faultString))
            return

        logging.debug("[input_writer] :: received binary image from server")
        return data

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
            print('object = ', obj)
            if obj['held'] == 'false':
                world.add_object_state_series(
                    [Object_State(name=str(obj['id']),timestamp=0,
                                  x=obj['position'][0],
                                  y=obj['position'][2],
                                  z=obj['position'][1],
                                  xsize=obj['wbbox_size'][0],
                                  zsize=obj['wbbox_size'][1],
                                  ysize=obj['wbbox_size'][2]
                                  # xsize=obj['bounding_box'][3]-obj['bounding_box'][0],
                                  # ysize=obj['bounding_box'][5]-obj['bounding_box'][2],
                                  # zsize=obj['bounding_box'][4]-obj['bounding_box'][1]
                    )])

        qsrlib_request_message = QSRlib_Request_Message(["rcc8","cardir","ra", "3dcd"], world)
        qsrlib_response_message = qsrlib.request_qsrs(req_msg=qsrlib_request_message)
        ret = {}
        for t in qsrlib_response_message.qsrs.get_sorted_timestamps():
            for k, v in zip(qsrlib_response_message.qsrs.trace[t].qsrs.keys(),
                            qsrlib_response_message.qsrs.trace[t].qsrs.values()):
                args = k.split(',')
                if args[0] not in ret:
                    ret[args[0]] = {}
                if 'ra' in v.qsr.keys():
                    v.qsr['ra'] = self.get_rcc8_symbols_for_allen_intervals(v.qsr['ra'])
                if '3dcd' in v.qsr.keys():
                    v.qsr['depth'] = v.qsr['3dcd'][-1].lower()
                ret[args[0]][args[1]] = v.qsr
        logging.debug("[input_writer] :: qsrs computed {}".format(ret))
        return ret

    def size_from_bounding_box(self, bbox):
        """
        Place holder for something more principled.
        Arbitrary thresholds as a first pass -> clustering on witnessed examples later
        """
        # area = abs((bbox[3] - bbox[0]) * (bbox[5] - bbox[2]))
        area = abs((bbox[2] - bbox[0]) * (bbox[3] - bbox[1]))
        if area < settings.SIZE_SM:
            return 'CVSmall'
        elif area < settings.SIZE_ML:
            return 'CVMedium'
        else:
            return 'CVLarge'

    def get_rcc8_symbols_for_allen_intervals(self, symbols):
        """
        We lose a bit of information here, but it works with the current concept learner so we have it as a stopgap
        mapping:
        <   dc
        >   dc
        =   eq
        m   ec
        mi  ec
        o   po
        oi  po
        d   ntpp
        di  ntppi
        s   tpp
        si  tppi
        f   tpp
        fi  tppi
        """

        symbol_split = symbols.split(',')
        new_symbols = ''
        count = 0
        for symbol in symbol_split:
            count += 1
            if symbol == '<' or symbol == '>':
                new_symbols += 'dc'
            if symbol == '=':
                new_symbols += 'eq'
            if symbol == 'm' or symbol == 'mi':
                new_symbols += 'ec'
            if symbol == 'o' or symbol == 'oi':
                new_symbols += 'po'
            if symbol == 'd':
                new_symbols += 'ntpp'
            if symbol == 'di':
                new_symbols += 'ntppi'
            if symbol in ['s','f']:
                new_symbols += 'tpp'
            if symbol in ['si', 'fi']:
                new_symbols += 'tppi'
            if count != len(symbol_split):
                new_symbols += ','
        return new_symbols
