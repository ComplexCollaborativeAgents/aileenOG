from agent.soar_interface.soar_agent import SoarAgent
import cv2
import settings
import argparse
from scipy.stats import mode
import numpy as np
import xmlrpclib
from instructor.log_config import logging
from instructor.generate_training_images import TrainingImage
from instructor.aileen_object import Color, Size, AileenObject
import pytest

# interfaces = {'id', 'id_string', 'id_name', 'position', 'wposition', 'bounding_box_camera', 'bbsize', 'wbbox_size',
#               'color', 'shape', 'size_type', 'hasRectPlane', 'hasRoundPlane', 'hasPlane', 'hasEdgeContour',
#               'hasCurveContour'}
interfaces = {'id', 'id_string', 'id_name', 'position', 'wposition', 'bounding_box_camera', 'bbsize', 'wbbox_size',
              'color', 'shape', 'size_type'}

def create_connection_with_aileen_world():
    url = 'http://{}:{}'.format(settings.WORLD_HOST, settings.WORLD_PORT)
    server = xmlrpclib.ServerProxy(url)
    logging.info("[aileen_instructor] :: created a connection with the world server at: {}".format(url))
    return server


# def create_connection_with_aileen_agent():
#     url = 'http://{}:{}'.format(settings.AGENT_HOST, settings.AGENT_PORT)
#     server = xmlrpclib.ServerProxy(url)
#     logging.info("[aileen_instructor] :: created a connection with the agent: {}".format(url))
#     return server


def check_interface(dict, key):
    if key in dict.keys():
        return True
    else:
        print key
        return False

def test_detector_gt_interfaces():
# if __name__ == '__main__':
    world_server = create_connection_with_aileen_world()

    object1 = AileenObject(shape='box', color=Color('blue', [0, 0, 1]), size=Size('small', [.04, .04, .04]))
    object2 = AileenObject(shape='cone', color=Color('red', [1, 0, 0]), size=Size('medium', [.08, .08, .08]))
    object3 = AileenObject(shape='capsule', color=Color('red', [1, 0, 0]), size=Size('small', [.04, .04, .04]))
    objects = [object1, object2, object3]
    qualify = False
    while qualify is False:
        TT = TrainingImage()
        lesson = TT.generate_test_lesson(world_server, objects, num_objects=3)
        agent = SoarAgent(world_server)
        iwriter = agent._input_writer
        data = iwriter.request_server_for_current_state_image()
        qualify = data['save']

    objects_list = data['objects']
    img = cv2.imread(settings.CURRENT_IMAGE_PATH)
    mask_img = cv2.imread(settings.CURRENT_REC_SEG_IMAGE_PATH)
    # print('object 0 = ', objects_list[0])
    # print('object 1 = ', objects_list[1])
    cv_detections = iwriter.detector.run(img)

    aligned = iwriter.align_cv_detections_to_world(cv_detections, objects_list)#[0]

    gt = iwriter.request_server_for_objects_info()# [0]
    # for i in range(0, len(objects_list)):
    #     obj = objects_list[i]
    #     # obj = refine_bbox(mask_img, obj)
    #     tx, ty, bx, by = obj['bounding_box_camera']
    #     cv2.rectangle(img, (tx, ty), (bx, by), (0, 255, 0), thickness=2)

    # for obj in cv_detections['objects']:
    #     tx, ty, bx, by = obj['camera_bounding_box_mrcnn']
    #     cv2.rectangle(img, (tx, ty), (bx, by), (0, 0, 255), thickness=2)

    # for obj in aligned:
    #     tx, ty, bx, by = obj['bounding_box_camera']
    #     cv2.rectangle(img, (tx, ty), (bx, by), (255, 0, 0), thickness=2)

    # cv2.imshow('img', img)
    # cv2.waitKey()
    for i in range(len(gt)):
        print("Checking object " + str(i))
        print("check interfaces in ground-truth...")
        for key in interfaces:
            assert check_interface(gt[i], key)
        print("Interfaces in ground-truth are correct.")

    print("check interfaces of cv detection...")
    for i in range(len(aligned)):
        print("Checking object " + str(i))
        for key in interfaces:
            assert check_interface(aligned[i], key)
        print("Interfaces of cv detection are correct")
