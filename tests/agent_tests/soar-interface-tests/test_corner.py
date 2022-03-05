from agent.soar_interface.soar_agent import SoarAgent
import cv2
import settings
import argparse
import xmlrpclib
from instructor.log_config import logging
from instructor.generate_training_images import TrainingImage
from instructor.aileen_object import Color, Size, AileenObject
import pytest

interfaces = {'id', 'id_string', 'id_name', 'position', 'wposition', 'bounding_box_camera', 'bbsize', 'wbbox_size',
              'color', 'shape', 'size_type', 'hasRectPlane', 'hasRoundPlane', 'hasPlane', 'hasEdgeContour',
              'hasCurveContour'}


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


def refine_bbox(mask, obj):
    gray = cv2.cvtColor(mask.copy(), cv2.COLOR_BGR2GRAY)
    x, y, w, h = cv2.boundingRect(gray)
    tx = x
    ty = y
    bx = x + w
    by = y + h
    position = [x + w / 2.0, y + h / 2.0]
    bbsize = [bx - tx, by - ty]
    obj['position'] = position
    obj['bbsize'] = bbsize
    obj['bounding_box_camera'] = [tx, ty, bx, by]

    return obj


def test_detector_gt_interfaces():
# if __name__ == '__main__':
    world_server = create_connection_with_aileen_world()

    object1 = AileenObject(shape='cylinder', color=Color('blue', [0, 0, 1]), size=Size('small', [.04, .04, .04]))
    object2 = AileenObject(shape='cone', color=Color('red', [1, 0, 0]), size=Size('medium', [.08, .08, .08]))
    # TrainingImage.generate_test_scene_location(world_server, object1)
    TrainingImage.generate_test_scene(world_server, object1)
    agent = SoarAgent(world_server)
    iwriter = agent._input_writer
    data = iwriter.request_server_for_current_state_image()
    objects_list = data['objects']
    print len(objects_list)
    if len(objects_list) is 0:
        print("No objects are detected.")
    else:
        img = cv2.imread(settings.CURRENT_IMAGE_PATH)
        mask_img = cv2.imread(settings.CURRENT_REC_SEG_IMAGE_PATH)
        objects_list[0] = refine_bbox(mask_img, objects_list[0])

        cv_detections = iwriter.detector.run(img)

        aligned = iwriter.align_cv_detections_to_world(cv_detections, objects_list)[0]
        for key in interfaces:
            assert check_interface(aligned, key)
            # if check_interface(aligned, key) is False:
            #     print key
        print("Interfaces of cv detection are correct")

    gt = iwriter.request_server_for_objects_info()[0]

    print("check interfaces in ground-truth...")
    for key in interfaces:
        assert check_interface(gt, key)
    print("Interfaces in ground-truth are correct.")

    print("check interfaces of cv detection...")

