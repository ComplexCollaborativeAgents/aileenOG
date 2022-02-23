from agent.soar_interface.soar_agent import SoarAgent
import cv2
import settings
import argparse
import xmlrpclib
from instructor.log_config import logging
from instructor.generate_training_images import TrainingImage
from instructor.aileen_object import Color, Size, AileenObject


def create_connection_with_aileen_world():
    url = 'http://{}:{}'.format(settings.WORLD_HOST, settings.WORLD_PORT)
    server = xmlrpclib.ServerProxy(url)
    logging.info("[aileen_instructor] :: created a connection with the world server at: {}".format(url))
    return server


def create_connection_with_aileen_agent():
    url = 'http://{}:{}'.format(settings.AGENT_HOST, settings.AGENT_PORT)
    server = xmlrpclib.ServerProxy(url)
    logging.info("[aileen_instructor] :: created a connection with the agent: {}".format(url))
    return server


def parse():
    parser = argparse.ArgumentParser(description='Aileen instructor.')
    parser.add_argument('--test-data-generation', action='store_true', help='Test vision data generation')
    parser.add_argument('--clean', action='store_true', help='Clean table')
    parser.add_argument('--test-cv', help='Test SIMULATE_CV', action='store_true')
    return parser.parse_args()


if __name__ == '__main__':
    arguments = parse()
    world_server = create_connection_with_aileen_world()
    agent_server = create_connection_with_aileen_agent()
    if arguments.test_data_generation:
        print('Testing training data generation.')
        TrainingImage.generate_scenes(world_server, agent_server)

    elif arguments.clean:
        print('Cleaning table.')
        TrainingImage.clean_scenes(world_server)

    else:
        object = AileenObject(shape='capsule', color=Color('blue', [0, 0, 1]), size=Size('medium', [.1, .1, .1]))
        TrainingImage.generate_test_scene(world_server, object)
        agent = SoarAgent(world_server)
        iwriter = agent._input_writer
        settings.SIMULATE_CV = False
        data = iwriter.request_server_for_current_state_image()
        objects_list = data['objects']

        img = cv2.imread(settings.CURRENT_IMAGE_PATH)
        mask_img = cv2.imread(settings.CURRENT_REC_SEG_IMAGE_PATH)
        gray = cv2.cvtColor(mask_img, cv2.COLOR_BGR2GRAY)
        x, y, w, h = cv2.boundingRect(gray)
        tx = x
        ty = y
        bx = x + w
        by = y + h
        position = [x + w / 2.0, y + h / 2.0]
        bbsize = [bx - tx, by - ty]
        objects_list[0]['bounding_box_camera'] = [tx, ty, bx, by]
        objects_list[0]['position'] = position
        objects_list[0]['bbsize'] = bbsize

        cv_detections = iwriter.detector.run(img)

        aligned = iwriter.align_cv_detections_to_world(cv_detections, objects_list)
        gt = iwriter.request_server_for_objects_info()
        # logging.debug("Aligned Detections: {}".format(objects_list))
        settings.SIMULATE_CV = True
        # logging.debug("Groundtruth world: {}".format(objects_list))

        # print ground-truth world information
        for key, value in gt[0].items():
            print(key, value)
        print("#########################################################################################")
        # print detection
        for key, value in cv_detections['objects'][0].items():
            print(key, value)
        print("#########################################################################################")
        # print detections after aligned with ground-truth
        for key, value in objects_list[0].items():
            print(key, value)
        print("#########################################################################################")

