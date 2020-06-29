from threading import Thread
from controller import Supervisor
import settings
from agent.vision.Detector import Detector
import os
import xmlrpclib
from world.controllers.action_executor import ActionExecutor
from world.log_config import logging
import json
import settings
from world.controllers.aileen_supervisor import AileenSupervisor
from world.world_server import AileenWorldServer
import time


PLACE_LOCATION = [.5, 0, .5]

def create_and_run_aileen_world_server(controller):
    server = AileenWorldServer(controller, port=settings.WORLD_PORT)
    server.run_in_background()

def create_connection_with_aileen_world():
    url = 'http://{}:{}'.format(settings.WORLD_HOST, settings.WORLD_PORT)
    server = xmlrpclib.ServerProxy(url)
    logging.info("[aileen] :: created a connection with the world server at: {}".format(url))
    return server

if __name__ == "__main__":
    aileen_supervisor = AileenSupervisor()
    aileen_executor = ActionExecutor(aileen_supervisor)
    create_and_run_aileen_world_server(aileen_supervisor)
    logging.info("[picking_test] :: Created World")
    world_server = create_connection_with_aileen_world()
    logging.info("[picking_test] :: Created Connection With World")
    obj = world_server.get_all()
    #Grab objects and throw them down on the ground
    pick_locs = []

    for object in obj['objects']:
        #Create Fake Pick Command
        pick = {'name':'pick-up', 'id':object['id'], 'requestor':'instructor'}
        #Pass to executor to execute
        aileen_executor.process_action_command(pick)
        place_pos = object['position']
        place_pos[0] += .15
        time.sleep(3)
        place = {'name':'place', 'location':object['position'], 'requestor':'instructor'}
        aileen_executor.process_action_command(place)
    """
    for loc in pick_locs:
        loc = [loc[0], loc[1]+.051, loc[2]]
        print('Planned loc {}'.format(loc))
        aileen_supervisor.go_to_point(aileen_supervisor.transform_point_to_robot_frame(loc))
        logging.info('Locking')
        time.sleep(10)
        print('{}'.format(aileen_supervisor.get_current_position()))
        aileen_supervisor._connectorNode.lock()
        #aileen_supervisor.command_pose(settings.HOME_POSE)
        currPose = aileen_supervisor.get_current_position()
        newPose = currPose
        newPose[2] -= 3.14/4
        newPose[0] += 1.57
        aileen_supervisor.command_pose(newPose)
        #time.sleep(.25)
        aileen_supervisor.go_to_point(aileen_supervisor.transform_point_to_robot_frame([0, .1, -1]))
        aileen_supervisor._connectorNode.unlock()
        aileen_supervisor.return_home()
    """
    while 1:
        time.sleep(.01)
