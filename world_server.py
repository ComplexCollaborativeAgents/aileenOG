from SimpleXMLRPCServer import SimpleXMLRPCServer
from SimpleXMLRPCServer import SimpleXMLRPCRequestHandler
import xmlrpclib

from log_config import logging
import socket
from threading import Thread
import constants
import time

class AileenWorldServer:

    def __init__(self, aileen_supervisor, port=10000):
        logging.info("[aileen_world_server] :: aileen_world_server connecting on port {}".format(port))

        self._quit = True
        self._port = port
        self._thread = None
        self._world_thread = None

        self._aileen_supervisor = aileen_supervisor

        # Restrict to a particular path
        class RequestHandler(SimpleXMLRPCRequestHandler):
            rpc_paths = ('/RPC2',)

        self._host = socket.gethostbyname("0.0.0.0")
        logging.info("[aileen_world_server] :: hostname: " + self._host)

        self._server = SimpleXMLRPCServer((self._host, self._port), requestHandler=RequestHandler)

        self._server.register_introspection_functions()

        def get_all():
            logging.info("[aileen_world_server] :: received get_all from agent client")
            output = aileen_supervisor.get_all()
            logging.debug("[aileen_world_server] :: sending response {}".format(output))
            return output

        def apply_action(action):
            logging.info("[aileen_world_server] :: received apply-action for {} from agent client".format(action))
            return aileen_supervisor.apply_action(action)

        def get_image():
            logging.info("[aileen_world_server] :: received get_image from agent client")
            binary_image = aileen_supervisor.get_image()
            logging.debug("[aileen_world_server] :: sending image in binary format")
            return binary_image

        def set_scene(scene_specification):
            logging.info("[aileen_world_server] :: received set_scene from instructor client")
            acknowledgement = aileen_supervisor.set_scene(scene_specification['configuration'], scene_specification['label'])
            logging.debug("[aileen_world_server] :: sending acknowledgement")
            return acknowledgement

        self._server.register_function(get_all, 'get_all')
        self._server.register_function(apply_action, 'apply_action')
        self._server.register_function(get_image, 'get_image')
        self._server.register_function(set_scene, 'set_scene')

    def run(self):
        while not self._quit:
            self._server.handle_request()
            logging.debug("[aileen_world_server] :: serving request {}".format(self._server.handle_request()))

    def update_world_in_background(self):
        while self._aileen_supervisor.step(constants.TIME_STEP) != -1:
            time.sleep(0.001)

    def run_in_background(self):
        self._world_thread = Thread(target=self.update_world_in_background, args=())
        self._world_thread.start()

        self._quit = False
        logging.info("[aileen_world_server] :: Starting aileen world server")
        self._thread = Thread(target=self.run, args=())
        self._thread.start()

    def stop(self):
        self._quit = True
