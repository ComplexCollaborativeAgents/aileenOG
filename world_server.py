from SimpleXMLRPCServer import SimpleXMLRPCServer
from SimpleXMLRPCServer import SimpleXMLRPCRequestHandler
import xmlrpclib

from log_config import logging
import socket
from threading import Thread


class AileenWorldServer:

    def __init__(self, aileen_supervisor, port=10000):
        logging.info("[aileen_world_server] :: aileen_world_server connecting on port {}".format(port))

        self._quit = True
        self._port = port
        self._thread = None

        # Restrict to a particular path
        class RequestHandler(SimpleXMLRPCRequestHandler):
            rpc_paths = ('/RPC2',)

        # Create server

        self._host = socket.gethostbyname("0.0.0.0")
        logging.info("[aileen_world_server] :: hostname: " + self._host)

        self._server = SimpleXMLRPCServer((self._host, self._port),requestHandler=RequestHandler)

        self._server.register_introspection_functions()

        def get_all():
            logging.info("[aileen_world_server] :: received get_all from client")
            output = aileen_supervisor.get_all()
            logging.debug("[aileen_world_server] :: sending response {}".format(output))
            return output

        def apply_action(action):
            logging.info("[aileen_world_server] :: received apply-action for {}".format(action))
            return aileen_supervisor.apply_action(action)

        def dummy():
            logging.info("[aileen_world_server] :: server ending.")

        self._server.register_function(get_all, 'get_all')
        self._server.register_function(apply_action, 'apply_action')

    def run(self):
        while not self._quit:
            self._server.handle_request()

    def run_in_background(self):
        self._quit = False
        logging.info("[aileen_world_server] :: Starting aileen world server")
        self._thread = Thread(target=self.run, args=())
        self._thread.start()

    def stop(self):
        self._quit = True
        url = 'http://{}:{}'.format(self.host, self.port)
        s = xmlrpclib.client.ServerProxy(url)
        s.dummy()
