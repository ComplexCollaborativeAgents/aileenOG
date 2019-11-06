from SimpleXMLRPCServer import SimpleXMLRPCServer
from SimpleXMLRPCServer import SimpleXMLRPCRequestHandler
from log_config import logging
import socket
from threading import Thread


class AileenAgentServer():
    def __init__(self, aileen_agent, port=11000):
        logging.info("[aileen_agent_server] :: aileen_agent_server connecting on port {}".format(port))

        self._port = port
        self._quit = True
        self._thread = None
        self._agent = aileen_agent

        self._host = socket.gethostbyname("0.0.0.0")
        logging.info("[aileen_world_server] :: starting aileen_agent_host on {}".format(self._host))

        class RequestHandler(SimpleXMLRPCRequestHandler):
            rpc_paths = ('/RPC2',)

        self._server = SimpleXMLRPCServer((self._host, self._port), requestHandler=RequestHandler)

        self._server.register_introspection_functions()

        def process_language(language_dict):
            acknowledgement = True
            print language_dict
            #aileen_agent.process_language(language_dict['language'])
            return acknowledgement

        self._server.register_function(process_language, 'process_language')

    def run(self):
        while not self._quit:
            self._server.handle_request()
            logging.debug("[aileen_agent_server] :: serving request {}".format(self._server.handle_request()))


    def run_in_background(self):
        self._quit = False
        logging.info("[aileen_world_server] :: Starting aileen world server")
        self._thread = Thread(target = self.run, args=())
        self._thread.start()

    def stop(self):
        self._quit = True
