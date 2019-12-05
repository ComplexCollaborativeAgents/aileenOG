import subprocess
import time
from agent.log_config import logging
import xmlrpclib
import json
import settings

class ConceptLearner(object):
    def __init__(self):
        self.start_concept_learner_server()
        self._server = xmlrpclib.ServerProxy('http://{}:{}/ConceptLearner'.format(settings.CONCEPT_LEARNER_HOST, settings.CONCEPT_LEARNER_PORT))

    def start_concept_learner_server(self):
        cmd = 'ssh {} {}/start_concept_learner.sh {}'.format(settings.CONCEPT_LEARNER_HOST,
                                                             settings.CONCEPT_LEARNER_PATH,
                                                             settings.CONCEPT_LEARNER_PORT)
        out = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True)
        time.sleep(5)
        logging.info("[concept_learner] :: started concept_learner server on {}".format(settings.CONCEPT_LEARNER_HOST))

    def create_new_concept(self, concept_symbol):
        response_json = self._server.create_reasoning_symbol(xmlrpclib.Binary(json.dumps({'symbol': concept_symbol})))
        response = json.loads(response_json.data)
        return response

    def store_instance(self, request):
        response_json = self._server.add_case_to_gpool(xmlrpclib.Binary(json.dumps(request)))
        response = json.loads(response_json.data)
        return response

    def query_scene(self, request):
        response_json = self._server.filter_scene_by_expression(xmlrpclib.Binary(json.dumps(request)))
        response = json.loads(response_json.data)
        return response
