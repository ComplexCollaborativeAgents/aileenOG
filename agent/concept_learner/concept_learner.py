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
        cmd = 'ssh {} screen {}/start_concept_learner.sh {}'.format(settings.CONCEPT_LEARNER_HOST,
                                                             settings.CONCEPT_LEARNER_PATH,
                                                             settings.CONCEPT_LEARNER_PORT)
        out = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True)
        time.sleep(5)
        logging.info("[concept_learner] :: started concept_learner server on {}".format(settings.CONCEPT_LEARNER_HOST))

    def create_new_concept(self, request):
        if request['type'] == "symbol":
            response_json = self._server.create_reasoning_symbol(xmlrpclib.Binary(json.dumps({'symbol': request['name']})))
            response = json.loads(response_json.data)
            return response
        if request['type'] == "predicate":
            response_json = self._server.create_reasoning_predicate(xmlrpclib.Binary(json.dumps({'predicate': request['name'], 'arity': 2})))
            response = json.loads(response_json.data)
            return response
        if request['type'] == "action":
            response_json = self._server.create_reasoning_action(xmlrpclib.Binary(json.dumps({'action': request['name'], 'arity':2})))
            response = json.loads(response_json.data)
            return response

    def store(self, request):
        response_json = self._server.store(xmlrpclib.Binary(json.dumps(request)))
        response = json.loads(response_json.data)
        return response

    def query(self, request):
        response_json = self._server.query(xmlrpclib.Binary(json.dumps(request)))
        response = json.loads(response_json.data)
        return response

    def project(self, request):
        response_json = self._server.project(xmlrpclib.Binary(json.dumps(request)))
        response = json.loads(response_json.data)
        return response

    def describe(self, request):
        response_json = self._server.describe(xmlrpclib.Binary(json.dumps(request)))
        response = json.loads(response_json.data)
        return response