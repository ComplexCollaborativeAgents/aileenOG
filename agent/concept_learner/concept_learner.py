from agent.configuration import Configuration
import subprocess
import time
from agent.log_config import logging
import xmlrpclib
import json

class ConceptLearner(object):
    def __init__(self):
        self.start_concept_learner_server()
        self._server = xmlrpclib.ServerProxy('http://dubs:'+str(Configuration.config['ConceptLearner']['port'])+'/ConceptLearner')

    def start_concept_learner_server(self):
        cmd = 'ssh {} {}/start_concept_learner.sh {}'.format(Configuration.config['ConceptLearner']['hostname'],
                                                             Configuration.config['ConceptLearner']['learner_path'],
                                                             Configuration.config['ConceptLearner']['port'])
        out = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True)
        time.sleep(5)
        logging.info("[concept_learner] :: started concept_learner server on {}".format(Configuration.config['ConceptLearner']
                                                                                        ['hostname']))

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
