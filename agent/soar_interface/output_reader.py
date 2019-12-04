from agent.log_config import logging
import xmlrpclib
from agent.language.aileen_grammar import AileenGrammar
from agent.concept_learner.concept_learner import ConceptLearner

class OutputReader(object):
    def __init__(self, soar_agent, world_server):
        self._soar_agent = soar_agent
        self._grammar = AileenGrammar()
        self._grammar.use_default_rules()
        self._response = None
        self._concept_learner = ConceptLearner()


        self._world_server = world_server

    def read_output(self):
        number_of_commands = self._soar_agent._agent.GetNumberCommands()
        for i in range(0, number_of_commands):
            commandID = self._soar_agent._agent.GetCommand(i)
            commandName = commandID.GetAttribute()

            if commandName == 'action':
                self.process_action_description(commandID)

            if commandName == 'language':
                self.process_language_command(commandID)

            if commandName == 'interaction':
                self.process_repsonse(commandID)

            if commandName == 'concept-memory':
                self.process_concept_learner_request(commandID)



    def process_action_description(self, commandID):
        action_dict = {}
        for i in range(0, commandID.GetNumberChildren()):
            child = commandID.GetChild(i)
            if child.GetAttribute() == 'name':
                action_dict['name'] = child.GetValueAsString()
            if child.GetAttribute() == 'id':
                action_dict['id'] = child.GetValueAsString()
            if child.GetAttribute() == 'location':
                action_dict['location'] = child.GetValueAsString()

        logging.info("[output_reader] :: soar agent output {}".format(action_dict))

        try:
            is_action_applied = self._world.apply_action(action_dict)
        except xmlrpclib.ProtocolError as err:
            logging.error("[output_reader] :: protocol error {}".format(err.errmsg))
            return
        except xmlrpclib.Fault as fault:
            logging.error("[output_reader] :: fault code {}; fault string{}".format(fault.faultCode, fault.faultString))
            return

        logging.info("[output_reader] :: received apply action status {}".format(is_action_applied))
        commandID.AddStatusComplete()

    def process_language_command(self, commandID):
        for i in range(0, commandID.GetNumberChildren()):
            child = commandID.GetChild(i)
            if child.GetAttribute() == 'parse-content':
                content = child.GetValueAsString().rstrip()
                logging.info("[output-reader] :: received parse-content command for {}".format(content))
                parsed_content = self._grammar.parse(content)
                logging.debug("[output-reader] :: parsed content to {}".format(parsed_content))
                self._soar_agent._input_writer.set_language({'parses': parsed_content})
        commandID.AddStatusComplete()

    def process_repsonse(self, commandID):
        for i in range(0, commandID.GetNumberChildren()):
            child = commandID.GetChild(i)
            if child.GetAttribute() == 'response':
                self._response = {'status': child.GetValueAsString()}
                logging.debug("[output_reader] :: repsonding with {}".format(self._response))
        commandID.AddStatusComplete()

    def process_concept_learner_request(self, commandID):
        logging.debug("[output_reader] :: processing concept_learner commands {}".format(commandID.GetNumberChildren()))
        for i in range(0, commandID.GetNumberChildren()):
            cl_command = commandID.GetChild(i).ConvertToIdentifier()
            if cl_command:
                if cl_command.GetAttribute() == "learn":
                    self.process_new_concept_symbol(cl_command)
                if cl_command.GetAttribute() == "store-instance":
                    self.process_store_instance(cl_command)
                if cl_command.GetAttribute() == "query":
                    self.process_query(cl_command)
        commandID.AddStatusComplete()

    def process_query(self, cl_command):
        logging.debug("[output-reader] :: querying concept learner")
        request = {}
        for j in range(0, cl_command.GetNumberChildren()):
            child = cl_command.GetChild(j)
            if child.GetAttribute() == "gpool":
                request['gpool'] = child.GetValueAsString()
            if child.GetAttribute() == "context":
                request['context'] = "episode{}".format(child.GetValueAsString())
            if child.GetAttribute() == "concept":
                request['pattern'] = ["isa", "?Obj", "{}".format(child.GetValueAsString())]
            if child.GetAttribute() == "scene":
                scene_id = child.ConvertToIdentifier()
                facts = []
                for k in range(0, scene_id.GetNumberChildren()):
                    object_id = scene_id.GetChild(k).ConvertToIdentifier()
                    object_dictionary = self.get_object_description_dictionary(object_id)
                    object_name = object_dictionary['id_string']
                    for item in object_dictionary:
                        if item != 'id_string':
                            fact = ['isa', object_name, object_dictionary[item]]
                            facts.append(fact)
                request['facts'] = facts
        logging.debug("[output-reader] :: processing query request {}".format(request))
        response = self._concept_learner.query_scene(request)
        logging.debug("[output-reader] :: response from concept learner {}".format(response))
        self._soar_agent._input_writer.set_concept_memory_status({
            'status': 'success',
            'matches': response['matches']
        })


    def process_store_instance(self, cl_command):
        request = {}
        for j in range(0, cl_command.GetNumberChildren()):
            child = cl_command.GetChild(j)
            if child.GetAttribute() == "gpool":
                request['gpool'] = child.GetValueAsString()
            if child.GetAttribute() == "context":
                request['context'] = "episode{}".format(child.GetValueAsString())
            if child.GetAttribute() == "object":
                child_id = child.ConvertToIdentifier()
                facts = []
                object_dictionary = self.get_object_description_dictionary(child_id)
                object_name = object_dictionary['id_string']
                for item in object_dictionary:
                    if item != 'id_string':
                        fact = ['isa', object_name, object_dictionary[item]]
                        facts.append(fact)
                request['facts'] = facts
        logging.debug("[output-reader] :: processing store-instance request {}".format(request))
        response = self._concept_learner.store_instance(request)
        logging.debug("[output-reader] :: received response {}".format(response))

    def process_new_concept_symbol(self, cl_command):
        for j in range(0, cl_command.GetNumberChildren()):
            child = cl_command.GetChild(j)
            if child.GetAttribute() == "new-concept-symbol":
                concept_symbol = child.GetValueAsString()
                logging.debug("[output-reader] :: processing new reasoning symbol {}".format(concept_symbol))
                response = self._concept_learner.create_new_concept(concept_symbol)
                logging.debug("[output-reader] :: created new concept with gpool {}".format(response['gpool']))
                self._soar_agent._input_writer.set_concept_memory_status({'status': 'success',
                                                                          'concept-symbol': concept_symbol,
                                                                          'gpool': response['gpool']})

    def get_object_description_dictionary(self, object_id):
        object_dictionary = {}
        for i in range(0, object_id.GetNumberChildren()):
            attr_val = object_id.GetChild(i)
            object_dictionary[attr_val.GetAttribute()] = attr_val.GetValueAsString()
        return object_dictionary
