from agent.log_config import logging
import xmlrpclib
from agent.language.aileen_grammar import AileenGrammar
from agent.language.language_learner import LanguageLearner
from agent.concept_learner.concept_learner import ConceptLearner
from agent.soar_interface import concept_learner_helper
from agent.soar_interface import action_helper
import settings


class OutputReader(object):
    def __init__(self, soar_agent, world_server):
        self._soar_agent = soar_agent
        if not settings.AGENT_LANGUAGE_LEARNING:
            self._grammar = AileenGrammar()
            self._grammar.use_default_rules()
        else:
            self._grammar = LanguageLearner()
        self._response = None
        self._concept_learner = ConceptLearner()
        self._context_counter = 0
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
                response = concept_learner_helper.process_concept_learner_request(commandID, self._concept_learner)
                self._soar_agent._input_writer.set_concept_memory_status(response)
                commandID.AddStatusComplete()


    def process_action_description(self, commandID):
        action_dict = {}
        for i in range(0, commandID.GetNumberChildren()):
            child = commandID.GetChild(i)
            if child.GetAttribute() == 'name':
                if child.GetValueAsString() == 'pick-up':
                    action_dict = action_helper.process_pick_command(commandID)
                else:
                    if child.GetValueAsString() == 'place':
                        action_dict = action_helper.process_place_command(commandID)
                    else:
                        if child.GetValueAsString() == 'point':
                            action_dict = action_helper.process_point_command(commandID)
                        else:
                            logging.error("[output_reader] :: soar agent provided an unknown command {}".format(child.GetValueAsString()))

        logging.info("[output_reader] :: soar agent output {}".format(action_dict))

        try:
            is_action_applied = self._world_server.apply_action(action_dict)
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
                content = child.GetValueAsString().strip()
                logging.info("[output-reader] :: received parse-content command for {}".format(content))
                parsed_content = self._grammar.parse(content)
                logging.debug("[output-reader] :: parsed content to {}".format(parsed_content))
                self._soar_agent._input_writer.set_language({'parses': parsed_content})
            if child.GetAttribute() == 'generate-content':
                child_id = child.ConvertToIdentifier()
                content_list = []
                for j in range(0, child_id.GetNumberChildren()):
                    subchild = child_id.GetChild(j)
                    subchild_attribute = subchild.GetAttribute()
                    if subchild_attribute == 'object':
                        subchild_id = subchild.ConvertToIdentifier()
                        words = []
                        identifier = None
                        for k in range(0, subchild_id.GetNumberChildren()):
                            item = subchild_id.GetChild(k)
                            if item.GetAttribute() == 'id':
                                identifier = item.GetValueAsString()
                            if item.GetAttribute() == 'word':
                                words.append(item.GetValueAsString())
                        if identifier and len(words) > 0:
                            object_dict = {'id': identifier, 'type': 'object', 'tokens': words}
                            content_list.append(object_dict)
                        else:
                            logging.error("[output_reader] :: malformed language request".format(subchild))
                logging.debug("[output_reader] :: requesting generation for {}".format(content_list))
                content = self._grammar.generate(content_list)
                self._soar_agent._input_writer.set_language({'sentence': content})
                logging.debug("[output_reader] :: generated sentence: {}".format(content))
        commandID.AddStatusComplete()

    def process_repsonse(self, commandID):
        for i in range(0, commandID.GetNumberChildren()):
            child = commandID.GetChild(i)
            if child.GetAttribute() == 'response':
                self._response = {'status': child.GetValueAsString()}
                logging.debug("[output_reader] :: repsonding with {}".format(self._response))
        commandID.AddStatusComplete()
