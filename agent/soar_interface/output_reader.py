from agent.log_config import logging
import xmlrpclib
from agent.language.aileen_grammar import AileenGrammar


class OutputReader(object):
    def __init__(self, soar_agent, world_server):
        self._soar_agent = soar_agent
        self._grammar = AileenGrammar()
        self._grammar.use_default_rules()
        self._response = None
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
            commandID.AddStatusComplete()

            if commandName == 'interaction':
                self.process_repsonse(commandID)
            commandID.AddStatusComplete()

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

    def process_language_command(self, commandID):
        for i in range(0, commandID.GetNumberChildren()):
            child = commandID.GetChild(i)
            if child.GetAttribute() == 'parse-content':
                content = child.GetValueAsString().rstrip()
                logging.info("[output-reader] :: received parse-content command for {}".format(content))
                parsed_content = self._grammar.parse(content)
                logging.debug("[output-reader] :: parsed content to {}".format(parsed_content))
                self._soar_agent._input_writer.set_language({'parses': parsed_content})

    def process_repsonse(self, commandID):
        for i in range(0, commandID.GetNumberChildren()):
            child = commandID.GetChild(i)
            if child.GetAttribute() == 'response':
                self._response = {'status': child.GetValueAsString()}
                logging.debug("[output_reader] :: repsonding with {}".format(self._response))
