import logging
import xmlrpclib


class OutputReader(object):
    def __init__(self, soar_agent, world_server):
        self._soar_agent = soar_agent
        self._world = world_server

    def read_output(self):
        number_of_commands = self._soar_agent._agent.GetNumberCommands()
        for i in range(0, number_of_commands):
            commandID = self._soar_agent._agent.GetCommand(i)
            commandName = commandID.GetAttribute()
            if commandName == 'action':
                self.process_action_description(commandID)
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
