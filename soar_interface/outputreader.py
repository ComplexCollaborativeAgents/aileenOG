import logging
import xmlrpclib


class OutputReader(object):
    def __init__(self, soar_agent, config, world_server):
        self._soar_agent = soar_agent
        self._config = config
        self._world = world_server

    def read_output(self):
        number_of_commands = self._soar_agent._agent.GetNumberCommands()
        output_dict = {}
        for i in range(0, number_of_commands):
            commandID = self._soar_agent._agent.GetCommand(i)
            commandName = commandID.GetAttribute()
            if commandName == 'action':
                output_dict['action'] = self.process_action_description(commandID)

            commandID.AddStatusComplete()
        logging.debug("[output_reader] :: soar agent output {}".format(output_dict))

        try:
            is_action_applied = self._world.apply_action(output_dict)
        except xmlrpclib.ProtocolError as err:
            logging.error("[input_writer] :: protocol error {}".format(err.errmsg))
            return
        except xmlrpclib.Fault as fault:
            logging.error("[input_writer] :: fault code {}; fault string{}".format(fault.faultCode, fault.faultString))
            return

        logging.debug("[output_reader] :: received apply action status {}".format(is_action_applied))

    def process_action_description(self, commandID):
        action_dict = {}
        for i in range(0, commandID.GetNumberChildren()):
            child = commandID.GetChild(i)
            if child.GetAttribute() == 'name':
                action_dict['name'] = child.GetValueAsString()
            if child.GetAttribute() == 'id':
                action_dict['id'] = child.GetValueAsString()

        if action_dict['name'] is None or action_dict['id'] is None:
            logging.error("[output_reader] :: received malformed action output {}".format(action_dict))
            return
        return action_dict
