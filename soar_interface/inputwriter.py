import time
from log_config import logging
import xmlrpclib
from svs_helper import SVSHelper
from configuration import Configuration


class InputWriter(object):
    def __init__(self, soar_agent, world_server):
        self._soar_agent = soar_agent
        self._input_link = soar_agent.get_input_link()
        self._world_server = world_server
        self.set_time = None
        self.timestamp = 0

        self._world_link = self._input_link.CreateIdWME("world")
        self._objects_link = self._world_link.CreateIdWME("objects")
        self._interaction_link = self._input_link.CreateIdWME("interaction")

        self._svs_objects = []

    def generate_input(self):
        time.sleep(Configuration.config['Soar']['sleep-time'])

        objects = self.request_server_for_objects_info()
        if objects is not None:
            self.add_objects_to_working_memory(objects)
            if Configuration.config['RunParams']['svs'] == "true":
                self.add_objects_to_svs(objects)

    ## SM: both these methods need to be rewritten to maintain the list of objects properly
    def add_objects_to_svs(self, objects):
        logging.info("[input_writer] :: writing objects to SVS")
        for w_object in objects:
            object_id = "object{}".format(w_object['id'])
            if object_id in self._svs_objects:
                svs_command = SVSHelper.get_svs_command_for_change_position(object_id,
                                                                            w_object['position'])
            else:
                svs_command = SVSHelper.get_svs_command_for_add_box(object_id,
                                                                    position=w_object['position'],
                                                                    bounding_box=w_object['bounding_box'])
                self._svs_objects.append(object_id)

            self._soar_agent._agent.SendSVSInput(svs_command)
            logging.debug("[input_writer] :: updating svs with {}".format(svs_command))

    def add_objects_to_working_memory(self, objects):
        self.delete_all_children(self._objects_link)
        for w_object in objects:
            object_id = self._objects_link.CreateIdWME("object")
            object_id.CreateIntWME('id', w_object['id']),
            position_id = object_id.CreateIdWME('position')
            position_id.CreateFloatWME('x', w_object['position'][0])
            position_id.CreateFloatWME('y', w_object['position'][1])
            position_id.CreateFloatWME('z', w_object['position'][2])
            object_id.CreateStringWME('held', w_object['held'])

    def request_server_for_objects_info(self):
        try:
            objects_dict = self._world_server.get_all()
        except xmlrpclib.ProtocolError as err:
            logging.error("[output_reader] :: protocol error {}".format(err.errmsg))
            return
        except xmlrpclib.Fault as fault:
            logging.error("[output_reader] :: fault code {}; fault string{}".format(fault.faultCode, fault.faultString))
            return

        logging.debug("[input_writer] :: received objects from server {}".format(objects_dict))
        objects = objects_dict['objects']
        return objects

    def delete_all_children(self, id):
        index = 0
        if id.GetNumberChildren is not None:
            for i in range(0, id.GetNumberChildren()):
                child = id.GetChild(
                    index)  # remove the 0th child several times, Soar kernel readjusts the list after an item is deletd
                if child is not None:
                    child.DestroyWME()

