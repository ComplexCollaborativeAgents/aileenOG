import time
from log_config import logging
import xmlrpclib

from qsrlib.qsrlib import QSRlib, QSRlib_Request_Message
from qsrlib_io.world_trace import Object_State, World_Trace

class input_writer(object):
    def __init__(self, soar_agent, config, world_server):
        self._soar_agent = soar_agent
        self._config = config
        self._input_link = soar_agent.get_input_link()
        self._world_server = world_server
        self.set_time = None
        self.timestamp = 0

        self._world_link = self._input_link.CreateIdWME("world")
        self._objects_link = self._world_link.CreateIdWME("objects")
        self._interaction_link = self._input_link.CreateIdWME("interaction")

    def generate_input(self):
        time.sleep(self._config['Soar']['sleep-time'])
        try:
            objects_dict = self._world_server.get_all()
        except xmlrpclib.ProtocolError as err:
            logging.error("[output_reader] :: protocol error {}".format(err.errmsg))
            return
        except xmlrpclib.Fault as fault:
            logging.error("[output_reader] :: fault code {}; fault string{}".format(fault.faultCode, fault.faultString))
            return

        logging.info("[input_writer] :: received objects from server {}".format(objects_dict))
        objects = objects_dict['objects']

        self.delete_all_children(self._objects_link)

        qsrs = create_qsr(objects)

        for w_object in objects:
            object_id = self._objects_link.CreateIdWME("object")
            object_id.CreateIntWME('id', w_object['id']),
            position_id = object_id.CreateIdWME('position')
            position_id.CreateFloatWME('x', w_object['position'][0])
            position_id.CreateFloatWME('y', w_object['position'][1])
            position_id.CreateFloatWME('z', w_object['position'][2])
            object_id.CreateStringWME('held', w_object['held'])
            pass

    def delete_all_children(self, id):
        index = 0
        if id.GetNumberChildren is not None:
            for i in range(0, id.GetNumberChildren()):
                child = id.GetChild(index)  # remove the 0th child several times, Soar kernel readjusts the list after an item is deletd
                if child is not None:
                    child.DestroyWME()

    def create_qsrs(objects):
        # Ignore y position as everything is on the table (use z instead)
        # Need to figure out how to send back the bounding boxes. Currently assuming .1
        qsrlib = QSRlib() # We don't need a new object each time
        world = World_Trace()
        for obj in objects:
            world.add_object_state_series([Object_State(name=str(obj['id']),timestamp=0,
                                                        x=obj['position'][0],
                                                        y=obj['position'][2],
                                                        xsize=0.1,ysize=0.1)])
        qsrlib_request_message = QSRlib_Request_Message(["rcc8","cardir"], world)
        qsrlib_response_message = qsrlib.request_qsrs(req_msg=qsrlib_request_message)
        res_str = ""
        for t in qsrlib_response_message.qsrs.get_sorted_timestamps():
            for k, v in zip(qsrlib_response_message.qsrs.trace[t].qsrs.keys(),
                            qsrlib_response_message.qsrs.trace[t].qsrs.values()):
                res_str += str(k) + ":" + str(v.qsr) + "; "
        logging.info("[input_writer] :: qsrs computed {}".format(res_str))
        return qsrlib_response_message
