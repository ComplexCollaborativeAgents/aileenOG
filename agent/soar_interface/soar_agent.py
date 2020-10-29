import logging
import os
import socket
import sys
import time
from contextlib import closing
from threading import Thread

import input_writer
import output_reader
import settings
from experiments.experiment.results_helper import ResultsHelper

try:
    import Python_sml_ClientInterface as sml
except ValueError, e:
    logging.fatal("[soar_agent] :: Cannot find local soar installation")
    sys.exit()


class SoarAgent(object):
    def __init__(self, world_server, kernel_port=None, agent_params={}):
        self.setup_soar_agent(world_server, kernel_port, agent_params)
        self.init_state_maintenance_data_structures()

        if settings.SOAR_SVS:
            self.execute_command("svs --enable")

    def init_state_maintenance_data_structures(self):
        self.stop_requested = False
        self._agent_thread = None
        self._is_running = False
        self._wmes_to_delete = []


    def setup_soar_agent(self, world_server, kernel_port, agent_params):
        self._kernel = self.create_kernel(kernel_port)
        self._agent = self.create_agent(settings.SOAR_AGENT_NAME)
        self._agentFilepath = settings.SOAR_AGENT_PATH
        self.set_agent_params(agent_params)
        self.load_agent_rules(self._agentFilepath)
        self._input_link = self._agent.GetInputLink()
        self._output_link = self._agent.GetOutputLink()
        self._input_writer = input_writer.InputWriter(self, world_server)
        self._output_reader = output_reader.OutputReader(self, world_server)


    def set_agent_params(self, agent_params):
        #logging.info("[soar-agent] :: setting agent params {}".format(agent_params))
        if 'visual-concepts-param' in agent_params:
            visual_concepts_param = agent_params['visual-concepts-param']
        else:
            visual_concepts_param = settings.AGENT_VISUAL_CONCEPTS_PARAM

        if 'spatial-concepts-param' in agent_params:
            spatial_concepts_param = agent_params['spatial-concepts-param']
        else:
            spatial_concepts_param = settings.AGENT_SPATIAL_CONCEPTS_PARAM

        if 'action-concepts-param' in agent_params:
            action_concepts_param = agent_params['action-concepts-param']
        else:
            action_concepts_param = settings.AGENT_ACTION_CONCEPTS_PARAM

        if 'preload-visual-concepts-param' in agent_params:
            preload_visual_concepts_param = agent_params['preload-visual-concepts-param']
        else:
            preload_visual_concepts_param = settings.AGENT_PRELOAD_VISUAL_CONCEPTS_PARAM

        if 'preload-spatial-concepts-param' in agent_params:
            preload_spatial_concepts_param = agent_params['preload-spatial-concepts-param']
        else:
            preload_spatial_concepts_param = settings.AGENT_PRELOAD_SPATIAL_CONCEPTS_PARAM

        if 'preload-action-concepts-param' in agent_params:
            preload_action_concepts_param = agent_params['preload-action-concepts-param']
        else:
            preload_action_concepts_param = settings.AGENT_PRELOAD_ACTION_CONCEPTS_PARAM

        recomp_param = settings.AGENT_RECOMPREHEND_AFTER_LEARN
        percept_param = settings.AGENT_PERCEPT_SYMBOLS_ATTR
        store_policy = settings.AGENT_STORE_CONCEPT_POLICY

        params = """sp {{aileen*apply*init-agent*agent_params 
                    (state <s>    ^operator.name initialize-agent)
                    -->
                    (<s>    ^_params <p>)
                    (<p>    ^visual-concepts {v_param}
                            ^spatial-concepts {s_param}
                            ^action-concepts {a_param}
                            ^interaction <inter>
                            ^policies <pol>)
                    (<p>    ^preload-visual-concepts {pv_param}
                            ^preload-spatial-concepts {ps_param}
                            ^preload-action-concepts {pa_param})
                    (<p>    ^relevant-percept-set <rps>)
                    (<rps>    ^type {p_set})
                    (<inter>    ^recomprehend {comp_param})
                    (<pol>  ^store {s_policy})
                    }}""".format(v_param=visual_concepts_param,
                                 s_param=spatial_concepts_param,
                                 a_param=action_concepts_param,
                                 pv_param=preload_visual_concepts_param,
                                 ps_param=preload_spatial_concepts_param,
                                 pa_param=preload_action_concepts_param,
                                 p_set=percept_param,
                                 comp_param=recomp_param,
                                 s_policy=store_policy)

        #logging.debug("[soar-agent] :: loading params into agent")
        self.execute_command(params)


    def create_kernel(self, kernel_port):
        if kernel_port:
            soar_kernel_port = kernel_port
        else:
            soar_kernel_port = find_free_port()
        kernel = sml.Kernel.CreateKernelInNewThread(soar_kernel_port)
        #kernel = sml.Kernel.CreateKernelInCurrentThread()
        if not kernel or kernel.HadError():
            logging.error("[soar_agent] :: Error creating kernel: " + kernel.GetLastErrorDescription())
            exit(1)
        logging.info("[soar_agent] :: created a soar kernel listening to port {}".format(soar_kernel_port))
        return kernel

    def create_agent(self, agent_name):
        agent = self._kernel.CreateAgent(agent_name)
        if not agent:
            logging.error("[soar_agent] :: Error creating agent: " + self.kernel.GetLastErrorDescription())
            exit(1)
        return agent

    def load_agent_rules(self, agent_file):
        logging.info("[soar_agent] :: Loading agent at %s" % agent_file)
        dirname = os.path.dirname(__file__)
        path = os.path.join(dirname, agent_file)
        self._agent.LoadProductions(path)

    def run_soar_java_debugger(self):
        self._agent.SpawnDebugger(self._kernel.GetListenerPort())

    def register_output_callback(self, function, caller_object=None):
        self._agent.RegisterForRunEvent(sml.smlEVENT_AFTER_OUTPUT_PHASE, function, caller_object, True)

    def run_till_output(self):
        self._agent.RunSelfTilOutput()

    def run_forever(self):
        self._agent.RunSelfForever()

    def get_input_link(self):
        return self._input_link

    def get_output_link(self):
        return self._output_link

    def commit(self):
        self._agent.Commit()

    def shutdown(self):
        self.stop_requested = True
        self._agent.KillDebugger()
        self._kernel.Shutdown()

    def quit(self):
        self.stop_requested = True
        self._agent.KillDebugger()

    def check_for_incoming_events(self):
        self._kernel.CheckForIncomingEvents()

    def execute_command(self, command):
        time.sleep(settings.SOAR_SLEEP_TIME)
        self._agent.ExecuteCommandLine(command)

    def set_time(self, week, day):
        self._input_writer.set_time = {'week': week, 'day': day}

    def start(self):
        if self._is_running:
            return
        self._is_running = True
        self._agent_thread = Thread(target=self.execute_command, args=("run",))
        self._agent_thread.daemon = True
        self._agent_thread.start()
        logging.info("[soar_agent] :: spun-off agent thread.")

        ## start debugger
        if settings.SOAR_DEBUG:
            self.run_soar_java_debugger()

    def stop(self):
        self.stop_requested = True

    def step(self):
        self.execute_command("step")

    def stop_agent_if_requested(self):
        if self.stop_requested is True:
            logging.info("[soar_agent] :: stopping agent")
            self.execute_command("stop")
            self._is_running = False
            self.stop_requested = False

    def get_number_of_commands(self):
        return self._agent.GetNumberCommands()

    def process_interaction(self, interaction_dictionary):
        logging.debug("[soar_agent] :: handling process_interaction request {}".format(interaction_dictionary))

        ResultsHelper.reset_create_concept_count()
        ResultsHelper.reset_store_instance_count()

        self._input_writer.set_interaction(interaction_dictionary)
        while self._output_reader._response is None:
            pass
        response = self._output_reader._response
        self._output_reader._response = None
        response['create_concept_count'] = ResultsHelper.create_concept_count
        response['store_instance_count'] = ResultsHelper.store_instance_count
        return response

    def delete_all_children(self, id):
        #logging.debug("[input_writer] :: deleting children of {}".format(id.GetValueAsString()))
        if id.GetNumberChildren() is not None:
            for i in range(0, id.GetNumberChildren()):
                child = id.GetChild(i)
                self._wmes_to_delete.append(child)
                # logging.debug(
                #     "[soar_agent] :: added {} {} {} child wme to destroy list".format(i, child.GetAttribute(),child.GetValueAsString()))

    def destroy_wme_on_list(self):
        #logging.debug("[soar_agent] :: wmes to be destroyed {}".format(self._wmes_to_delete))
        if len(self._wmes_to_delete) > 0:
            for wme in self._wmes_to_delete:
                if wme is not None:
                    #logging.debug("[soar_agent] :: destroying wme {}".format(wme.GetValueAsString()))
                    self._agent.DestroyWME(wme)
            self._wmes_to_delete = []


def update(mid, this_agent, agent, message):
    this_agent.stop_agent_if_requested()
    this_agent._output_reader.read_output()
    this_agent._input_writer.generate_input()
    this_agent.destroy_wme_on_list()



def find_free_port():
    """Source: https://stackoverflow.com/a/45690594."""
    with closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as s:
        s.bind(('', 0))
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        return s.getsockname()[1]
