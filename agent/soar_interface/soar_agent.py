import os
import sys
import logging
from threading import Thread
import time
import random
import output_reader
import input_writer
from agent.configuration import Configuration

try:
    import Python_sml_ClientInterface as sml
except ValueError, e:
    logging.fatal("[soar_agent] :: Cannot find local soar installation")
    sys.exit()


class soar_agent(object):
    def __init__(self, world_server, headless=False):
        self.setup_soar_agent(world_server)
        self.init_state_maintenance_data_structures()
        self._headless = headless

        if Configuration.config['RunParams']['svs'] == "true":
            self.execute_command("svs --enable")

    def init_state_maintenance_data_structures(self):
        self.stop_requested = False
        self._agent_thread = None
        self._is_running = False

    def setup_soar_agent(self, world_server):
        self._kernel = self.create_kernel()
        self._agent = self.create_agent(str(Configuration.config['SoarAgent']['name']))
        self._agentFilepath = str(Configuration.config['SoarAgent']['file'])
        self.load_agent_rules(self._agentFilepath)
        self._input_link = self._agent.GetInputLink()
        self._output_link = self._agent.GetOutputLink()
        self._input_writer = input_writer.InputWriter(self, world_server)
        self._output_reader = output_reader.OutputReader(self, world_server)

    def create_kernel(self):
        soar_kernel_port = random.randint(40000, 60000)
        kernel = sml.Kernel.CreateKernelInNewThread(soar_kernel_port)
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
        self._agent.LoadProductions(os.path.realpath(agent_file))

    def run_soar_java_debugger(self):
        self.stop()
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
        time.sleep(Configuration.config['Soar']['sleep-time'])
        self._agent.ExecuteCommandLine(command)

    def set_time(self, week, day):
        self._input_writer.set_time = {'week': week, 'day': day}

    def start(self):
        if (self._is_running):
            return
        self._is_running = True
        self._agent_thread = Thread(target=self.execute_command, args=("run",))
        self._agent_thread.start()
        logging.info("[soar_agent] :: spun-off agent thread.")

        ## start debugger
        if Configuration.config["RunParams"]["run_mode"] == "debug" or not self._headless:
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
        self._input_writer.set_interaction(interaction_dictionary)
        while self._output_reader._response is None:
            pass
        response = self._output_reader._response
        self._output_reader._response = None
        return response

def update(mid, this_agent, agent, message):
    this_agent.stop_agent_if_requested()
    this_agent._output_reader.read_output()
    this_agent._input_writer.generate_input()
