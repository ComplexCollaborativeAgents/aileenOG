import subprocess
import time
from log_config import logging
import settings
import os, sys

import argparse

#WEBOTS_CMD = 'nohup ./webots_headless.sh > experiments/logs/webots.out &'
WEBOTS_CMD = './webots_headless.sh'
WORLD_CMD = 'python world'
AGENT_CMD = 'python agent'

def parse():
    parser = argparse.ArgumentParser(description='run a named experiment set for empirics')
    parser.add_argument('--name', help='run a named experiment')
    return parser.parse_args()


if __name__ == "__main__":

    arguments = parse()
    if arguments.name:
        experiment_name = arguments.name

    if not os.path.exists("experiments/results/{}".format(experiment_name)):
        os.mkdir("experiments/results/{}".format(experiment_name))
        os.mkdir("experiments/results/{}/system_logs".format(experiment_name))
        os.mkdir("experiments/results/{}/concept_logs".format(experiment_name))
    else:
        logging.error("[batch_runner] :: directory already exists")
        sys.exit()

    for run in range(settings.BATCH_SIZE):
        #Redefine Runner CMD
        RUNNER_CMD = 'python experiments/experiment/runner.py --type {} --file experiments/results/{}/{}-run-{}.csv ' \
                     '--concept {} --distractors {} --episodes {}'.format(experiment_name, settings.BATCH_TYPE,
                                                                          settings.BATCH_TYPE, run,
                                                                          settings.BATCH_CONCEPT,
                                                                          settings.BATCH_DISTRACTORS,
                                                                          settings.BATCH_EPISODE)
        logging.info('[batch_runner] :: Starting run {} of {} of type {}'.format(run+1, settings.BATCH_SIZE, settings.BATCH_TYPE))
        #Run Webots
        logging.info('[batch_runner] :: Starting Webots')
        subprocess.Popen(WEBOTS_CMD, stdout = open("experiments/results/{}/system_logs/webots-run-{}.out".format(experiment_name, run),'w'), stderr = subprocess.STDOUT, shell = True)
        time.sleep(5)
        logging.info('[batch_runner] :: Webots Started')
        #Run World
        logging.info('[batch_runner] :: Starting World')
        subprocess.Popen(WORLD_CMD, stdout = open("experiments/results/{}/system_logs/world-run-{}.out".format(experiment_name, run), 'w'), stderr = subprocess.STDOUT, shell = True)
        time.sleep(20)
        logging.info('[batch_runner] :: World Started')
        #Run Agent
        logging.info('[batch_runner] :: Starting Agent')
        subprocess.Popen(AGENT_CMD, stdout = open("experiments/results/{}/system_logs/agent-run-{}.out".format(experiment_name, run), 'w'), stderr = subprocess.STDOUT, shell = True)
        time.sleep(25)
        logging.info('[batch_runner] :: Agent Started')
        #Run Runner
        logging.info('[batch_runner] :: Starting Runner')
        subprocess.call(RUNNER_CMD, stdout = open("experiments/results/{}/system_logs/runner-run-{}.out".format(experiment_name, run), 'w'), stderr = subprocess.STDOUT, shell = True)
        logging.info('[batch_runner] :: Runner returned')
        time.sleep(5)


        #Kill subprocesses
        logging.info('[batch_runner] :: Killing Subprocesses')
        subprocess.Popen("experiments/experiment/kill_mlisp.sh", stdout = open('/dev/null'), shell = True)
        SCP_COMMAND = "scp dubs:aileen-agent/agent/concept_learner/concept.log experiments/results/{}/concept_logs/concept-run-{}.log".format(experiment_name, run)
        logging.info("[batch_runner] :: copying log file to {}".format(SCP_COMMAND))
        subprocess.call(SCP_COMMAND)
        subprocess.Popen("kill $(ps -u $USERNAME | grep python | awk '{print $1}')", shell = True)
        subprocess.Popen("kill $(ps -u $USERNAME | grep webots-bin | awk '{print $1}')", shell = True)
        logging.info('[batch_runner] :: Subprocesses murdered')
