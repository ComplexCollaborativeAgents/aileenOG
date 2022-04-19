import subprocess
import time
from log_config import logging
import settings
import os, sys
import argparse
import random

#WEBOTS_CMD = 'nohup ./webots_headless.sh > experiments/logs/webots.out &'
WEBOTS_CMD = './webots_headless.sh'
WORLD_CMD = 'python world'


def parse():
    parser = argparse.ArgumentParser(description='run a named experiment set for empirics')
    parser.add_argument('--name', help='run a named experiment')
    parser.add_argument('--batch_size', help='number of epochs')
    parser.add_argument('--type', help='run this type of experiment')
    parser.add_argument('--concept', help='only generate examples of this concept')
    parser.add_argument('--episodes', help='number of tranining instances per concept')
    parser.add_argument('--exam_length', help='number of samples in exams')
    parser.add_argument('--distractors', help='number of distractors in exams')


    return parser.parse_args()


if __name__ == "__main__":

    arguments = parse()
    if arguments.name:
        experiment_name = arguments.name
    if arguments.batch_size:
        EXP_BATCH_SIZE = int(arguments.batch_size)

    if arguments.concept:
        EXP_CONCEPT = arguments.concept
    if arguments.distractors:
        EXP_DISTRACTORS = arguments.distractors
    if arguments.episodes:
        EXP_EPISODES = arguments.episodes
    if arguments.exam_length:
        EXP_EXAM_LENGTH = arguments.exam_length
    if arguments.type:
        EXP_TYPE = arguments.type

    if not os.path.exists("experiments/results/{}".format(experiment_name)):
        os.mkdir("experiments/results/{}".format(experiment_name))
        os.mkdir("experiments/results/{}/system_logs".format(experiment_name))
        os.mkdir("experiments/results/{}/concept_logs".format(experiment_name))
    else:
        logging.error("[batch_runner] :: directory already exists")
        sys.exit()


    agent_port = 40002

    for run in range(EXP_BATCH_SIZE):
        agent_port += 1
        AGENT_CMD = "python agent --port {}".format(agent_port)
        #Redefine Runner CMD
        RUNNER_CMD = 'python experiments/experiment/runner.py --file experiments/results/{}/{}-run-{}.csv --type {} ' \
                     '--concept {} --distractors {} --episodes {} --exam_length {} --agent_port {}'.format(experiment_name,
                                                                          EXP_BATCH_SIZE, run,
                                                                          EXP_TYPE,
                                                                          EXP_CONCEPT,
                                                                          EXP_DISTRACTORS,
                                                                          EXP_EPISODES,
                                                                          EXP_EXAM_LENGTH,
                                                                          agent_port)
        logging.info('[batch_runner] :: Starting run {} of {} of type {}'.format(run+1, EXP_BATCH_SIZE, EXP_TYPE))
        #Run Webots
        logging.info('[batch_runner] :: Starting Webots')
        webots_proc = subprocess.Popen(WEBOTS_CMD, stdout = open("experiments/results/{}/system_logs/webots-run-{}.out".format(experiment_name, run),'w'), stderr = subprocess.STDOUT, shell = True)
        time.sleep(5)
        logging.info('[batch_runner] :: Webots Started')
        #Run World
        logging.info('[batch_runner] :: Starting World')
        world_proc = subprocess.Popen(WORLD_CMD, stdout = open("experiments/results/{}/system_logs/world-run-{}.out".format(experiment_name, run), 'w'), stderr = subprocess.STDOUT, shell = True)
        time.sleep(20)
        logging.info('[batch_runner] :: World Started')
        #Run Agent
        logging.info('[batch_runner] :: Starting Agent')
        agent_proc = subprocess.Popen(AGENT_CMD, stdout = open("experiments/results/{}/system_logs/agent-run-{}.out".format(experiment_name, run), 'w'), stderr = subprocess.STDOUT, shell = True)
        time.sleep(25)
        logging.info('[batch_runner] :: Agent Started')
        #Run Runner
        logging.info('[batch_runner] :: Starting Runner')
        #print cmd
        logging.info("[batch_runner] :: Running experiment {}".format(RUNNER_CMD))
        subprocess.call(RUNNER_CMD, stdout = open("experiments/results/{}/system_logs/runner-run-{}.out".format(experiment_name, run), 'w'), stderr = subprocess.STDOUT, shell = True)
        logging.info('[batch_runner] :: Runner returned')
        time.sleep(5)


        #Kill subprocesses
        logging.info('[batch_runner] :: Killing Subprocesses')
        subprocess.Popen("experiments/experiment/kill_mlisp.sh", stdout = open('/dev/null'), shell = True)
        SCP_COMMAND = "scp dubs:aileen-agent/agent/concept_learner/concept.log experiments/results/{}/concept_logs/concept-run-{}.log".format(experiment_name, run)
        logging.info("[batch_runner] :: copying log file to {}".format(SCP_COMMAND))
        subprocess.Popen(SCP_COMMAND,shell = True)

        webots_proc.kill()
        world_proc.kill()
        agent_proc.kill()

        #subprocess.Popen("kill $(ps -u $USERNAME | grep python | awk '{print $1}')", shell = True)
        #subprocess.Popen("kill $(ps -u $USERNAME | grep webots | awk '{print $1}')", shell = True)
        logging.info('[batch_runner] :: Subprocesses murdered')
        time.sleep(15)
