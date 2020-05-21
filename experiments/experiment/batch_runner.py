import subprocess
import time
from log_config import logging
import settings

#WEBOTS_CMD = 'nohup ./webots_headless.sh > experiments/logs/webots.out &'
WEBOTS_CMD = './webots_headless.sh'
WORLD_CMD = 'python world'
AGENT_CMD = 'python agent'

if __name__ == "__main__":
    for run in range(settings.BATCH_SIZE):
        #Redefine Runner CMD
        RUNNER_CMD = 'python experiments/experiment/runner.py --type {} --file experiments/results/{}-run-{}.csv'.format(settings.BATCH_TYPE, settings.BATCH_TYPE, run)
        logging.info('[batch_runner] :: Starting run {} of {} of type {}'.format(run+1, settings.BATCH_SIZE, settings.BATCH_TYPE))
        #Run Webots
        logging.info('[batch_runner] :: Starting Webots')
        subprocess.Popen(WEBOTS_CMD, stdout = open('experiments/logs/webots.out','w'), stderr = subprocess.STDOUT, shell = True)
        time.sleep(5)
        logging.info('[batch_runner] :: Webots Started')
        #Run World
        logging.info('[batch_runner] :: Starting World')
        subprocess.Popen(WORLD_CMD, stdout = open('experiments/logs/world.out', 'w'), stderr = subprocess.STDOUT, shell = True)
        time.sleep(20)
        logging.info('[batch_runner] :: World Started')
        #Run Agent
        logging.info('[batch_runner] :: Starting Agent')
        subprocess.Popen(AGENT_CMD, stdout = open('experiments/logs/agent.out', 'w'), stderr = subprocess.STDOUT, shell = True)
        time.sleep(15)
        logging.info('[batch_runner] :: Agent Started')
        #Run Runner
        logging.info('[batch_runner] :: Starting Runner')
        subprocess.call(RUNNER_CMD, stdout = open('experiments/logs/runner.out', 'w'), stderr = subprocess.STDOUT, shell = True)
        logging.info('[batch_runner] :: Runner returned')
        time.sleep(5)


        #Kill subprocesses
        logging.info('[batch_runner] :: Killing Subprocesses')
        subprocess.Popen("experiments/experiment/kill_mlisp.sh", stdout = open('/dev/null'), shell = True)
        subprocess.Popen("kill $(ps -u $USERNAME | grep python | awk '{print $1}')", shell = True)
        subprocess.Popen("kill $(ps -u $USERNAME | grep webots-bin | awk '{print $1}')", shell = True)
        logging.info('[batch_runner] :: Subprocesses murdered')
