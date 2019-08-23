# aileen-world
A simple xmlrpc python server to receive requests from the agent client and update the world accordingly. 
## Pre-requisites
* Anaconda
* Python 2
* Webots 



## Steps
* Download [Webots](https://www.cyberbotics.com/) and perform the standard install. Typically on Ubuntu, it will install it at `/usr/local/webots`.
* Clone this repository
  `git clone git@gitlab-external.parc.com:aileen/aileen-world.git`
* Configure a Python2 conda environment
   `conda create --name aileen_env python=2.7`
* Install relevant python packages
    `conda install coloredlogs`
    
## Usage
* Activate the conda environment
    `conda activate aileen_env`
* Configure the following environment variables
    * `PYTHONPATH` to include `/usr/local/webots/lib/python2.7` if you have the standard install
    * `LD_LIBRARY_PATH` to include `/usr/local/webots/lib`
* Start webots on a terminal or from the GUI shell 
    `webots`
* Click on `File > Open World` and navigate to `aileen_world/worlds/aileen_world.wbt`. This should start the example world in webots. You can pause and play the world using control buttons in the webots application.
* On a separate terminal window, run aileen_world. Note, that the server will not run properly until the webots simulation is actually 'playing'
    `python world.py`


## Editing
   (I am using PyCharm for editing, so the instructions are specific to that IDE. Please adapt for other IDEs)
   * Open the project in PyCharm via `File > Open`
   * In `File > Settings`, point the project interpreter to your conda environment
   * Configure the following environment variable in `Run configurations`
        * `PYTHONPATH` to include `/usr/local/webots/lib/python2.7` if you have the standard install
        * `LD_LIBRARY_PATH` to include `/usr/local/webots/lib`