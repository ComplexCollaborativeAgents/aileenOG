# aileen-agent
Grounded language learning agent based on Soar cognitive architecture

## Install and Build
### Prerequisites
* Anaconda
* Python 2 
* Soar cognitive architecture
* [aileen_world](https://gitlab-external.parc.com/aileen/aileen-world)

### Steps
* Download Soar9.6 from [here](https://soar.eecs.umich.edu/Downloads) and extract the files to a preferred location on the local file system.
* Clone this repository  
   `git clone git@gitlab-external.parc.com:aileen/aileen-world.git`
* Edit the `Soar`  `path` element in `config.json` to point to your `/local/soar/installation/bin/linux64`
* Ignore changes to config.json by the command `git update-index --assume-unchanged config.json`
      
* Configure a Python2 conda environment

       `conda create --name aileen python=2.7`

       `conda install coloredlogs pyyaml yaml`

       `conda install -c marufr python-graph`
    
   
## Usage
* Run [aileen_world](https://gitlab-external.parc.com/aileen/aileen-world)
* Activate the conda environment `conda activate aileen`
* Run aileen `python aileen.py`
  * Assuming webots and aileen-world are running, then should see debug messages in both aileen-world and aileen-agent about the objects detected.
 
This should startup the SoarJavaDebugger window that lets us inspect the state of Soar kernel.


## Structure and Editing
* aileen-agent is built in three parts: 
    1. a python client to aileen-world `aileen-agent/aileen.py`
    2. a python interface to Soar kernel `aileen-agent/soar_interface` that uses python SWIG bindings to the Soar kernel
    2. a set of rules that guide reasoning in Soar in `aileen-agent/agent`
    
  Python client and the interface are easily edited in PyCharm. Rules can be edited in Eclipse using [Soar IDE](https://github.com/soartech/soaride).
  
## Misc
* My setup is throwing a lot of warnings. They are coming from SoarJavaDebugger that seems to be built for GTK-2. Adding the following environment variable should be useful in cutting down the warnings.
`export SWT_GTK3=0`

* If you want PyCharm terminal to show color coded logs, you can check the box for `emulate terminal output` in `Run configurations` dialog.