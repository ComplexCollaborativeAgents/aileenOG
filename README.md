# Aileen
![](https://img.shields.io/badge/python-2.7-blue)

## Contributing

### Development environment setup
1. Install [Qhull](http://www.qhull.org/), required for `svs_viewer`:
   ```bash
   sudo apt install qhull-bin
   ```
2. Download [Soar 9.6.0](https://soar.eecs.umich.edu/) and extract the files to 
   `/usr/local/SoarSuite_9.6.0-Multiplatform_64bit` and run `SoarJavaDebugger.sh` once:
   ```bash
   wget http://soar.eecs.umich.edu/downloads/SoarSuite/SoarSuite_9.6.0-Multiplatform_64bit.zip
   sudo unzip -d /usr/local SoarSuite_9.6.0-Multiplatform_64bit.zip
   /usr/local/SoarSuite_9.6.0-Multiplatform_64bit/SoarJavaDebugger.sh
   ```
3. Download and install [Webots](https://cyberbotics.com/):
   ```bash
   wget https://github.com/cyberbotics/webots/releases/download/R2019b-rev1/webots_2019b-rev1_amd64.deb
   sudo dpkg -i webots_2019b-rev1_amd64.deb
   ```
4. Clone this repository and its submodules:
   ```bash
   cd aileen-agent
   git clone --recurse-submodules git@gitlab-external.parc.com:aileen/aileen-agent.git
   ``` 
5. Run the `bootstrap.sh` script to set up the Conda environment and build the Python dependencies, such as 
   [Pynini](http://www.openfst.org/twiki/bin/view/GRM/Pynini) and [Darknet](https://pjreddie.com/darknet/):
   ```bash
   ./bootstrap.sh
   ```
   > Note: This script creates a Conda environment, called `aileen`. To activate the environment:
   > ```bash
   > conda activate aileen
   > ```
   > Whenever a new dependency is added to `environment.yml`, the environment can be updated using:
   > ```bash
   > conda env update -f environment.yml
   > ```

### Run
1. Open Webots and load the world: "File", "Open World...", and select `world/data/aileen_world.wbt`.
> Note: Webots can be run headless using Xvfb
   > First Time Xvfb setup:
   > ```bash
   > sudo apt install xvfb
   >```
   > To run Webots headless, run the following in a new shell:
   > ```bash
   > cd /path/to/aileen-agent
   > ./webots_headless.sh
   > ```
2. Run the world server:
   ```bash
   export WEBOTS_HOME="/usr/local/webots"
   export PYTHONPATH="$WEBOTS_HOME/lib/python27:$PYTHONPATH"
   export LD_LIBRARY_PATH="$WEBOTS_HOME/lib"
   conda activate aileen
   (aileen) python world
   ```
3. Run the agent in another shell instance:
   ```bash
   conda activate aileen
   (aileen) python agent
   ```
   > Note: To suppress the GTK3 warnings: `export SWT_GTK3=0`.
4. Run the instructor in another shell instance:
   ```bash
   conda activate aileen
   (aileen) python instructor
   ```

### Run test suite
```bash
conda activate aileen
(aileen) pytest --pyargs tests
```

### PyCharm
1. Go to "File", "Settings...", "Tools", "Python Integrated Tools".
2. Under "Testing", set the default test runner to "pytest".


## Interactive Instructor
The instructor can be controlled in two ways: REPL and JSON file.

### REPL
When the REPL is started, we can input individual lessons that together form a curriculum.
```python
>>> { 'lesson': 'visual', 'color': 'red', 'shape': 'cube' }
>>> { 'lesson': 'visual', 'color': 'red', 'shape': 'cube', 'position': [0, 0.5, 0] }
>>> { 'lesson': 'visual', 'color': 'red', 'shape': 'cube', 'distractors': 3 }
>>> { 'lesson': 'spatial', 'language': [ { 'color': 'red', 'shape': 'cube', 'position': [0, 0.5, 0] },
                                         'left-of',
                                         { 'color': 'red', 'shape': 'cone', 'position': [0, 1, 0] } ],
      'distractors': 2 }
>>> { 'lesson': 'action', 'language': [ 'move',
                                        { 'color': 'red', 'shape': 'cube', 'position': [0, 0.5, 0] },
                                        'left-of',
                                        { 'color': 'red', 'shape': 'cone', 'position': [0, 1, 0] } ] }
```

### JSON File
A curriculum can also be put together as a JSON file and then be used as input for the instructor.
```json
[
{ "lesson": "visual", "color": "red", "shape": "box", "signal": "verify" },
{ "lesson": "visual", "color": "red", "shape": "box", "position": [0, 0.5, 0] },
{ "lesson": "visual", "color": "red", "shape": "box", "distractors": 3 },
{ "lesson": "spatial", "language": [ { "color": "red", "shape": "box", "position": [0, 0.5, 0] },
                                     "left-of",
                                     { "color": "red", "shape": "cone", "position": [0, 1, 0] } ],
  "distractors": 2 },
{ "lesson": "action", "language": [ "move",
                                    { "color": "red", "shape": "box", "position": [0, 0.5, 0] },
                                    "left-of",
                                    { "color": "red", "shape": "cone", "position": [0, 1, 0] } ] }
]
```



## aileen-agent
Grounded language learning agent based on Soar cognitive architecture

### Install and Build
#### Prerequisites
* Anaconda
* Python 2 
* Soar cognitive architecture
* [aileen_world](https://gitlab-external.parc.com/aileen/aileen-world)
* qhull (needed for running svs_viewer; ubuntu `sudo apt install qhull-bin`)

#### Steps
* Download Soar9.6 from [here](https://soar.eecs.umich.edu/Downloads) and extract the files to a preferred location on the local file system.
* Clone this repository  
   `git clone git@gitlab-external.parc.com:aileen/aileen-world.git`
* Pull in code from `strands_qsr_lib` submodule
   `git submodule update --init`
* Edit the `Soar`  `path` element in `config.json` to point to your `/local/soar/installation/bin/linux64`
* Ignore changes to config.json by the command `git update-index --assume-unchanged config.json`
   
* Configure a Python2 conda environment

       `conda create --name aileen python=2.7`

       `conda install coloredlogs numpy pyyaml yaml shapely pytest`

       `conda install -c marufr python-igraph`
    
* On the Mac using Python2, you will also need to:

	cd /usr/local/opt/python
	ln -s /System/Library/Frameworks
        sudo -H pip2 install PyYAML
	brew install cairo
	brew install py2cairo
	brew install igraph
	sudo pip install --user python-igraph

* To install Pynini for Python 2, download pynini-1.9.3.tar.gz from http://www.openfst.org/twiki/bin/view/GRM/PyniniDownload and follow the instructions in README.rst.
Be sure to get the specified versions of openfst and re2 and to configure openfst using ./configure --enable-grm.
  * This is what worked on Shiwali's setup (Ubuntu 19.04)
    * Installing OpenFST: download 1.6.8 from [openFST](http://www.openfst.org/twiki/bin/view/FST/FstDownload)
      `./configure --enable-grm --prefix=<path to your local conda environment>`
      `make & make install`
    * Installing re2: download re2 from [re2](git@github.com:google/re2.git)
      `make & make install --prefix=<path to your local conda environment>`
    * Installing pynini: `python setup.py install` seemed to install to my local conda environment. However, I was still unable to use Pynini. It seems like it is compiling some files that are not copied to the conda environment. Setting up `$PYTHONPATH` to `pynini-1.9.3` directory enabled running pynini.

### Usage
* Run [aileen_world](https://gitlab-external.parc.com/aileen/aileen-world)
* Activate the conda environment `conda activate aileen`
* Run aileen `python aileen.py`
  * Assuming webots and aileen-world are running, then should see debug messages in both aileen-world and aileen-agent about the objects detected.
 
This should startup the SoarJavaDebugger window that lets us inspect the state of Soar kernel.


### Architecture
An overview of how `aileen-agent` fits with the rest of the [architecture](https://gitlab-external.parc.com/aileen/aileen-agent/wikis/architecture-diagram)


### Structure and Editing
* aileen-agent has the following components: 
    1. a python client to aileen-world `aileen-agent/aileen.py`. This module makes requests as described in the [protocol](https://gitlab-external.parc.com/aileen/aileen-world/wikis/communication-protocol).
    2. a python interface to Soar kernel `aileen-agent/soar_interface` that uses python SWIG bindings to the Soar kernel
    2. a set of rules that guide reasoning in Soar in `aileen-agent/agent`
    
  Python client and the interface are easily edited in PyCharm. Rules can be edited in Eclipse using [Soar IDE](https://github.com/soartech/soaride).
  
### Misc
* My setup is throwing a lot of warnings. They are coming from SoarJavaDebugger that seems to be built for GTK-2. Adding the following environment variable should be useful in cutting down the warnings.
`export SWT_GTK3=0`

* If you want PyCharm terminal to show color coded logs, you can check the box for `emulate terminal output` in `Run configurations` dialog.

### Testing

`python -m pytest --junitxml=test.xml`

* From the top level dictory should result in a single test being run and a text.xml file being created at the top level.





## aileen-world
A simple xmlrpc python server to receive requests from the agent client and update the world accordingly. 
### Pre-requisites
* Anaconda
* Python 2
* Webots 

### Steps
* Download [Webots](https://www.cyberbotics.com/) and perform the standard install. Typically on Ubuntu, it will install it at `/usr/local/webots`.
* Clone this repository
  `git clone git@gitlab-external.parc.com:aileen/aileen-world.git`
* Configure a Python2 conda environment
   `conda create --name aileen_env python=2.7`
* Install relevant python packages
    `conda install coloredlogs`
    
### Usage
* Activate the conda environment
    `conda activate aileen_env`
* Configure the following environment variables
    * `WEBOTS_HOME` to your installation path /usr/local/webots recommended
    * `PYTHONPATH` to include `$WEBOTS_HOME/lib/python2.7` if you have the standard install
    * `LD_LIBRARY_PATH` to include `$WEBOTS_HOME/lib`
* Start webots on a terminal or from the GUI shell 
    `webots`
* Click on `File > Open World` and navigate to `aileen_world/worlds/aileen_world.wbt`. This should start the example world in webots. You can pause and play the world using control buttons in the webots application.
* On a separate terminal window, run aileen_world. Note, that the server will not run properly until the webots simulation is actually 'playing'. You will see a message stating taht the aileen world server is starting.
    `python world.py`


### Editing
   (I am using PyCharm for editing, so the instructions are specific to that IDE. Please adapt for other IDEs)
   * Open the project in PyCharm via `File > Open`
   * In `File > Settings`, point the project interpreter to your conda environment
   * Configure the following environment variable in `Run configurations`
        * `PYTHONPATH` to include `/usr/local/webots/lib/python2.7` if you have the standard install
        * `LD_LIBRARY_PATH` to include `/usr/local/webots/lib`

### Misc
* If you want PyCharm terminal to show color coded logs, you can check the box for `emulate terminal output` in the `Run configurations` dialog.
