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
* Configure a Python2 conda environment

       `conda create --name aileen python=2.7`

       `conda install coloredlogs`
    
   
## Usage
* Run [aileen_world](https://gitlab-external.parc.com/aileen/aileen-world)
* Activate the conda environment `conda activate aileen`
* Run aileen `python aileen.py`
 
 
This should startup a window that lets us inspect the state of Soar kernel.