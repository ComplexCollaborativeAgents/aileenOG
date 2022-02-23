# Aileen
![](https://img.shields.io/badge/python-2.7-blue)

## Contributing

### Development environment setup

1. Install Anaconda or Miniconda

2. Generate a ssh key on your machine and register it with Gitlab

3. Clone this repository and its submodules:
   ```bash
   git clone --recurse-submodules git@gitlab-external.parc.com:aileen/aileen-agent.git
   ```
4. Check out this branch
   ```bash
   git checkout december2021demo
   ```
5. Run the `bootstrap.sh` script to set up the Conda environment and build the Python dependencies, such as
   [Pynini](http://www.openfst.org/twiki/bin/view/GRM/Pynini) and [Darknet](https://pjreddie.com/darknet/):
   ```bash
   cd aileen-agent
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



#### Running Aileen

Activate aileen env

`conda activate aileen`


1. On first run of webots, load the world: "File", "Open World...", and select `world/data/aileen_world_ur10_v3_recog.wbt`.
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
2. Run the world server in another shell instance:
   ```bash
   conda activate aileen
   ./runworld.sh
   ```
3. Run the agent in another shell instance:
   ```bash
   conda activate aileen
   python agent
   ```
   > Note: To suppress the GTK3 warnings: `export SWT_GTK3=0`.
4. Run an instructor script in another shell instance:
   ```bash
   conda activate aileen
   python instructor --json "./instructor/scripts/...somescript.json"
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
      'distractorswwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwyyyyy,s': 2 }
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