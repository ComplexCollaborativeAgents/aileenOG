import os
from os import path

OS_ROOT_PATH = path.abspath(os.sep)
ROOT_PATH = path.join(path.dirname(path.dirname(path.abspath(__file__))))
CONCEPT_LEARNER_PATH = path.join(ROOT_PATH, 'agent', 'concept_learner')
#SOAR_PATH = path.join("/home/smohan/soar/soar9.6/bin")
SOAR_PATH = path.join("/home/smohan/soar/SoarSuite_9.6.0-Multiplatform_64bit/bin")
#SOAR_PATH = path.join(OS_ROOT_PATH, 'usr', 'local', 'SoarSuite_9.6.0-Multipjlatform_64bit', 'bin', 'linux64')
#SOAR_PATH = path.join(OS_ROOT_PATH, 'usr', 'local', 'SoarSuite_9.6.0-Multiplatform_64bit', 'bin')
SOAR_AGENT_PATH = path.join(ROOT_PATH, 'agent', 'soar_interface', 'soar', 'load.soar')
SOAR_AGENT_NAME = 'aileen'
SOAR_SLEEP_TIME = 0.0001
SOAR_DEBUG = True
SOAR_SVS = False
SOAR_CV = False
CURRENT_IMAGE_PATH = path.join(ROOT_PATH, 'controllers', 'images', 'current_image.png')
COLOR_PATH = path.join(ROOT_PATH, 'instructor', 'resources', 'colors.json')

# Servers
WORLD_HOST = 'localhost'
WORLD_PORT = 30000
AGENT_HOST = 'localhost'
AGENT_PORT = 40001
CONCEPT_LEARNER_HOST = 'dubs.parc.xerox.com'
CONCEPT_LEARNER_PORT = 8080

# World
ROBOT_PLATE_LOCATION = [-0.1, 0.2, 0]
TIME_STEP = 32
TEST_LOCATION = [0.771, 0.5610656650000001, -0.199]

# Instructor
OBJECT_POSITION_MAX_X = 0.855
OBJECT_POSITION_MIN_X = 0.356

OBJECT_POSITION_MAX_Y = 0.45
OBJECT_POSITION_MIN_Y = 0.45

OBJECT_POSITION_MAX_Z = 0.242
OBJECT_POSITION_MIN_Z = -0.245

OBJECT_STANDARD_HEIGHT = 0.1
OBJECT_STANDARD_WIDTH_X = 0.1
OBJECT_STANDARD_WIDTH_Z = 0.1

SHAPE_SET = ['cone', 'box', 'cylinder', 'sphere']

SPATIAL_CONFIGURATION_FILE_NAME = 'spatial_configuration.json'
SPATIAL_DEF_OBJECTS = 'objects'
SPATIAL_DEF_DEFINITION = 'definition'
SPATIAL_DEF_LANGUAGE_TEMPLATE = 'language'

ACTION_DEFINITION_FILE_NAME = 'action_definition.json'
ACTION_DEF_OBJECTS = 'objects'
ACTION_DEF_RELATIONS = 'relations'
ACTION_DEF_INIT_CONFIG = 'initial_configuration'
ACTION_DEF_TRACE_ACTIONS = 'trace-actions'
ACTION_DEF_LANGUAGE = 'language'

ACTION_LESSON_STATE_START = 'start'
ACTION_LESSON_STATE_TRACE = 'trace'
ACTION_LESSON_STATE_END = 'end'
ACTION_LESSON_STATE_COMPLETE = 'complete'
ACTION_LESSON_STATE_BAD = 'bad'

## Agent
AGENT_PARAM_RUNTIME_FILE = path.join(ROOT_PATH, 'agent', 'soar_interface', 'soar', '_agent_params_runtime.soar')
AGENT_VISUAL_CONCEPTS_PARAM = 'external'
AGENT_PRELOAD_VISUAL_CONCEPTS_PARAM = 'false'

AGENT_SPATIAL_CONCEPTS_PARAM = 'soar'
AGENT_PRELOAD_SPATIAL_CONCEPTS_PARAM = 'true'

AGENT_ACTION_CONCEPTS_PARAM = 'soar'
AGENT_PRELOAD_ACTION_CONCEPTS_PARAM = 'true'

try:
    from local_settings import *
except ImportError:
    pass
