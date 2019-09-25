from random import uniform
import constants

class AileenScene:
    def __init__(self):
        self._objects = []

    def add_object(self, aileen_object):
        self._objects.append(aileen_object)

    def generate_scene_description(self):
        description = []
        for scene_object in self._objects:
            description.append(scene_object.get_object_description())
        return description

    @staticmethod
    def get_random_position_on_table():
        position = [uniform(constants.OBJECT_POSITION_MIN_X, constants.OBJECT_POSITION_MAX_X),
                    uniform(constants.OBJECT_POSITION_MIN_Y, constants.OBJECT_POSITION_MAX_Y),
                    uniform(constants.OBJECT_POSITION_MIN_Z, constants.OBJECT_POSITION_MAX_Z)]
        return position
