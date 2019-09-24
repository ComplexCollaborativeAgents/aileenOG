from aileen_object import AileenObject

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
