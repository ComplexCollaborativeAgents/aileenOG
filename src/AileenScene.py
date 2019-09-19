import json
import random
import sys
import uuid

import AileenObject
import AileenSceneEncoder

TEMPLATE_OBJECTS_MARKER = '<objects>'

class AileenScene:

    def __init__(self):
        self.scene_id = str(uuid.uuid4())
        self.objects = list()
        self.objects.append(AileenObject.AileenObject())
        self.objects.append(AileenObject.AileenObject())
        self.objects.append(AileenObject.AileenObject())


    def write_scene(self, template_file, out):
        '''
        Using the given template file, write a Webots world file expanding
        the number of objects specified. The out parameter is a file handle
        to use for writing the contents of the file.
        '''

        with open(template_file) as f:
            for line in f:
                cleaned_line = line.strip()
                if cleaned_line.startswith(TEMPLATE_OBJECTS_MARKER):
                    for obj in self.objects:
                        out.write(obj.get_yaml())
                        out.write('\n')
                else:
                    out.write(line)

    def get_json(self):
        return json.dumps(self, indent=4, cls=AileenSceneEncoder.AileenSceneEncoder)
