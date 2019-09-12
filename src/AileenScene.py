import json
import random
import sys
import uuid

import AileenObject

class AileenScene:

    def __init__(self):
        self.scene_id = str(uuid.uuid4())
        self.objects = list()
        self.objects.append(AileenObject.AileenObject())
        self.objects.append(AileenObject.AileenObject())
        self.objects.append(AileenObject.AileenObject())

