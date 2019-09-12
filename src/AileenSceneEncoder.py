

import json

class AileenSceneEncoder(json.JSONEncoder):

    def default(self, o):
        return o.__dict__


