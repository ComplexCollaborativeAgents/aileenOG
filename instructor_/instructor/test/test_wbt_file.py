import os
import sys
import unittest

from instructor.aileen_scene import AileenScene

class SceneFileTest(unittest.TestCase):

    def test_write_scene(self):

        with open("scene.wbt", "w") as out:
            d = os.path.dirname(os.path.abspath(__file__))
            scene = AileenScene()
            scene.write_scene(os.path.join(d, "template.wbt"), out)

        self.assertTrue(os.path.isfile("scene.wbt"))

    @unittest.expectedFailure
    def test_nonexistent_template(self):
        scene = AileenScene()
        scene.write_scene("nofile.wbt", sys.stdout)
