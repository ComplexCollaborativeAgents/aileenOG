#!/usr/bin/env python
# from aileen_object import aileen_object
from random import uniform
import unittest
from log_config import logging
import constants
from aileen_object import AileenObject
from aileen_scene import AileenScene
from language_generator import LanguageGenerator
from action_word_lesson import ActionWordLesson
from spatial_word_lesson import SpatialWordLesson
from visual_word_lesson import VisualWordLesson

class LessonGeneratorTest(unittest.TestCase):
    
    def test_action_word_segment(self):
        logging.debug("[test_lesson_generator] :: test_action_word_segment")
        ActionWordLesson.test_id = 1
        AileenObject.randomizer = ObjectRandomizer()
        AileenScene.test_id = 2
        LanguageGenerator.test_id = 1
        self.maxDiff = None
        lesson1 = ActionWordLesson()
        segment = lesson1.get_next_segment()
        self.assertEquals(segment, {'interaction': {'marker': 'start', 'language': 'move blue cylinder  left-of blue box'}, 'scene': ['Solid {\n   translation 0.586304972021 0.45 0.238382561155\n   children [\n       Shape {\n          appearance PBRAppearance {\n          baseColor 0 0 1\n          metalness 0\n          emissiveColor 0 0 1\n        }\n        geometry Cylinder {\n          radius 0.05\n          height 0.1\n        }\n        castShadows FALSE\n        }\n    ]\n    name "1"\n   boundingObject Box {\n     size 0.1 0.1 0.1\n   }\n   physics Physics {\n}}', 'Solid {\n   translation 0.477095092251 0.45 -0.167128202174\n   children [\n       Shape {\n          appearance PBRAppearance {\n          baseColor 0 0 1\n          metalness 0\n          emissiveColor 0 0 1\n        }\n        geometry Box {\n          size 0.1 0.1 0.1\n        }\n        castShadows FALSE\n        }\n    ]\n    name "2"\n   boundingObject Box {\n     size 0.1 0.1 0.1\n   }\n   physics Physics {\n}}']})
        segment = lesson1.get_next_segment()
        self.assertEquals(segment, {'action': {'name': 'pick-up', 'uuid': '1'}, 'interaction': 'none'})
        segment = lesson1.get_next_segment()
        self.assertEquals(segment, {'action': None, 'interaction': 'none'})
        segment = lesson1.get_next_segment()
        self.assertEquals(segment, None)
    
    def test_spatial_word_lesson(self): 
        logging.debug("[test_lesson_generator] :: test_spatial_word_segment")
        AileenObject.randomizer = ObjectRandomizer()
        AileenScene.test_id = 1
        # AileenScene.test_id = None
        # AileenScene.randomizer = SceneRandomizer()
        LanguageGenerator.test_id = 1
        SpatialWordLesson.test_id = 1
        lesson1 = SpatialWordLesson()
        self.maxDiff = None
        self.assertEquals(lesson1.generate_lesson(), {'interaction': {'language': 'blue cylinder  right-of blue box'}, 'scene': ['Solid {\n   translation 0.676147498734 0.45 -0.0202334240995\n   children [\n       Shape {\n          appearance PBRAppearance {\n          baseColor 0 0 1\n          metalness 0\n          emissiveColor 0 0 1\n        }\n        geometry Cylinder {\n          radius 0.05\n          height 0.1\n        }\n        castShadows FALSE\n        }\n    ]\n    name "1"\n   boundingObject Box {\n     size 0.1 0.1 0.1\n   }\n   physics Physics {\n}}', 'Solid {\n   translation 0.750422886282 0.45 0.177034392513\n   children [\n       Shape {\n          appearance PBRAppearance {\n          baseColor 0 0 1\n          metalness 0\n          emissiveColor 0 0 1\n        }\n        geometry Box {\n          size 0.1 0.1 0.1\n        }\n        castShadows FALSE\n        }\n    ]\n    name "2"\n   boundingObject Box {\n     size 0.1 0.1 0.1\n   }\n   physics Physics {\n}}']})
    
    def test_visual_word_lesson(self):
        logging.debug("[test_lesson_generator] :: test_visual_word_segment")
        AileenObject.randomizer = ObjectRandomizer()
        AileenScene.test_id = None
        AileenScene.randomizer = SceneRandomizer()
        LanguageGenerator.test_id = 1
        lesson1 = VisualWordLesson()
        self.maxDiff = None
        self.assertEquals(lesson1.generate_lesson(), {'interaction': 'blue cylinder ', 'scene': ['Solid {\n   translation 0.586304972021 0.45 0.238382561155\n   children [\n       Shape {\n          appearance PBRAppearance {\n          baseColor 0 0 1\n          metalness 0\n          emissiveColor 0 0 1\n        }\n        geometry Cylinder {\n          radius 0.05\n          height 0.1\n        }\n        castShadows FALSE\n        }\n    ]\n    name "1"\n   boundingObject Box {\n     size 0.1 0.1 0.1\n   }\n   physics Physics {\n}}']})
        
class ObjectRandomizer:

        colors = ['blue']
        color_index = -1

        shapes = ['cylinder', 'box']
        shape_index = -1

        uuid = 0

        def get_random_color(self):
            self.color_index += 1
            return self.colors[self.color_index % len(self.colors)]

        def get_color_vector_sample(self,color_symbol):
            return AileenObject.get_colors()[color_symbol][0]

        def get_random_shape(self):
            self.shape_index += 1
            return self.shapes[self.shape_index % len(self.shapes)]

        def uuid4(self):
            self.uuid += 1
            return self.uuid

class SceneRandomizer:

    positions = [[0.586304972021, 0.45, 0.238382561155],
                 [0.477095092251, 0.45, -0.1671282021741],
                 [0.8094095153766557, 0.45, 0.10263457589999814],
                 [0.8171403687520798, 0.45, -0.16360461612604268]]
    position_index = -1

    def get_random_position_on_table(self):
        self.position_index += 1
        position = self.positions[self.position_index % len(self.positions)]
        if (self.position_index >= len(self.positions)):
            position = [uniform(constants.OBJECT_POSITION_MIN_X, constants.OBJECT_POSITION_MAX_X),
                        uniform(constants.OBJECT_POSITION_MIN_Y, constants.OBJECT_POSITION_MAX_Y),
                        uniform(constants.OBJECT_POSITION_MIN_Z, constants.OBJECT_POSITION_MAX_Z)]
        logging.debug("[scene_randomizer] :: random position {}".format(position))
        return position

if __name__ == '__main__':
    unittest.main()
