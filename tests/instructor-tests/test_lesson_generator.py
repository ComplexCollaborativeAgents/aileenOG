from random import uniform
from instructor.log_config import logging
from shapely.geometry import Point
import settings
from instructor.aileen_object import AileenObject
from instructor.aileen_scene import AileenScene
from instructor.language_generator import LanguageGenerator
from instructor.action_word_lesson import ActionWordLesson
from instructor.spatial_word_lesson import SpatialWordLesson
from instructor.visual_word_lesson import VisualWordLesson


def test_action_word_segment():
    logging.debug("[test_lesson_generator] :: test_action_word_segment")
    ActionWordLesson.randomizer = ActionRandomizer()
    AileenObject.randomizer = ObjectRandomizer()
    AileenScene.randomizer = SceneRandomizer()
    LanguageGenerator.randomizer = LanguageRandomizer()
    lesson1 = ActionWordLesson(is_positive=True, signal='verify', description=None, distractors=None, content=None)
    lesson1.generate_lesson()
    segment = lesson1.get_next_segment()
    assert segment == {'interaction': {'marker': 'start', 'content': 'move blue cylinder left of blue box', 'signal': 'verify'},
                       'scene': [
                           'Solid {\n   recognitionColors 0 0 1\n   translation 0.586304972021 0.45 0.092\n   children [\n       Shape {\n          appearance PBRAppearance {\n          baseColor 0 0 1\n          metalness 0\n          emissiveColor 0 0 1\n        }\n        geometry Cylinder {\n          radius 0.05\n          height 0.1\n        }\n        castShadows FALSE\n        }\n    ]\n    name "object1"\n   boundingObject Box {\n     size 0.1 0.1 0.1\n   }\n   physics Physics {\n}}',
                           'Solid {\n   recognitionColors 0 0 1\n   translation 0.506 0.45 -0.095\n   children [\n       Shape {\n          appearance PBRAppearance {\n          baseColor 0 0 1\n          metalness 0\n          emissiveColor 0 0 1\n        }\n        geometry Box {\n          size 0.1 0.1 0.1\n        }\n        castShadows FALSE\n        }\n    ]\n    name "object2"\n   boundingObject Box {\n     size 0.1 0.1 0.1\n   }\n   physics Physics {\n}}']}
    segment = lesson1.get_next_segment()
    assert segment == {'action': {'name': 'pick-up', 'uuid': 'object1'}, 'interaction': {'marker':'trace'}}
    segment = lesson1.get_next_segment()
    assert segment == {'action': {'location': [0.676147498734, 0.45, -0.0202334240995], 'name': 'place'},
                       'interaction': {'marker':'trace'}}
    segment = lesson1.get_next_segment()
    assert segment == None


def test_spatial_word_lesson():
    logging.debug("[test_lesson_generator] :: test_spatial_word_segment")
    AileenObject.randomizer = ObjectRandomizer()
    AileenScene.randomizer = SceneRandomizer()
    LanguageGenerator.randomizer = LanguageRandomizer()
    SpatialWordLesson.randomizer = SpatialRandomizer()
    lesson1 = SpatialWordLesson(is_positive=True,
                                signal='verify',
                                description=None,
                                distractors=None,
                                content=None)
    assert lesson1.generate_lesson() == {'interaction': {'content': 'blue cylinder right of blue box','signal': 'verify'}, 'scene': [
        'Solid {\n   recognitionColors 0 0 1\n   translation 0.676147498734 0.45 -0.0202334240995\n   children [\n       Shape {\n          appearance PBRAppearance {\n          baseColor 0 0 1\n          metalness 0\n          emissiveColor 0 0 1\n        }\n        geometry Cylinder {\n          radius 0.05\n          height 0.1\n        }\n        castShadows FALSE\n        }\n    ]\n    name "object1"\n   boundingObject Box {\n     size 0.1 0.1 0.1\n   }\n   physics Physics {\n}}',
        'Solid {\n   recognitionColors 0 0 1\n   translation 0.586304972021 0.45 0.238382561155\n   children [\n       Shape {\n          appearance PBRAppearance {\n          baseColor 0 0 1\n          metalness 0\n          emissiveColor 0 0 1\n        }\n        geometry Box {\n          size 0.1 0.1 0.1\n        }\n        castShadows FALSE\n        }\n    ]\n    name "object2"\n   boundingObject Box {\n     size 0.1 0.1 0.1\n   }\n   physics Physics {\n}}']}


def test_visual_word_lesson():
    logging.debug("[test_lesson_generator] :: test_visual_word_segment")
    AileenObject.randomizer = ObjectRandomizer()
    AileenScene.randomizer = SceneRandomizer()
    LanguageGenerator.randomizer = LanguageRandomizer()
    lesson1 = VisualWordLesson(is_positive=True,
                               signal='verify',
                               description=None,
                               distractors=None,
                               content=None)
    assert lesson1.generate_lesson() == {'interaction': {'content': 'blue cylinder', 'signal': 'verify'}, 'scene': [
        'Solid {\n   recognitionColors 0 0 1\n   translation 0.586304972021 0.45 0.238382561155\n   children [\n       Shape {\n          appearance PBRAppearance {\n          baseColor 0 0 1\n          metalness 0\n          emissiveColor 0 0 1\n        }\n        geometry Cylinder {\n          radius 0.05\n          height 0.1\n        }\n        castShadows FALSE\n        }\n    ]\n    name "object1"\n   boundingObject Box {\n     size 0.1 0.1 0.1\n   }\n   physics Physics {\n}}']}


class ObjectRandomizer:
    colors = ['blue']
    color_index = -1

    shapes = ['cylinder', 'box']
    shape_index = -1

    uuid = 0

    def get_random_color(self):
        self.color_index += 1
        return self.colors[self.color_index % len(self.colors)]

    def get_color_vector_sample(self, color_symbol):
        return AileenObject.get_colors()[color_symbol][0]

    def get_random_shape(self):
        self.shape_index += 13
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

    points = [Point(0.676147498734, -0.0202334240995, 0.3)]
    point_index = -1

    def get_random_position_on_table(self):
        self.position_index += 1
        position = self.positions[self.position_index % len(self.positions)]
        if (self.position_index >= len(self.positions)):
            position = [uniform(settings.OBJECT_POSITION_MIN_X, settings.OBJECT_POSITION_MAX_X),
                        uniform(settings.OBJECT_POSITION_MIN_Y, settings.OBJECT_POSITION_MAX_Y),
                        uniform(settings.OBJECT_POSITION_MIN_Z, settings.OBJECT_POSITION_MAX_Z)]
        logging.debug("[scene_randomizer] :: random table position {}".format(position))
        return position

    def sample_position_from_region(self, region):
        self.point_index += 1
        point = self.points[self.point_index % len(self.points)]
        logging.debug("[scene_randomizer] :: random region position {}".format(point))
        return point


class SpatialRandomizer:

    def random_spatial_configuration(self, configurations):
        return 'right-of'


class ActionRandomizer:

    def random_action(self, actions):
        return 'move-left-of'


class LanguageRandomizer:

    def shuffle_string(self, string):
        pass
