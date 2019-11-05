#!/usr/bin/env python
import unittest
from log_config import logging
from language_learner import LanguageLearner

class LanguageLearnerTest(unittest.TestCase):

    def test_parse_description(self):
        # These tests build on each other.
        logging.debug("[test_language_learner] :: test_parse_description")
        learner = LanguageLearner()
        self.maxDiff = None
        #
        outputs = learner.parse_description("car")
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['obj', 'car'])
        #
        outputs = learner.parse_description("car")
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['obj', 'car'])
        #
        outputs = learner.parse_description("truck")
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['obj', 'truck'])
        #
        outputs = learner.parse_description("tractor")
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['obj', 'tractor'])
        #
        outputs = learner.parse_description("tan car")
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['obj', ['prop', 'tan'], 'car'])
        #
        outputs = learner.parse_description("tan car less than truck")
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['obj', ['prop', 'tan'], 'car', ['rel', 'less', 'than', ['obj', 'truck']]])
        #
        outputs = learner.parse_description("car between truck and tractor")
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['obj', 'car', ['rel', 'between', ['obj', 'truck'], 'and', ['obj', 'tractor']]])
        #
        outputs = learner.parse_description("car is tan")
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['obj', 'car', ['rel', 'is', ['prop', 'tan']]])

    def test_give_ball(self):
        logging.debug("[test_language_learner] :: test_parse_action")
        learner = LanguageLearner()
        grammar = learner.get_grammar()
        grammar.object_names = ["ball", "instructor"]
        grammar.object_rules = ["[obj_name]"]
        self.maxDiff = None
        outputs = learner.parse_action("give ball to instructor")
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['action', 'give', ['obj', 'ball'], 'to', ['obj', 'instructor']])
        outputs = learner.parse_action("give to instructor ball")
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['action', 'give', 'to', ['obj', 'instructor'], ['obj', 'ball']])

    def test_move_ball(self):
        logging.debug("[test_language_learner] :: testing 'move ball left of cylinder'")
        learner = LanguageLearner()
        grammar = learner.get_grammar()
        grammar.object_names = ["ball", "cylinder"]
        grammar.object_rules = ["[obj_name]", "[obj_name] [rel]"]
        grammar.relation_rules = ["left of [obj]"]
        self.maxDiff = None
        # ball is only left of cylinder in goal scene.
        scene1 = MockScene([['obj', 'ball'], ['obj', 'cylinder']])
        scene2 = MockScene([['obj', 'ball', ['rel', 'left', 'of', ['obj', 'cylinder']]]])
        outputs = learner.parse_action("move ball left of cylinder", [scene1, scene2])
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['action', 'move', ['obj', 'ball'], ['rel', 'left', 'of', ['obj', 'cylinder']]])
        # ball is left of cylinder in both scenes.
        grammar.reset_rules()
        grammar.object_names = ["ball", "cylinder"]
        grammar.object_rules = ["[obj_name]", "[obj_name] [rel]"]
        grammar.relation_rules = ["left of [obj]"]
        outputs = learner.parse_action("move ball left of cylinder", [scene2, scene2])
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['action', 'move', ['obj', 'ball', ['rel', 'left', 'of', ['obj', 'cylinder']]]])
        # ball is left of cylinder in initial scene.
        grammar.reset_rules()
        grammar.object_names = ["ball", "cylinder"]
        grammar.object_rules = ["[obj_name]", "[obj_name] [rel]"]
        grammar.relation_rules = ["left of [obj]"]
        outputs = learner.parse_action("move ball left of cylinder", [scene2, scene1])
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['action', 'move', ['obj', 'ball', ['rel', 'left', 'of', ['obj', 'cylinder']]]])

    def test_build_tower(self):
        logging.debug("[test_language_learner] :: testing 'build tall tower'")
        learner = LanguageLearner()
        grammar = learner.get_grammar()
        self.maxDiff = None
        # Build a tall tower from scratch.
        grammar.object_names = ["tower"]
        grammar.object_rules = ["[obj_name]", "[prop] [obj_name]"]
        grammar.property_names = ["tall"]
        scene1 = MockScene([])
        scene2 = MockScene([['obj', ['prop', 'tall'], 'tower']])
        outputs = learner.parse_action("build tall tower", [scene1, scene2])
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['action', 'build', ['obj', ['prop', 'tall'], 'tower']])
        # Build a tall tower out of a short tower.
        grammar.reset_rules()
        grammar.object_names = ["tower"]
        grammar.object_rules = ["[obj_name]", "[prop] [obj_name]"]
        grammar.property_names = ["tall"]
        scene1 = MockScene([['obj', 'tower']])
        scene2 = MockScene([['obj', ['prop', 'tall'], 'tower']])
        outputs = learner.parse_action("build tall tower", [scene1, scene2])
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['action', 'build', ['prop', 'tall'], ['obj', 'tower']])


class MockScene:

    def __init__(self, objects):
        self._objects = objects

    def ground_object_description(self, description):
        objects = []
        for object_ in self._objects:
            if description == object_:
                objects.append(object_)
        return objects

if __name__ == '__main__':
    unittest.main()
