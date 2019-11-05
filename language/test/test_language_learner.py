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

    def test_parse_action(self):
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

if __name__ == '__main__':
    unittest.main()
