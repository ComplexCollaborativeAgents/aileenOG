#!/usr/bin/env python
import unittest
from log_config import logging
from language_learner import LanguageLearner

class LanguageLearnerTest(unittest.TestCase):

    def test_parse_description(self):
        logging.debug("[test_language_learner] :: test_parse_description")
        learner = LanguageLearner()
        outputs = learner.parse_description("car")
        self.maxDiff = None
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['obj', 'car'])


if __name__ == '__main__':
    unittest.main()
