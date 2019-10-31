#!/usr/bin/env python
import unittest
from log_config import logging
from aileen_grammar import AileenGrammar

class AileenGrammarTest(unittest.TestCase):
    
    def test_parse(self):
        logging.debug("[test_aileen_generator] :: test_parse")
        grammar = AileenGrammar()
        outputs = grammar.parse("blue box")
        self.maxDiff = None
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['obj', ['prop', 'blue'], 'box'])
        outputs = grammar.parse("the blue box")
        self.assertEquals(outputs[0], ['obj', ['prop', 'the'], ['prop', 'blue'], 'box'])
        outputs = grammar.parse("sphere on box")
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['obj', 'sphere', ['rel', 'on', ['obj', 'box']]])
        outputs = grammar.parse("sphere on box on block")
        self.assertEquals(len(outputs), 2)

    def test_fragments(self):
        logging.debug("[test_aileen_generator] :: test_fragments")
        grammar = AileenGrammar()
        outputs = grammar.parse("blue blue")
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['fragments', ['prop', 'blue'], ['prop', 'blue']])
        outputs = grammar.parse("blue box blue")
        self.assertEquals(len(outputs), 1)
        self.assertEquals(outputs[0], ['fragments', ['obj', ['prop', 'blue'], 'box'], ['prop', 'blue']])


if __name__ == '__main__':
    unittest.main()
