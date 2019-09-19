#!/usr/bin/env python
import unittest

from AileenObject import AileenObject

class AileenObjectTest(unittest.TestCase):

    def test_get_colors(self):
        obj = AileenObject()
        colors = obj.get_colors()
        self.assertEquals(len(colors.keys()), 4)

if __name__ == '__main__':
    unittest.main()
