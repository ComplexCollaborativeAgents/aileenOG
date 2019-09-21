#!/usr/bin/env python2
import os
import unittest

import scene_writer

class scene_writer_test(unittest.TestCase):

    def test_write_scene(self):
        output_file = scene_writer.generated_scene_file
        if os.path.isfile(output_file):
            os.remove(output_file)
        objects = list()
        objects.append('Solid {\ntranslation 5, 5, 5\n}\n')
        objects.append('Solid {\ntranslation 6, 6, 6\n}\n')
        scene_writer.write_scene(objects)
        self.assertTrue(os.path.isfile(output_file))
        os.remove(output_file)

if __name__ == '__main__':
   unittest.main() 
