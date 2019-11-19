# Matthew Shreve, PARC 2019
# Generates random arrangements of N shapes in webots simulator
# Saves annotated images to disk and generates training files

from aileen_object import AileenObject
from aileen_scene import AileenScene
from log_config import logging
from language_generator import LanguageGenerator
from agent.soar_interface import input_writer
import cv2
import numpy as np
import constants
import random
import os

# create output folder if it does not exist
os.makedirs(constants.TRAINING_DATA_FOLDER)


class TrainingImage:
    def __init__(self):
        self._scene = AileenScene()
        self._language = None

    def generate_lesson(self, num_objects=1):
        lesson = {}
        self.generate_scene(num_objects)
        lesson['scene'] = self._scene.generate_scene_description()
        lesson['interaction'] = self._language
        return lesson

    def generate_scene(self, num_objects=1):
        logging.debug("[aileen_visual_word_lesson] :: generating a new scene for visual word learning")
        for i in range(0, num_objects):
            scene_object = AileenObject.generate_random_object()
            scene_object.set_translation(AileenScene.randomizer.get_random_position_on_table())
            self._scene.add_object(scene_object)
            self._language = LanguageGenerator.generate_language_for_object(scene_object)

    @staticmethod
    def generate_scenes(world_server, agent_server):

        # iw = input_writer.InputWriter(agent_server, world_server)
        counter = 557
        while True:
            num_obj = random.randint(1,6)
            lesson = TrainingImage().generate_lesson(num_obj)
            scene_acknowledgement = world_server.set_scene(
                {'configuration': lesson['scene'], 'label': lesson['interaction']})
            logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))
            binary_image = world_server.get_image()
            im = cv2.imdecode(np.fromstring(binary_image.data, dtype=np.uint8), 1)
            cv2.imwrite(constants.TRAINING_DATA_FOLDER + '/frame_' + "{:0>6d}".format(counter) + '.jpg', im)
            meta = world_server.get_all()

            for j in range(0, len(meta['objects'])):
                obj = meta['objects'][j]
                bb = obj['bounding_box_camera']
                shape = obj['shape'].split('s_')[1]
                with open(constants.TRAINING_DATA_FOLDER + '/frame_' + "{:0>6d}".format(counter) + '.txt', 'a+') as f:
                    f.write("%d %f %f %f %f\n" % (constants.SHAPE_SET.index(shape), bb[0], bb[1], bb[2], bb[3]))

            counter += 1


if __name__ == '__main__':
    lesson1 = TrainingImage()
    print(lesson1.generate_lesson())
