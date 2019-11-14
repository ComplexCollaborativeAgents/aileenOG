from aileen_object import AileenObject
from aileen_scene import AileenScene
from log_config import logging
from language_generator import LanguageGenerator
from agent.soar_interface import input_writer
import cv2
import numpy as np

image_output_folder = '/home/mshreve/M/Datasets/Aileen/session2'


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
        counter = 0
        while True:

            lesson = TrainingImage().generate_lesson(4)
            scene_acknowledgement = world_server.set_scene(
                {'configuration': lesson['scene'], 'label': lesson['interaction']})
            logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))
            binary_image = world_server.get_image()
            im = cv2.imdecode(np.fromstring(binary_image.data, dtype=np.uint8), 1)
            cv2.imwrite(image_output_folder + '/image' + str(counter) + '.png', im)
            meta = world_server.get_all()
            counter += 1


if __name__ == '__main__':
    lesson1 = TrainingImage()
    print(lesson1.generate_lesson())
