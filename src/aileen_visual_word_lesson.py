from aileen_object import AileenObject
from aileen_scene import AileenScene
from log_config import logging


class AileenVisualWordLesson:
    def __init__(self):
        self._scene = AileenScene()
        self._language = {}

    def generate_lesson(self):
        lesson = {}
        self.generate_scene()
        lesson['scene'] = self._scene.generate_scene_description()
        lesson['interaction'] = self._language
        return lesson

    def generate_scene(self):
        logging.debug("[aileen_visual_word_lesson] :: generating a new scene for visual word learning")
        position = AileenScene.get_random_position_on_table()
        scene_object = AileenObject.generate_random_object_at(position)
        self._scene.add_object(scene_object)
        self._language['language'] = "{} {}".format(scene_object_color, scene_object_shape)

    def generate_training_example(self):
        pass

    def generate_verification_test(self):
        pass

    def generate_comprehension_test(self):
        pass

    def generate_generation_test(self):
        pass

if __name__ == '__main__':
    lesson1 = AileenVisualWordLesson()
    print(lesson1.generate_lesson())
