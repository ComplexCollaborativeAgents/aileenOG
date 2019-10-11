from aileen_object import AileenObject
from aileen_scene import AileenScene
from log_config import logging
from language_generator import LanguageGenerator


class VisualWordLesson:
    def __init__(self):
        self._scene = AileenScene()
        self._language = None

    def generate_lesson(self):
        lesson = {}
        self.generate_scene()
        lesson['scene'] = self._scene.generate_scene_description()
        lesson['interaction'] = self._language
        return lesson

    def generate_scene(self):
        logging.debug("[aileen_visual_word_lesson] :: generating a new scene for visual word learning")
        scene_object = AileenObject.generate_random_object()
        scene_object.set_translation(AileenScene.randomizer.get_random_position_on_table())
        self._scene.add_object(scene_object)
        self._language = LanguageGenerator.generate_language_for_object(scene_object)

    def generate_training_example(self):
        pass

    def generate_verification_test(self):
        pass

    def generate_comprehension_test(self):
        pass

    def generate_generation_test(self):
        pass

    @staticmethod
    def administer_curriculum(world_server, agent_server):
        while True:
            raw_input("Press any key to generate the next lesson...")

            lesson = VisualWordLesson().generate_lesson()

            scene_acknowledgement = world_server.set_scene(
                {'configuration': lesson['scene'], 'label': lesson['interaction']})
            logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))

            language_acknowledgement = agent_server.process_language(lesson['interaction'])
            logging.info("[aileen_instructor] :: received from agent {}".format(language_acknowledgement))

if __name__ == '__main__':
    lesson1 = VisualWordLesson()
    print(lesson1.generate_lesson())
