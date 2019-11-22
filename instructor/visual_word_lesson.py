from aileen_object import AileenObject
from aileen_scene import AileenScene
from language_generator import LanguageGenerator
from log_config import logging


class VisualWordLesson:
    def __init__(self):
        self._scene = AileenScene()
        self._language = None

    def generate_lesson(self, distractors=0):
        """
        :param distractors: Number of distractors to be generated. Default is zero distractors.
        """
        lesson = {}
        self.generate_scene(distractors)
        lesson['scene'] = self._scene.generate_scene_description()
        lesson['interaction'] = self._language
        return lesson

    def generate_scene(self, distractors):
        """
        :param distractors: Number of distractors to be generated.
        """
        logging.debug("[aileen_visual_word_lesson] :: generating a new scene for visual word learning")
        target = AileenObject.generate_random_object()
        target.set_translation(AileenScene.randomizer.get_random_position_on_table())
        self._scene.add_object(target)
        self._language = LanguageGenerator.generate_language_for_object(target)

        for distractor in self._generate_distractors(target, distractors):
            self._scene.add_object(distractor)

    def generate_training_example(self):
        pass

    def generate_verification_test(self):
        pass

    def generate_comprehension_test(self):
        pass

    def generate_generation_test(self):
        pass

    def _generate_distractors(self, target, n):
        distractors = []
        while n:
            distractor = AileenObject.generate_random_object()
            if distractor != target:
                distractor.set_translation(AileenScene.randomizer.get_random_position_on_table())
                distractors.append(distractor)
                n -= 1
        return distractors

    @staticmethod
    def administer_curriculum(world_server, agent_server):
        while True:
            raw_input("Press any key to generate the next lesson...")

            lesson = VisualWordLesson().generate_lesson(distractors=3)

            scene_acknowledgement = world_server.set_scene(
                {'configuration': lesson['scene'], 'label': lesson['interaction']})
            logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))

            # language_acknowledgement = agent_server.process_language(lesson['interaction'])
            # logging.info("[aileen_instructor] :: received from agent {}".format(language_acknowledgement))


if __name__ == '__main__':
    lesson1 = VisualWordLesson()
    print(lesson1.generate_lesson())
