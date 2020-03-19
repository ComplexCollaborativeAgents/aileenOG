from aileen_object import AileenObject
from aileen_scene import AileenScene
from language_generator import LanguageGenerator
from log_config import logging


class VisualWordLesson:
    def __init__(self):
        self._scene = AileenScene()
        self._interaction = {}

    def generate_lesson(self, description=None, distractors=0):
        """
        :param distractors: Number of distractors to be generated. Default is zero distractors.
        """
        if description is None:
            description = {}
        lesson = {}
        self.generate_scene(description, distractors)
        lesson['scene'] = self._scene.generate_scene_description()
        lesson['interaction'] = self._interaction
        return lesson

    def generate_scene(self, description, distractors):
        """
        :param distractors: Number of distractors to be generated.
        """
        logging.debug("[aileen_visual_word_lesson] :: generating a new scene for visual word learning")

        if description:
            target = AileenObject.generate_object(description)
        else:
            target = AileenObject.generate_random_object()
        if description.get('position', None):
            target.set_translation(description['position'])
        else:
            target.set_translation(AileenScene.randomizer.get_random_position_on_table())
        self._scene.add_object(target)

        for distractor in AileenObject.generate_distractors(target, distractors):
            distractor.set_translation(AileenScene.randomizer.get_random_position_on_table())
            self._scene.add_object(distractor)

        self._interaction['signal'] = 'verify'
        self._interaction['content'] = LanguageGenerator.generate_language_for_object(target)

    def evaluate_agent_response(self, agent_response):
        if agent_response['status'] == 'success':
            return {'signal': 'correct'}

        if agent_response['status'] == 'failure':
            return {'signal': 'correct'}

    @staticmethod
    def administer_curriculum(world_server, agent_server):
        while True:
            raw_input("Press any key to generate the next lesson...")

            lesson_object = VisualWordLesson()
            lesson = lesson_object.generate_lesson(distractors=0)

            scene_acknowledgement = world_server.set_scene(
                {'configuration': lesson['scene'], 'label': lesson['interaction']['content']})
            logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))

            agent_response = agent_server.process_interaction(lesson['interaction'])
            logging.info("[aileen_instructor] :: received from agent {}".format(agent_response))

            evaluation = lesson_object.evaluate_agent_response(agent_response)
            agent_response = agent_server.process_interaction(evaluation)
            logging.info("[aileen_instructor] :: provided feedback to agent")


if __name__ == '__main__':
    lesson1 = VisualWordLesson()
    print(lesson1.generate_lesson())
