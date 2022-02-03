from aileen_object import AileenObject
from aileen_scene import AileenScene
from language_generator import LanguageGenerator
from log_config import logging


class VisualWordLesson:
    def __init__(self, is_positive, signal, description, distractors, content, concept):
        self._scene = AileenScene()
        self._interaction = {}

        self._signal = signal
        self._is_positive = is_positive
        self._description = description
        self._distractors = distractors
        self._content = content
        self._concept = concept
        self._language = None

    def generate_lesson(self):
        """
        :param distractors: Number of distractors to be generated. Default is zero distractors.
        """
        if self._description is None:
            description = {}
        lesson = {}
        self.generate_scene()
        lesson['scene'] = self._scene.generate_scene_world_config()
        lesson['interaction'] = self._interaction

        if self._signal == 'generate':
            content = ""
            concept = self._concept
        else:
            concept=""
            if self._content is not None:
                content = self._content
            else:
                content = self._language


        lesson = {
            'scene': self._scene.generate_scene_world_config(),
            'interaction': {
                'signal': self._signal,
                'content': content,
                'concept': concept
            }
        }
        return lesson

    def generate_scene(self):
        """
        :param description: language set externally
        :param is_positive: should language describe the object or no
        :param distractors: Number of distractors to be generated.
        """
        logging.debug("[aileen_visual_word_lesson] :: generating a new scene for visual word learning")

        if self._description:
            target = AileenObject.generate_object(self._description)
            if self._description.get('position', None):
                target.set_translation(self._description['position'])
            else:
                target.set_translation(AileenScene.randomizer.get_random_position_on_table())
        else:
            target = AileenObject.generate_random_object()
            target.set_translation(AileenScene.randomizer.get_random_position_on_table())



        self._scene.add_object(target)

        distractor_objects = AileenObject.generate_distractors(target, self._distractors)

        for distractor in distractor_objects:
            distractor.set_translation(AileenScene.randomizer.get_random_position_on_table())
            self._scene.add_object(distractor)

        self._language = LanguageGenerator.generate_language_for_object(target, distractor_objects, self._is_positive)


    def evaluate_agent_response(self, agent_response):
        if 'status' in agent_response:
            if self._is_positive:
                if agent_response['status'] == 'success':
                    return {'signal': 'correct', 'score': 1}
                else:
                    return {'signal': 'incorrect', 'score': 0}
            else:
                if agent_response['status'] == 'failure':
                    return {'signal': 'correct', 'score': 1}
                else:
                    return {'signal': 'incorrect', 'score': 0}
        if 'language' in agent_response:
            if self._content == agent_response['language']:
                return {'signal': 'correct', 'score': 1}
            else:
                return {'signal': 'incorrect', 'score': 0}

    def administer_lesson(self, world, agent):
        lesson = self.generate_lesson()
        content = lesson['interaction']['content']
        scene_acknowledgement = world.set_scene(
             {'configuration': lesson['scene'], 'label': lesson['interaction']['content']})
        agent_response = agent.process_interaction(lesson['interaction'])
        evaluation = self.evaluate_agent_response(agent_response)
        score = evaluation['score']
        agent_response = agent.process_interaction(evaluation)
        return score, content

    @staticmethod
    def administer_curriculum(world_server, agent_server):
        while True:
            raw_input("Press any key to generate the next lesson...")

            lesson_object = VisualWordLesson(is_positive=True,
                                             signal="inform",
                                             description=None,
                                             distractors=None,
                                             content=None)
            lesson = lesson_object.generate_lesson()

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
