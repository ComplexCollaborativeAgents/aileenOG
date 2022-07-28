import random
import sys
import json
import settings
from experiments.log_config import logging
import copy

from instructor.curriculum import Curriculum
from instructor.spatial_word_lesson import SpatialWordLesson
from instructor.action_word_lesson import ActionWordLesson


class Generator:
    def __init__(self, lesson_type="visual-word", experiment_concept=None, num_episodes_per_concept=1, exam_length=1, max_distractors=0):
        self._lesson_type = lesson_type
        self._experiment_concept = experiment_concept
        self._num_episodes_per_concept = num_episodes_per_concept
        self._exam_length = exam_length
        self._max_distractors = max_distractors
        pass

    def generate_inform_training_gamut(self):
        lesson_config = None
        if self._lesson_type == "visual-word":
            lesson_config = self._generate_visual_word_lesson_descriptions(signal='inform',
                                                                           distractors=None,
                                                                           is_positive=True,
                                                                           number_of_samples=self._num_episodes_per_concept)
        if self._lesson_type == "spatial-word":
            lesson_config = self._generate_spatial_word_lesson_descriptions(signal='inform',
                                                                            distractors=None,
                                                                            is_positive=True,
                                                                            number_of_samples=self._num_episodes_per_concept)
        if self._lesson_type == "action-word":
            lesson_config = self._generate_action_word_lesson_descriptions(signal='inform',
                                                                           distractors=None,
                                                                           is_positive=True)
        return lesson_config

    def generate_verify_testing_gamut_generality(self):
        lesson_config = None
        if self._lesson_type == "visual-word":
            lesson_config = self._generate_visual_word_lesson_descriptions(signal='verify',
                                                                           is_positive=True,
                                                                           number_of_samples=self._exam_length,
                                                                           distractors=(0, self._max_distractors))
        if self._lesson_type == "spatial-word":
            lesson_config = self._generate_spatial_word_lesson_descriptions(signal='verify',
                                                                            distractors=(0, self._max_distractors),
                                                                            is_positive=True,
                                                                            number_of_samples=self._exam_length)
        if self._lesson_type == "action-word":
            lesson_config = self._generate_action_word_lesson_descriptions(signal='verify',
                                                                           distractors=(0,3),
                                                                           is_positive=True)

        return lesson_config

    def generate_verify_testing_gamut_specificity(self):
        lessons_config = None
        if self._lesson_type == "visual-word":
            lessons_config = self._generate_visual_word_lesson_descriptions(signal='verify',
                                                                            distractors=(0, self._max_distractors),
                                                                            is_positive=False,
                                                                            number_of_samples=self._exam_length)

        if self._lesson_type == "spatial-word":
            lessons_config = self._generate_spatial_word_lesson_descriptions(signal='verify',
                                                                            distractors=(0, self._max_distractors),
                                                                            is_positive=False,
                                                                            number_of_samples = self._exam_length)
        if self._lesson_type == "action-word":
            lessons_config = self._generate_action_word_lesson_descriptions(signal='verify',
                                                                           distractors=(0,3),
                                                                           is_positive=False)

        if lessons_config is None:
            raise Exception("Lesson type undefined")

        return lessons_config

    def _generate_visual_word_lesson_descriptions(self, signal, distractors, is_positive, number_of_samples=1):
        shapes = settings.SHAPE_SET
        with open(settings.COLOR_PATH, 'r') as f:
            colors = json.load(f).keys()
        with open(settings.SIZE_PATH, 'r') as f:
            sizes = json.load(f).keys()

        def generate_lesson_description(color, shape, size, distractors, signal, is_positive):
            if color is None and shape is None and size is None:
                sys.exit('Either color or shape is required!')
            lesson = {'lesson-type': 'visual-word', 'is_positive': str(is_positive), 'description': {}}
            if color:
                lesson['description']['color'] = color
            if shape:
                lesson['description']['shape'] = shape
            # lesson['description']['size']= 'medium'
            if size:
                lesson['description']['size'] = size
            if distractors:
                lesson['distractors'] = random.randint(distractors[0], distractors[1])
            if signal:
                lesson['signal'] = signal
            return lesson

        print("Concept is", self._experiment_concept, 'and samples is', number_of_samples)

        if self._experiment_concept and number_of_samples > 1:
            lessons = []
            if self._experiment_concept in shapes:
                for i in range(0, number_of_samples):
                    color_choice = random.choice(colors)
                    size_choice = random.choice(sizes)
                    if not is_positive:
                        reduced_set = copy.deepcopy(shapes)
                        reduced_set.remove(self._experiment_concept)
                        
                        new_lesson = generate_lesson_description(color_choice, random.choice(reduced_set), size_choice, distractors, signal, is_positive)
                        new_lesson['content'] = "{} {} {}".format(color_choice, size_choice, self._experiment_concept)
                        lessons += [new_lesson]
                    else:
                        new_lesson = generate_lesson_description(color_choice, self._experiment_concept, size_choice, distractors, signal, is_positive)
                        new_lesson['content'] = "{} {}".format(size_choice, self._experiment_concept)
                        lessons += [new_lesson]

            if self._experiment_concept in colors:
                for i in range(0, number_of_samples):
                    shape_choice = random.choice(shapes)
                    size_choice = random.choice(sizes)
                    if not is_positive:
                        reduced_set = copy.deepcopy(colors)
                        reduced_set.remove(self._experiment_concept)
                        
                        new_lesson = generate_lesson_description(random.choice(reduced_set), shape_choice, size_choice, distractors, signal, is_positive)
                        new_lesson['content'] = "{} {} {}".format(self._experiment_concept, size_choice, shape_choice)
                        lessons += [new_lesson]
                    else:
                        new_lesson = generate_lesson_description(self._experiment_concept, shape_choice, size_choice,
                                                                 distractors, signal, is_positive)
                        new_lesson['content'] = "{} {}".format(self._experiment_concept, shape_choice)
                        lessons += [new_lesson]

            # print('SHAPES ARE', shapes)

            # wwh adding size
            if self._experiment_concept in sizes:
                for i in range(0, number_of_samples):
                    shape_choice = random.choice(shapes)
                    color_choice = random.choice(colors)
                    if not is_positive:
                        reduced_set = copy.deepcopy(sizes)
                        reduced_set.remove(self._experiment_concept)
                        size_choice = random.choice(reduced_set)
                        new_lesson = generate_lesson_description(color_choice, shape_choice, size_choice, distractors, signal, is_positive)
                        new_lesson['content'] = "{} {} {}".format(color_choice, self._experiment_concept, shape_choice)
                        lessons += [new_lesson]
                    else:
                        shape_choice = random.choice(shapes)
                        color_choice = random.choice(colors)
                        # color_choice = None # grammar can't handle color and size right now
                        new_lesson = generate_lesson_description(color_choice, shape_choice, self._experiment_concept,
                                                                 distractors, signal, is_positive)
                        new_lesson['content'] = "{}".format(self._experiment_concept)
                        lessons += [new_lesson]


            return lessons

        else:

            # print('WE ARE HERE num samples', number_of_samples)

            if number_of_samples > 1 and not self._experiment_concept:
                logging.warning("[generator] :: generating multiple samples for unique combination of objects")
            lessons = []
            for i in range(0, number_of_samples):
                lessons += [generate_lesson_description(c, s, None, distractors, signal, is_positive) for s in shapes for c in colors]
                if self._experiment_concept is None:
                    lessons += [generate_lesson_description(None, s, None, distractors, signal, is_positive) for s in shapes]
            random.shuffle(lessons)
            print('lessons are', lessons)
            return lessons

    def _generate_spatial_word_lesson_descriptions(self, signal, distractors, is_positive, number_of_samples=5):
        relation_configs_list = SpatialWordLesson.get_spatial_configurations_set()
        relation_configs = {}
        if self._experiment_concept:
            relation_configs[self._experiment_concept] = relation_configs_list[self._experiment_concept]
        else:
            relation_configs = relation_configs_list

        logging.debug("[generator] :: relation list is {}".format(relation_configs))

        shapes = settings.SHAPE_SET
        with open(settings.COLOR_PATH, 'r') as f:
            colors = json.load(f).keys()

        with open(settings.SIZE_PATH, 'r') as f:
            sizes = json.load(f).keys()

        object_set = [{"color": c, "shape": s, "size": size} for s in shapes for c in colors for size in sizes]

        lessons = []
        for key in relation_configs.keys():
            for i in range(0, number_of_samples):
                random.shuffle(object_set)
                lesson = {'lesson-type': 'spatial-word',
                          'is_positive': str(is_positive),
                          'content': "{} {} {} {} {}".format(object_set[0]['color'], object_set[0]['shape'], key, object_set[1]['color'], object_set[1]['shape']),
                          'description': {
                              "objects": [object_set[0], object_set[1]],
                              "relation": key
                            },
                          'signal': signal
                          }
                if distractors:
                    lesson['distractors'] = random.randint(distractors[0], distractors[1])
                lessons.append(lesson)

        random.shuffle(lessons)
        return lessons


    def _generate_action_word_lesson_descriptions(self, signal, distractors, is_positive, number_of_samples=8):
        action_configs = ActionWordLesson.get_action_definition_set()
        lessons = []

        for key in action_configs.keys():
            for i in range(0, number_of_samples):
                description = action_configs[key]
                description['action'] = key
                lesson = {
                    'lesson-type': 'action-word',
                    'is_positive': str(is_positive),
                    'description': description,
                    'signal': signal}
                if distractors:
                    lesson['distractors'] = random.randint(distractors[0], distractors[1])
                lessons.append(lesson)
        random.shuffle(lessons)
        return lessons


if __name__ == '__main__':

    rail = Generator("visual-word")
    test_gamut = rail.generate_verify_testing_gamut_specificity()
    print json.dumps(test_gamut)



