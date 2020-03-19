import json

from instructor.visual_word_lesson import VisualWordLesson
from instructor.spatial_word_lesson import SpatialWordLesson
from instructor.aileen_object import Color


class Curriculum(object):
    def __init__(self, lessons):
        self.curriculum = iter(lessons)

    def __iter__(self):
        return self

    def __next__(self):
        lesson_configuration = next(self.curriculum)
        lesson_type = lesson_configuration['lesson']
        distractors = lesson_configuration.get('distractors', 0)
        signal = lesson_configuration['signal']
        description = lesson_configuration['description']
        if lesson_type == 'visual':
            lesson_object = VisualWordLesson()
            lesson = lesson_object.generate_lesson(lesson_configuration, distractors)
            lesson['object'] = lesson_object
        elif lesson_type == 'spatial':
            language = lesson_configuration['language']
            configuration = language[1]
            objects = [language[0], language[-1]]
            lesson = SpatialWordLesson(configuration).generate_lesson(objects, distractors)
        elif lesson_type == 'action':
            pass

        lesson['interaction']['content'] = description
        lesson['interaction']['signal'] = signal

        return lesson

    next = __next__  # Python 2 support
