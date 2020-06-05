import json

from instructor.visual_word_lesson import VisualWordLesson
from instructor.spatial_word_lesson import SpatialWordLesson
from instructor.action_word_lesson import ActionWordLesson
from language_generator import LanguageGenerator


class Curriculum(object):
    def __init__(self, lessons):
        self.curriculum = iter(lessons)

    def __iter__(self):
        return self

    def __next__(self):
        lesson_configuration = next(self.curriculum)

        lesson_type = lesson_configuration['lesson-type']
        signal = lesson_configuration['signal']

        description = lesson_configuration.get('description', None)
        distractors = lesson_configuration.get('distractors', 0)
        is_positive = True if lesson_configuration.get('is_positive', "True") == "True" else False
        content = lesson_configuration.get('content', None)

        lesson = {}
        if lesson_type == 'visual-word':
            lesson_object = VisualWordLesson(is_positive=is_positive,
                                             signal=signal,
                                             description=description,
                                             distractors=distractors,
                                             content=content)
            #lesson = lesson_object.generate_lesson()

        elif lesson_type == 'spatial-word':
            lesson_object = SpatialWordLesson(is_positive=is_positive,
                                              signal=signal,
                                              description=description,
                                              distractors=distractors,
                                              content=content)
            #lesson = lesson_object.generate_lesson()

        elif lesson_type == 'action-word':
            lesson_object = ActionWordLesson(is_positive=is_positive,
                                             signal=signal,
                                             description=description,
                                             distractors=distractors,
                                             content=content)

        else:
            return None

        lesson['object'] = lesson_object
        lesson['type'] = lesson_type
        return lesson

    next = __next__  # Python 2 support
