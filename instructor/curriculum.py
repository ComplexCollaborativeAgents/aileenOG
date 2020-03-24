import json

from instructor.visual_word_lesson import VisualWordLesson
from instructor.spatial_word_lesson import SpatialWordLesson
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
        is_positive = lesson_configuration.get('is_positive', "True")

        if lesson_type == 'visual-word':
            if is_positive == 'True':
                lesson_object = VisualWordLesson(is_positive=True, signal=signal, description=description, distractors=distractors)
            else:
                lesson_object = VisualWordLesson(is_positive=False, signal=signal, description=description, distractors=distractors)

            lesson = lesson_object.generate_lesson()
            lesson['object'] = lesson_object

        elif lesson_type == 'spatial-word':
            language = lesson_configuration['language']
            configuration = language[1]
            objects = [language[0], language[-1]]
            lesson = SpatialWordLesson(configuration).generate_lesson(objects, distractors)
            description = lesson_configuration['description']
        elif lesson_type == 'action':
            pass

        if 'content' in lesson_configuration:
            lesson['interaction']['content'] = lesson_configuration['content']
        lesson['interaction']['signal'] = signal

        return lesson

    next = __next__  # Python 2 support
