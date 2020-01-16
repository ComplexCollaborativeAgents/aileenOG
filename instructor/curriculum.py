import json

from instructor.visual_word_lesson import VisualWordLesson


class Curriculum(object):
    def __init__(self, lessons):
        self.curriculum = iter(lessons)

    def __iter__(self):
        return self

    def __next__(self):
        lesson_configuration = next(self.curriculum)
        lesson_type = lesson_configuration['lesson']
        shape = lesson_configuration['shape']
        color = lesson_configuration['color']
        distractors = lesson_configuration.get('distractors', 0)
        position = lesson_configuration.get('position', None)
        signal = lesson_configuration['signal']
        description = lesson_configuration['description']
        if lesson_type == 'visual':
            lesson = VisualWordLesson().generate_lesson(shape, color, position, distractors)
        elif lesson_type == 'spatial':
            pass
        elif lesson_type == 'action':
            pass

        lesson['interaction']['content'] = description
        lesson['interaction']['signal'] = signal

        return lesson

    next = __next__  # Python 2 support
