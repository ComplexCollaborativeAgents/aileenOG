import random
from aileen_object import AileenObject
from copy import deepcopy

import settings
from random import shuffle

class LanguageGenerator:

    def __init__(self):
        pass

    @staticmethod
    def generate_language_for_object(aileen_object, distractors=[], is_positive = True):
        ## SM: removing this because the default rules in the language parser do not accept any arbitrary order.
        # LanguageGenerator.randomizer.shuffle_string(aileen_object._language)
        if is_positive:
            return ' '.join(aileen_object._language)
        else:
            return ' '.join(LanguageGenerator.generate_negative_language_for_object(aileen_object, distractors))

    @staticmethod
    def generate_negative_language_for_object(target, distractors):
        colors = AileenObject.get_colors().keys()
        shapes = deepcopy(settings.SHAPE_SET)

        print distractors

        distractors_copy = deepcopy(distractors)
        distractors_copy.append(target)

        word_list = []

        for item1 in colors:
            for item2 in shapes:
                word_list.append([item1, item2])


        for obj in distractors_copy:
            for word in word_list:
                if word == obj._language:
                    word_list.remove(word)


        shuffle(word_list)
        return word_list[0]

    @staticmethod
    def generate_language_for_spatial_relation(arg1, arg2, relation):
        object_phrase1 = LanguageGenerator.generate_language_for_object(arg1)
        object_phrase2 = LanguageGenerator.generate_language_for_object(arg2)
        return object_phrase1 + " " + relation + " " + object_phrase2

    @staticmethod
    def generate_language_from_template(scene_objects, language_template):
        word_list = deepcopy(language_template)
        for i in range(0, len(word_list)):
            if "<" in word_list[i]:
                word_list[i] = LanguageGenerator.generate_language_for_object(scene_objects[word_list[i]], is_positive=True)
                print word_list[i]
        return LanguageGenerator.generate_string_from(word_list)

    @staticmethod
    def generate_string_from(word_list):
        string = ""
        for word in word_list:
            string = string + word + " "
        return str(string).rstrip()

    class Randomizer:

        def shuffle_string(self, string):
            random.shuffle(string)

    randomizer = Randomizer()
