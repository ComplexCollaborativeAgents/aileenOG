import random

class LanguageGenerator:

    def __init__(self):
        pass


    @staticmethod
    def generate_language_for_object(aileen_object):
        object_phrase = ""
        random.shuffle(aileen_object._language)
        for visual_word in aileen_object._language:
            object_phrase += visual_word + " "
        return object_phrase

    @staticmethod
    def generate_language_for_spatial_relation(arg1, arg2, relation):
        object_phrase1 = LanguageGenerator.generate_language_for_object(arg1)
        object_phrase2 = LanguageGenerator.generate_language_for_object(arg2)
        return object_phrase1 + relation + " " + object_phrase2

    @staticmethod
    def generate_language_for_action(word_list):
        string = ""
        for word in word_list:
            string = string + word + " "
        return string