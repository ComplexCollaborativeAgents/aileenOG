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
    def generate_language_for_spatial_relation(aileen_object1, spatial_relationship, aileen_object2):
        object_phrase1 = LanguageGenerator.generate_language_for_object(aileen_object1)
        object_phrase2 = LanguageGenerator.generate_language_for_object(aileen_object2)

        return object_phrase1 + spatial_relationship + " " + object_phrase2
