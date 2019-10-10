import random

class LanguageGenerator:

    def __init__(self):
        pass

    test_id = None  # Class variable for unit tests

    @staticmethod
    def generate_language_for_object(aileen_object):
        object_phrase = ""
        if LanguageGenerator.test_id == None:
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
    def generate_language_from_template(scene_objects, language_template):
        word_list = language_template
        for i in range(0, len(word_list)):
            if "<" in word_list[i]:
                word_list[i] = LanguageGenerator.generate_language_for_object(scene_objects[word_list[i]])
        return LanguageGenerator.generate_string_from(word_list)
    

    @staticmethod
    def generate_string_from(word_list):
        string = ""
        for word in word_list:
            string = string + word + " "
        return str(string).rstrip()