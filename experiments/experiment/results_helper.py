import settings
from shutil import copyfile


class ResultsHelper:
    def __init__(self):
        pass

    gfilename = settings.RUN_DATA_FILE_PATH
    create_concept_count = 0
    store_instance_count = 0

    @staticmethod
    def increase_create_concept_count():
        ResultsHelper.create_concept_count += 1

    @staticmethod
    def reset_create_concept_count():
        ResultsHelper.create_concept_count = 0

    @staticmethod
    def reset_store_instance_count():
        ResultsHelper.store_instance_count = 0

    @staticmethod
    def increase_store_instance_count():
        ResultsHelper.store_instance_count += 1

    @staticmethod
    def set_do_record(state):
        ResultsHelper.do_record = state

    @staticmethod
    def reset_results_file():
        open(ResultsHelper.gfilename, "w").close()

    @staticmethod
    def write_lesson_number_to_results_file(lesson_number):
        with open(ResultsHelper.gfilename, "a") as myfile:
            myfile.write("\n" + str(lesson_number) + ",")

    @staticmethod
    def record_processing_phase(phase_string):
        with open(ResultsHelper.gfilename, "a") as myfile:
            myfile.write(phase_string + ",")

    @staticmethod
    def record_performance_score(score):
        with open(ResultsHelper.gfilename, "a") as myfile:
            myfile.write(str(score) + ",")

    @staticmethod
    def record_content(content):
        with open(ResultsHelper.gfilename, "a") as myfile:
            myfile.write(content + ",")

    @staticmethod
    def copy_results_file(filename):
        copyfile(ResultsHelper.gfilename, filename)
