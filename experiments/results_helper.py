import settings


class ResultsHelper:
    def __init__(self):
        pass

    do_record = True
    gfilename = settings.RUN_DATA_FILE_PATH

    @staticmethod
    def set_do_record(state):
        ResultsHelper.do_record = state

    @staticmethod
    def reset_results_file(title):
        if ResultsHelper.do_record:
            with open(ResultsHelper.gfilename, "w") as myfile:
                myfile.write(title)

    @staticmethod
    def write_lesson_number_to_results_file(lesson_number):
        if ResultsHelper.do_record:
            with open(ResultsHelper.gfilename, "a") as myfile:
                myfile.write("\n" + str(lesson_number) + ",")

    @staticmethod
    def record_processing_phase(phase_string):
        if ResultsHelper.do_record:
            with open(ResultsHelper.gfilename, "a") as myfile:
                myfile.write(phase_string + ",")

    @staticmethod
    def record_generality_performance_score(score):
        if ResultsHelper.do_record:
            with open(ResultsHelper.gfilename, "a") as myfile:
                myfile.write(str(score))
