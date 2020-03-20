import settings

class ResultsHelper:
    @staticmethod
    def reset_results_file(title):
        with open(settings.RESULTS_FILE_PATH, "w") as myfile:
            myfile.write(title)

    @staticmethod
    def write_lesson_number_to_results_file(lesson_number):
        with open(settings.RESULTS_FILE_PATH, "a") as myfile:
            myfile.write("\n"+str(lesson_number)+",")

    @staticmethod
    def record_processing_phase(phase_string):
        with open(settings.RESULTS_FILE_PATH, "a") as myfile:
            myfile.write(phase_string+",")
