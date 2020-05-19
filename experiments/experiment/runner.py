from experiments.experiment.generator import Generator
from results_helper import ResultsHelper
from log_config import logging
import xmlrpclib
import settings
from instructor.curriculum import Curriculum

import argparse


def create_connection_with_aileen_world():
    url = 'http://{}:{}'.format(settings.WORLD_HOST, settings.WORLD_PORT)
    server = xmlrpclib.ServerProxy(url)
    logging.info("[aileen_instructor] :: created a connection with the world server at: {}".format(url))
    return server


def create_connection_with_aileen_agent():
    url = 'http://{}:{}'.format(settings.AGENT_HOST, settings.AGENT_PORT)
    server = xmlrpclib.ServerProxy(url)
    logging.info("[aileen_instructor] :: created a connection with the agent: {}".format(url))
    return server


def parse():
    parser = argparse.ArgumentParser(description='run experiments with aileen')
    parser.add_argument('--type', help='run this type of experiment')
    parser.add_argument('--file', help='write results to this file')
    return parser.parse_args()


if __name__ == '__main__':
    arguments = parse()

    if arguments.type:
        experiment_type = arguments.type
    else:
        experiment_type = "visual-word"

    if arguments.file:
        results_file = arguments.file
    else:
        results_file = None

    lesson_number = 0

    world = create_connection_with_aileen_world()
    agent = create_connection_with_aileen_agent()

    ResultsHelper.reset_results_file()

    rails = Generator(experiment_type)
    lessons = rails.generate_inform_training_gamut()
    g_exams = rails.generate_verify_testing_gamut_generality()
    s_exams = rails.generate_verify_testing_gamut_specificity()

    for lesson in Curriculum(lessons):
        ResultsHelper.write_lesson_number_to_results_file(lesson_number)
        lesson_object = lesson['object']
        score, lesson_content = lesson_object.administer_lesson(world, agent)
        ResultsHelper.record_content(lesson_content)
        lesson_number = lesson_number + 1
        if g_exams is not None:
            score = 0
            for exam in Curriculum(g_exams[0:5]):
                exam_object = exam['object']
                e_score, content = exam_object.administer_lesson(world, agent)
                score = score + e_score
            ResultsHelper.record_performance_score(score)
        if s_exams is not None:
            score = 0
            for exam in Curriculum(s_exams[0:5]):
                exam_object = exam['object']
                e_score, content = exam_object.administer_lesson(world, agent)
                score = score + e_score
            ResultsHelper.record_performance_score(score)

    ResultsHelper.copy_results_file(results_file)
