from experiments.generator import Generator
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
    parser.add_argument('--type', help ='run this type of experiment')
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
        scene_acknowledgement = world.set_scene(
            {'configuration': lesson['scene'], 'label': lesson['interaction']['content']})
        agent_response = agent.process_interaction(lesson['interaction'])
        evaluation = lesson_object.evaluate_agent_response(agent_response)
        agent_response = agent.process_interaction(evaluation)
        lesson_number = lesson_number + 1

        if g_exams is not None:
            score = 0
            for exam in Curriculum(g_exams[0:5]):
                exam_object = exam['object']
                scene_acknowledgement = world.set_scene({'configuration': exam['scene'], 'label': exam['interaction']['content']})
                agent_response = agent.process_interaction(exam['interaction'])
                evaluation = exam_object.evaluate_agent_response(agent_response)
                score = score + evaluation['score']
                agent_response = agent.process_interaction(evaluation)
            ResultsHelper.record_generality_performance_score(score)
        if s_exams is not None:
            score = 0
            for exam in Curriculum(s_exams[0:5]):
                exam_object = exam['object']
                scene_acknowledgement = world.set_scene(
                    {'configuration': exam['scene'], 'label': exam['interaction']['content']})
                agent_response = agent.process_interaction(exam['interaction'])
                evaluation = exam_object.evaluate_agent_response(agent_response)
                score = score + evaluation['score']
                agent_response = agent.process_interaction(evaluation)
            ResultsHelper.record_generality_performance_score(score)

    # specificity_tests = rails.generate_verify_testing_gamut_specificity()
    # print len(specificity_tests)
    # if specificity_tests is not None:
    #     score = 0
    #     for test in Curriculum(specificity_tests):
    #         test_object = test['object']
    #         scene_acknowledgement = world.set_scene(
    #             {'configuration': test['scene'], 'label': test['interaction']['content']})
    #         agent_response = agent.process_interaction(test['interaction'])
    #         evaluation = test_object.evaluate_agent_response(agent_response)
    #         score = score + evaluation['score']
    #         agent_response = agent.process_interaction(evaluation)
    #     ResultsHelper.record_specificity_performance_score(score)

    ResultsHelper.copy_results_file(results_file)