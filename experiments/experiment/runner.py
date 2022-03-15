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


def create_connection_with_aileen_agent(agent_port):
    url = 'http://{}:{}'.format(settings.AGENT_HOST, agent_port)
    server = xmlrpclib.ServerProxy(url)
    logging.info("[aileen_instructor] :: created a connection with the agent: {}".format(url))
    return server


def parse():
    parser = argparse.ArgumentParser(description='run experiments with aileen')
    parser.add_argument('--type', help='run this type of experiment')
    parser.add_argument('--file', help='write results to this file')
    parser.add_argument('--concept', help='only generate examples of this concept')
    parser.add_argument('--episodes', help='number of tranining instances per concept')
    parser.add_argument('--exam_length', help='number of samples in exams')
    parser.add_argument('--distractors', help='number of distractors in exams')
    parser.add_argument('--agent_port', help='agent port')
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

    if arguments.concept:
        experiment_concept = arguments.concept
    else:
        experiment_concept = None

    if arguments.episodes:
        num_episodes_per_concept = int(arguments.episodes)
    else:
        num_episodes_per_concept = 1

    if arguments.exam_length:
        exam_length = int(arguments.exam_length)
    else:
        exam_length = 1

    if arguments.distractors:
        distractors = int(arguments.distractors)
    else:
        distractors = 0

    if arguments.agent_port:
        agent_port = arguments.agent_port
    else:
        agent_port = settings.AGENT_PORT

    lesson_number = 0

    world = create_connection_with_aileen_world()
    agent = create_connection_with_aileen_agent(agent_port)

    ResultsHelper.reset_results_file()


    # prep the experiments with a set of inform-only lessons
    # added by will, strictly for testing purposes
    # normally we can learn a concept in one shot, but with continuous stuff
    # we need a few examples already before we can learn a pdf
    # study_mod = Generator(experiment_type, experiment_concept, 3, exam_length, distractors)
    # study_lessons = study_mod.generate_inform_training_gamut()
    # for lesson in Curriculum(study_lessons):
    #     lesson_object = lesson['object']
    #     score, lesson_content = lesson_object.administer_lesson(world, agent)



    rails = Generator(experiment_type, experiment_concept, num_episodes_per_concept, exam_length, distractors)
    print('GENERATING LESSONS')
    lessons = rails.generate_inform_training_gamut()
    print('GENERTING G')
    g_exams = rails.generate_verify_testing_gamut_generality()
    print('GENERATING S')
    s_exams = rails.generate_verify_testing_gamut_specificity()

    logging.debug("[runner] :: lessons \n{}\n\n".format(lessons))
    logging.debug("[runner] :: g exams \n{}\n\n".format(g_exams))
    logging.debug("[runner] :: s exams \n{}".format(s_exams))


    for lesson in Curriculum(lessons):
        ResultsHelper.write_lesson_number_to_results_file(lesson_number)
        lesson_object = lesson['object']
        logging.debug('\n\n\n\nRunning Inform lesson')
        score, lesson_content = lesson_object.administer_lesson(world, agent)
        ResultsHelper.record_content(lesson_content)
        lesson_number = lesson_number + 1
        if g_exams is not None:
            score = 0
            for exam in Curriculum(g_exams):
                logging.debug('\n\n\n\nRunning generality exam')
                exam_object = exam['object']
                e_score, content = exam_object.administer_lesson(world, agent)
                logging.debug("[runner] :: g score is {}".format(e_score))
                score = score + e_score
            logging.debug("[runner] :: total g score is {}".format(score))
            ResultsHelper.record_performance_score(score)
        if s_exams is not None:
            score = 0
            for exam in Curriculum(s_exams):
                logging.debug('\n\n\n\nRunning specificity exam')
                exam_object = exam['object']
                e_score, content = exam_object.administer_lesson(world, agent)
                logging.debug("[runner] :: s score is {}".format(e_score))
                score = score + e_score
            logging.debug("[runner] :: total s score is {}".format(score))
            ResultsHelper.record_performance_score(score)

    ResultsHelper.copy_results_file(results_file)
