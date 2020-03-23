from instructor.curriculum_on_rails import CurriculumOnRails
from results_helper import ResultsHelper
from log_config import logging
import xmlrpclib
import settings
from instructor.curriculum import Curriculum


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


if __name__ == '__main__':
    settings.DO_RECORD = True
    ResultsHelper.reset_results_file('visual-word-learning-run1')
    lesson_number = 0

    world = create_connection_with_aileen_world()
    agent = create_connection_with_aileen_agent()

    rails = CurriculumOnRails("visual-word")
    lessons = rails.generate_inform_training_gamut()
    exams = rails.generate_verify_testing_gamut()[0:5]

    for lesson in Curriculum(lessons):
        ResultsHelper.write_lesson_number_to_results_file(lesson_number)

        lesson_object = lesson['object']

        scene_acknowledgement = world.set_scene(
            {'configuration': lesson['scene'], 'label': lesson['interaction']['content']})

        agent_response = agent.process_interaction(lesson['interaction'])

        evaluation = lesson_object.evaluate_agent_response(agent_response)
        agent_response = agent.process_interaction(evaluation)
        lesson_number = lesson_number + 1

        score = 0
        for exam in Curriculum(exams):
            exam_object = exam['object']
            scene_acknowledgement = world.set_scene({'configuration': exam['scene'], 'label': exam['interaction']['content']})
            agent_response = agent.process_interaction(exam['interaction'])

            if agent_response['status'] == 'success':
                score = score + 1

            evaluation = exam_object.evaluate_agent_response(agent_response)
            agent_response = agent.process_interaction(evaluation)

        ResultsHelper.record_generality_performance_score(score)





