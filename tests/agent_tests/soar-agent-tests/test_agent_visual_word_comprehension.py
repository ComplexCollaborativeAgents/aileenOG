from agent.soar_interface.soar_agent import SoarAgent
from agent.soar_interface.soar_agent import update
import settings

# class TestServer(object):
#     def __init__(self, object_list):
#         self._object_list = object_list
#         pass
#
#     def get_all(self):
#         return {'objects': self._object_list}
#
#

# def test_agent_visual_word_comprehension_known_concepts_single_object_on_scene_no_concept_memory():
#     agent = SoarAgent(None, headless=True)
#     iwriter = agent._input_writer
#
#     object_list = [
#             {
#                 'color': 'cv_red', 'shape': 'cv_sphere', 'texture': 't_',
#                 'id_name': '6a43b30f-3e84-49cd-85ef-4d62bc773d76',
#                 'held': 'false',
#                 'bounding_box': [0.685001719931, 0.39980379946355843, -0.0109654541087,
#                                  0.7850017199310001, 0.4998037994635584, 0.0890345458913],
#                 'position': [0.735001719931, 0.4498037994635584, 0.0390345458913],
#                 'id': 409,
#                 'id_string': "object409"
#             }
#         ]
#
#     iwriter.add_objects_to_working_memory(object_list)
#
#     iwriter._interaction = {'content': 'red sphere ', 'signal': 'verify'}
#     iwriter.write_interaction_dictionary_to_input_link()
#     while agent._agent.GetNumberCommands() == 0:
#         agent._agent.RunSelf(1)
#     assert agent._agent.GetNumberCommands() == 1
#     commandID = agent._agent.GetCommand(0)
#     assert commandID.GetAttribute() == 'language'
#     assert commandID.GetNumberChildren() == 1
#     parse_command = commandID.GetChild(0)
#     assert parse_command.GetAttribute() == 'parse-content'
#     assert parse_command.GetValueAsString() == 'red sphere '
#     commandID.AddStatusComplete()
#     agent._agent.RunSelf(1)
#     assert agent._agent.GetNumberCommands() == 0
#
#     iwriter._language = {'parses': [['obj', ['prop', 'red'], 'sphere']]}
#     iwriter.write_language_to_input_link()
#     while agent._agent.GetNumberCommands() == 0:
#         agent._agent.RunSelf(1)
#     assert agent._agent.GetNumberCommands() == 1
#     commandID = agent._agent.GetCommand(0)
#     assert commandID.GetAttribute() == 'interaction'
#     assert commandID.GetNumberChildren() == 1
#     parse_command = commandID.GetChild(0)
#     assert parse_command.GetAttribute() == 'response'
#     assert parse_command.GetValueAsString() == 'success'
#     commandID.AddStatusComplete()
#     agent._agent.RunSelf(1)
#     assert agent._agent.GetNumberCommands() == 0
#
#     iwriter._interaction = {'signal': 'correct'}
#     iwriter.write_interaction_dictionary_to_input_link()
#     while agent._agent.GetNumberCommands() == 0:
#         agent._agent.RunSelf(1)
#     assert agent._agent.GetNumberCommands() == 1
#     commandID = agent._agent.GetCommand(0)
#     assert commandID.GetAttribute() == 'interaction'
#     assert commandID.GetNumberChildren() == 1
#     parse_command = commandID.GetChild(0)
#     assert parse_command.GetAttribute() == 'response'
#     assert parse_command.GetValueAsString() == 'ok'
#     commandID.AddStatusComplete()
#     agent._agent.RunSelf(1)
#     assert agent._agent.GetNumberCommands() == 0
#
#     agent.stop()
#     agent.shutdown()

# def test_visual_word_learning_single_object_analogy_concept_learner():
#     object_list = [
#         {
#             'color': 'CVRed', 'shape': 'CVSphere', 'texture': 't_',
#             'id_name': '6a43b30f-3e84-49cd-85ef-4d62bc773d76',
#             'held': 'false',
#             'bounding_box': [0.685001719931, 0.39980379946355843, -0.0109654541087,
#                              0.7850017199310001, 0.4998037994635584, 0.0890345458913],
#             'position': [0.735001719931, 0.4498037994635584, 0.0390345458913],
#             'id': 409,
#             'id_string': "object409"
#         }
#     ]
#
#     server = TestServer(object_list)
#     agent_params = {
#         'visual-concepts-param': 'external',
#         'spatial-concepts-param': 'soar',
#         'action-concepts-param': 'soar',
#         'preload-visual-concepts-param': 'false'
#     }
#
#     agent = SoarAgent(world_server=server, headless=False, kernel_port=40000, agent_params=agent_params)
#     iwriter = agent._input_writer
#     oreader = agent._output_reader
#
#     agent._agent.RunSelf(20)
#
#     assert agent._agent.GetNumberCommands() == 0
#
#     iwriter.process_interaction({'content': 'red sphere ', 'signal': 'verify'})
#     iwriter.generate_input()
#     agent._agent.RunSelf(1)
#     assert agent._agent.GetNumberCommands() == 0
#     run_agent_until_next_output(agent)
#
#     ## 1. parse the interaction command
#     assert agent._agent.GetNumberCommands() == 1
#     commandID = agent._agent.GetCommand(0)
#     assert commandID.GetAttribute() == 'language'
#     assert commandID.GetNumberChildren() == 1
#     parse_command = commandID.GetChild(0)
#     assert parse_command.GetAttribute() == 'parse-content'
#     assert parse_command.GetValueAsString() == 'red sphere '
#
#     iwriter.generate_input()
#     agent._agent.RunSelf(1)
#     assert agent._agent.GetNumberCommands() == 0
#     run_agent_until_next_output(agent)
#     commandID = agent._agent.GetCommand(0)
#     assert commandID.GetAttribute() == 'concept-memory'
#
#     # run_agent_until_next_output(agent)
#     # commandID = agent._agent.GetCommand(0)
#     #
#     #
#     #
#     # run_agent_until_next_output(agent)
#     # commandID = agent._agent.GetCommand(0)
#     # assert commandID.GetAttribute() == 'concept-memory'
#
#
#     #
#     # run_agent_until_next_output(agent)
#     # commandID = agent._agent.GetCommand(0)
#     # assert commandID.GetAttribute() == 'concept-memory'
#     #
#     # run_agent_until_next_output(agent)
#     # commandID = agent._agent.GetCommand(0)
#     # assert commandID.GetAttribute() == 'concept-memory'
#     #
#     # run_agent_until_next_output(agent)
#     # commandID = agent._agent.GetCommand(0)
#     # assert commandID.GetAttribute() == 'concept-memory'
#     #
#     # run_agent_until_next_output(agent)
#     # commandID = agent._agent.GetCommand(0)
#     # assert commandID.GetAttribute() == 'concept-memory'
#     #
#     # run_agent_until_next_output(agent)
#     # commandID = agent._agent.GetCommand(0)
#     # assert commandID.GetAttribute() == 'concept-memory'
#
#
#     #run_agent_until_next_output(agent)
#     # commandID = agent._agent.GetCommand(0)
#     # assert commandID.GetAttribute() == 'interaction'
#     # assert commandID.GetNumberChildren() == 1
#     # parse_command = commandID.GetChild(0)
#     # assert parse_command.GetAttribute() == 'response'
#     # assert parse_command.GetValueAsString() == 'success'
#
#
#
# def run_agent_until_next_output(agent):
#     agent._input_writer.generate_input()
#     agent._agent.RunSelf(1)
#     while agent._agent.GetNumberCommands() == 0:
#         agent._agent.RunSelf(1)
#
# def test_update(mid, this_agent, agent, message):
#     if this_agent._agent.GetNumberCommands() > 0:
#         print("output commands")
#         exit()
#     update(mid, this_agent, agent, message)
#
# def test_agent_update():
#     object_list = [
#         {
#             'color': 'CVRed', 'shape': 'CVSphere', 'texture': 't_',
#             'id_name': '6a43b30f-3e84-49cd-85ef-4d62bc773d76',
#             'held': 'false',
#             'bounding_box': [0.685001719931, 0.39980379946355843, -0.0109654541087,
#                              0.7850017199310001, 0.4998037994635584, 0.0890345458913],
#             'position': [0.735001719931, 0.4498037994635584, 0.0390345458913],
#             'id': 409,
#             'id_string': "object409"
#         }
#     ]
#
#     server = TestServer(object_list)
#     agent_params = {
#         'visual-concepts-param': 'external',
#         'spatial-concepts-param': 'soar',
#         'action-concepts-param': 'soar',
#         'preload-visual-concepts-param': 'false'
#     }
#
#     agent = SoarAgent(world_server=server, headless=True, agent_params=agent_params)
#     #agent.register_output_callback(test_update, agent)
#     agent._agent.RunSelf(10)
#     agent.start()
#     agent.stop()
#     assert False