from agent.soar_interface.soar_agent import soar_agent


def test_agent_visual_word_comprehension_known_concepts_single_object_on_scene():
    agent = soar_agent(None, headless=True)
    iwriter = agent._input_writer
    oreader = agent._output_reader
    agent.run_till_output()

    object_list = [
            {
                'color': 'cv_red', 'shape': 'cv_sphere', 'texture': 't_',
                'id_name': '6a43b30f-3e84-49cd-85ef-4d62bc773d76',
                'held': 'false',
                'bounding_box': [0.685001719931, 0.39980379946355843, -0.0109654541087,
                                 0.7850017199310001, 0.4998037994635584, 0.0890345458913],
                'position': [0.735001719931, 0.4498037994635584, 0.0390345458913],
                'id': 409
            }
        ]

    iwriter.add_objects_to_working_memory(object_list)
    iwriter._interaction = {'interaction': {'content': 'red sphere ', 'signal': 'verify'}}

    print agent._output_link

    assert agent._agent.GetNumberCommands() == 1

    # create corresponding instruction

    # create an agent

    # set semantic memory specifically to known items

    # run agent until output

    agent.stop()
    agent.shutdown()
