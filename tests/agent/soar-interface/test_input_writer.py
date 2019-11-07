from agent.soar_interface.soar_agent import soar_agent

def test_interaction_to_input_link_writer():
    agent = soar_agent(None)
    iwriter = agent._input_writer
    iwriter._interaction = {'signal': 'lesson', 'content': 'test_content'}
    iwriter.write_interaction_dictionary_to_input_link()

    ilink = iwriter._interaction_link

    signal_child = ilink.GetChild(0)
    assert signal_child.GetAttribute() == 'signal'
    assert signal_child.GetValueAsString() == 'lesson'

    content_child = ilink.GetChild(1)
    assert content_child.GetAttribute() == 'content'
    assert content_child.GetValueAsString() == 'test_content'

    assert iwriter._interaction is None