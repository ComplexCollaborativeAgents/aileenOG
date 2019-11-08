from agent.soar_interface.soar_agent import soar_agent

def test_writing_interaction_to_input_link():
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


def test_writing_language_to_input_link_obj():
    agent = soar_agent(None)
    iwriter = agent._input_writer
    iwriter._language = {'parse': [['obj', ['prop', 'blue'], 'box']]}
    iwriter.write_language_to_input_link()

    llink = iwriter._language_link.ConvertToIdentifier()

    # print llink.GetValueAsString()
    parses_link = llink.GetChild(0).ConvertToIdentifier()
    assert parses_link.GetAttribute() == 'parses'

    parse_link = parses_link.GetChild(0).ConvertToIdentifier()
    assert parse_link.GetAttribute() == 'parse'

    for i in range(0, parses_link.GetNumberChildren()):
         child = parses_link.GetChild(i)
         if child.GetAttribute() == 'tag':
             assert child.GetValueAsString == 'box'
         if child.GetAttribute == 'prop':
             childId = child.ConvertToIdentifier()
             assert childId.GetChild(0).GetAttribute() == 'tag'
             assert childId.GetChild(0).GetAttribute() == 'blue'

    assert iwriter._language is None