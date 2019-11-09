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
    iwriter._language = {'parses': [['obj', ['prop', 'blue'], 'box']]}
    iwriter.write_language_to_input_link()

    llink = iwriter._language_link.ConvertToIdentifier()

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

def test_qsr_input_writer():
    agent = soar_agent(None)
    iw = agent._input_writer
    objects = [{'orientation': [1.0, -5.75539615965681e-17, 3.38996313371214e-17, 5.75539615965681e-17, 1.0, 2.98427949019241e-17, -3.38996313371214e-17, -2.98427949019241e-17, 1.0], 'bounding_object': 'Box', 'held': 'false', 'bounding_box': [0.721, 0.3998037998119487, -0.249, 0.8210000000000001, 0.49980379981194867, -0.14900000000000002], 'position': [0.771, 0.4498037998119487, -0.199], 'id': 397}, {'orientation': [1.0, 4.8853319907279786e-17, -4.193655877514327e-17, -4.8853319907279786e-17, 1.0, -1.80524117148876e-16, 4.193655877514327e-17, 1.80524117148876e-16, 1.0], 'bounding_object': 'Cylinder', 'held': 'false', 'bounding_box': [0.369851, 0.39992295234206066, 0.067742, 0.46985099999999996, 0.49992295234206063, 0.167742], 'position': [0.419851, 0.44992295234206064, 0.117742], 'id': 403}]
    res = iw.create_qsrs(objects)
    assert len(res)==2
    assert len(res['403']['397']) == 2