from agent.soar_interface.soar_agent import SoarAgent


def test_writing_world_info_output_link():
    objects_list = [
        {
            'color': 'cv_blue', 'shape': 'cv_sphere', 'texture': 't_',
            'id_name': 'b781977d-031e-445e-8554-30e26f583bd9',
            'held': 'true',
            'bounding_box': [0.7202012373359999, 0.3998037994635584, -0.0672224341581,
                             0.820201237336, 0.49980379946355835, 0.0327775658419],
            'position': [0.770201237336, 0.44980379946355836, -0.0172224341581],
            'id': 415
        },
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

    agent = SoarAgent(None)
    iwriter = agent._input_writer

    iwriter.add_objects_to_working_memory(objects_list)

    wlink = iwriter._world_link

    assert wlink.GetNumberChildren() == 1

    objects_WME = wlink.GetChild(0)
    assert objects_WME.GetAttribute() == 'objects'
    objects_id = objects_WME.ConvertToIdentifier()

    assert objects_id.GetNumberChildren() == 2

    first_object = objects_id.GetChild(0)
    assert first_object.GetAttribute() == 'object'
    first_object_id = first_object.ConvertToIdentifier()
    first_object_asserts = []

    for i in range(0, first_object_id.GetNumberChildren()):
        child = first_object_id.GetChild(i)
        if child.GetAttribute() == 'id':
            assert child.GetValueAsString() == "415"
            first_object_asserts.append('id')
        if child.GetAttribute() == 'color':
            assert child.GetValueAsString() == "cv_blue"
            first_object_asserts.append('color')
        if child.GetAttribute() == 'shape':
            assert child.GetValueAsString() == "cv_sphere"
            first_object_asserts.append('shape')
        if child.GetAttribute() == 'id_name':
            assert child.GetValueAsString() == "b781977d-031e-445e-8554-30e26f583bd9"
            first_object_asserts.append('id_name')
        if child.GetAttribute() == 'held':
            assert child.GetValueAsString() == "true"
            first_object_asserts.append('held')
        # SM: currently these are not used by the Soar agent directly
        # if child.GetAttribute() == 'bounding_box':
        #     assert child.GetValueAsString() == [0.7202012373359999, 0.3998037994635584, -0.0672224341581,
        #                      0.820201237336, 0.49980379946355835, 0.0327775658419]
        #     first_object_asserts.append('bounding_box')
        # if child.GetAttribute() == 'position':
        #     assert child.GetValueAsString() == [0.7202012373359999, 0.3998037994635584, -0.0672224341581,
        #                      0.820201237336, 0.49980379946355835, 0.0327775658419]
        #     first_object_asserts.append('id_name')

    assert 'id' in first_object_asserts and 'color' in first_object_asserts and 'shape' in first_object_asserts and \
           'id_name' in first_object_asserts and 'held' in first_object_asserts

    second_object = objects_id.GetChild(1)
    assert second_object.GetAttribute() == 'object'
    second_object_id = second_object.ConvertToIdentifier()
    second_object_asserts = []

    for i in range(0, second_object_id.GetNumberChildren()):
        child = second_object_id.GetChild(i)
        if child.GetAttribute() == 'id':
            assert child.GetValueAsString() == "409"
            second_object_asserts.append('id')
        if child.GetAttribute() == 'color':
            assert child.GetValueAsString() == "cv_red"
            second_object_asserts.append('color')
        if child.GetAttribute() == 'shape':
            assert child.GetValueAsString() == "cv_sphere"
            second_object_asserts.append('shape')
        if child.GetAttribute() == 'id_name':
            assert child.GetValueAsString() == "6a43b30f-3e84-49cd-85ef-4d62bc773d76"
            second_object_asserts.append('id_name')
        if child.GetAttribute() == 'held':
            assert child.GetValueAsString() == "false"
            second_object_asserts.append('held')
        # SM: currently these are not used by the Soar agent directly
        # if child.GetAttribute() == 'bounding_box':
        #     assert child.GetValueAsString() == [0.7202012373359999, 0.3998037994635584, -0.0672224341581,
        #                      0.820201237336, 0.49980379946355835, 0.0327775658419]
        #     first_object_asserts.append('bounding_box')
        # if child.GetAttribute() == 'position':
        #     assert child.GetValueAsString() == [0.7202012373359999, 0.3998037994635584, -0.0672224341581,
        #                      0.820201237336, 0.49980379946355835, 0.0327775658419]
        #     first_object_asserts.append('id_name')

    assert 'id' in second_object_asserts and 'color' in second_object_asserts and 'shape' in second_object_asserts and \
           'id_name' in second_object_asserts and 'held' in second_object_asserts

    agent.stop()
    agent.shutdown()


def test_writing_interaction_to_input_link():
    agent = SoarAgent(None)
    iwriter = agent._input_writer
    iwriter._interaction = {'signal': 'verify', 'content': 'test_content'}
    iwriter.write_interaction_dictionary_to_input_link()

    ilink = iwriter._interaction_link

    message_child_WME = ilink.GetChild(0)
    assert message_child_WME.GetAttribute() == 'message'

    message_child = message_child_WME.ConvertToIdentifier()

    signal_child = message_child.GetChild(0)
    assert signal_child.GetAttribute() == 'signal'
    assert signal_child.GetValueAsString() == 'verify'

    content_child = message_child.GetChild(1)
    assert content_child.GetAttribute() == 'content'
    assert content_child.GetValueAsString() == 'test_content'

    assert iwriter._interaction is None
    agent.stop()
    agent.shutdown()


def test_writing_language_to_input_link_obj():
    agent = SoarAgent(None)
    iwriter = agent._input_writer
    iwriter._language = {'parses': [['obj', ['prop', 'blue'], 'box']]}
    iwriter.write_language_to_input_link()

    llink = iwriter._language_link

    language_link_WME = llink.GetChild(0)
    assert language_link_WME.GetAttribute() == "language"

    language_link = language_link_WME.ConvertToIdentifier()

    parses_link = language_link.GetChild(0).ConvertToIdentifier()
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
    agent.stop()
    agent.shutdown()


def test_qsr_input_writer():
    agent = SoarAgent(None)
    iw = agent._input_writer
    objects = [{'orientation': [1.0, -5.75539615965681e-17, 3.38996313371214e-17, 5.75539615965681e-17, 1.0,
                                2.98427949019241e-17, -3.38996313371214e-17, -2.98427949019241e-17, 1.0],
                'bounding_object': 'Box', 'held': 'false',
                'bounding_box': [0.721, 0.3998037998119487, -0.249, 0.8210000000000001, 0.49980379981194867,
                                 -0.14900000000000002], 'position': [0.771, 0.4498037998119487, -0.199], 'id': 397}, {
                   'orientation': [1.0, 4.8853319907279786e-17, -4.193655877514327e-17, -4.8853319907279786e-17, 1.0,
                                   -1.80524117148876e-16, 4.193655877514327e-17, 1.80524117148876e-16, 1.0],
                   'bounding_object': 'Cylinder', 'held': 'false',
                   'bounding_box': [0.369851, 0.39992295234206066, 0.067742, 0.46985099999999996, 0.49992295234206063,
                                    0.167742], 'position': [0.419851, 0.44992295234206064, 0.117742], 'id': 403}]
    res = iw.create_qsrs(objects)
    assert len(res) == 2
    assert len(res['403']['397']) == 2
    agent.stop()
    agent.shutdown()
