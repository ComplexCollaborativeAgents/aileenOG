from agent.soar_interface.soar_agent import SoarAgent
from agent.soar_interface.action_helper import place_object_in_configuration_with
import pytest


@pytest.mark.skip(reason='Something is causing a segfault')
def test_process_language_command():
    agent = SoarAgent(None)
    oreader = agent._output_reader
    olink = agent._input_link

    language_link = olink.CreateIdWME("language")
    language_link.CreateStringWME("parse-content", "blue box ")
    oreader.process_language_command(language_link)

    assert agent._input_writer._language == {'parses': [['obj', ['prop', 'blue'], 'box']]}
    agent.stop()
    agent.quit()

def test_generate_language_command():
    agent = SoarAgent(None)
    oreader = agent._output_reader
    olink = agent._input_link

    language_link = olink.CreateIdWME("language")
    cid = language_link.CreateIdWME("generate-content")
    oid = cid.CreateIdWME("object")
    oid.CreateStringWME("id", "ob432")
    oid.CreateStringWME("word", "blue")
    oid.CreateStringWME("word", "box")
    oreader.process_language_command(language_link)
    assert agent._input_writer._language == {'sentence': "blue box"}
    agent.stop()
    agent.quit()


def test_place_object_in_configuration_with():

    target =  {'zsize': '0.1', 'name': '673', 'xsize': '0.1'}
    reference =  {'zsize': '0.1', 'xpos': '0.506146', 'name': '679', 'zpos': '0.092', 'ypos': '0.449804', 'xsize': '0.1'}
    config =  [['s', '679', '673'], ['dc', '679', '673']]

    location = place_object_in_configuration_with(target, reference, config)
    print location