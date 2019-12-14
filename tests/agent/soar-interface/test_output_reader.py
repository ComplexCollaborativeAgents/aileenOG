from agent.soar_interface.soar_agent import soar_agent

def test_process_language_command():
    agent = soar_agent(None)
    oreader = agent._output_reader
    olink = agent._input_link

    language_link = olink.CreateIdWME("language")
    language_link.CreateStringWME("parse-content", "blue box ")
    oreader.process_language_command(language_link)

    assert agent._input_writer._language == {'parses': [['obj', ['prop', 'blue'], 'box']]}
    agent.stop()
    agent.quit()