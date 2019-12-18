from agent.soar_interface.soar_agent import soar_agent
import settings

def test_set_agent_params():
    agent_params = {
        'visual-concepts-param': 'soar-vtest',
        'spatial-concepts-param': 'soar-stest',
        'action-concepts-param': 'soar-atest',
        'preload-visual-concepts-param': 'true-test'
    }

    test_param_string = """sp {aileen*apply*init-agent*agent_params 
                    (state <s>    ^operator.name initialize-agent)
                    -->
                    (<s>    ^_params <p>)
                    (<p>    ^visual-concepts soar-vtest
                            ^spatial-concepts soar-stest
                            ^action-concepts soar-atest)
                    (<p>    ^preload-visual-concepts true-test)
                    (<p>    ^relevant-percept-set <rps>)
                    (<rps>    ^type color shape id_string)
                    }"""

    agent = soar_agent(world_server=None, headless=True, agent_params=agent_params)
    agent.shutdown()
    with open(settings.AGENT_PARAM_RUNTIME_FILE, 'r') as agent_params_file:
        param_string = agent_params_file.read()
    assert param_string == test_param_string
