from agent.log_config import logging

def process_concept_learner_request(commandId, concept_learner):
    logging.debug("[output_reader] :: processing concept_learner command")
    for i in range(0, commandId.GetNumberChildren()):
        cl_child = commandId.GetChild(i).ConvertToIdentifier()
        if cl_child:
            if cl_child.GetAttribute() == "store":
                return process_store_command(cl_child, concept_learner)
            else:
                if cl_child.GetAttribute() == "query":
                    return process_query_command(cl_child, concept_learner)
                else:
                    if cl_child.GetAttribute() == "project":
                        return process_project_command(cl_child, concept_learner)
                    else:
                        if cl_child.GetAttribute() == "create":
                            return process_create_concept_command(cl_child, concept_learner)
                        else:
                            logging.error("[output_reader] :: concept learner does not implement this command")

def process_create_concept_command(create_command_id, concept_learner):
    request={}
    added_name = None
    added_type = None
    logging.debug("[output_reader] :: processing create-concept command")
    for i in range(0, create_command_id.GetNumberChildren()):
        child_id = create_command_id.GetChild(i)
        if child_id.GetAttribute() == "name":
            request['name'] = child_id.GetValueAsString()
            added_name = True
        if child_id.GetAttribute() == "type":
            request['type'] = child_id.GetValueAsString()
            added_type = True
    if added_name and added_type:
        response = concept_learner.create_new_concept(request)
        logging.debug("[concept-learner-helper] :: response from concept memory {}".format(response))
        return {'status': 'success', 'gpool': response['gpool']}
    else:
        logging.error("[concept-learner-helper] :: create concept command is incompletely specified")
        return {'status':'failure'}


def process_store_command(store_command_id, concept_learner):
    request = {}
    logging.debug("[concept_learner_helper] :: processing store command")
    added_facts = None
    added_concept = None
    added_context = None

    for i in range(0, store_command_id.GetNumberChildren()):
        child_id = store_command_id.GetChild(i)
        if child_id.GetAttribute() == "facts":
            request['facts'] = translate_soar_facts_to_tuple_list(child_id.ConvertToIdentifier())
            added_facts = True
        if child_id.GetAttribute() == "concept":
            request['concept'] = child_id.GetValueAsString()
            added_concept = True
        if child_id.GetAttribute() == "context":
            request['context'] = "episode{}".format(child_id.GetValueAsString())
            added_context = True

    if added_facts and added_concept and added_context:
        logging.debug("[concept-learner-helper] :: requesting concept memory {}".format(request))
        response = concept_learner.store(request)
        logging.debug("[concept-learner-helper] :: response from concept memory {}".format(response))
        return {'status': 'success'}
    else:
        logging.error("[output_reader] :: incomplete store command")
        return {'status': 'failure'}

def process_project_command(project_command_id):
    pass

def process_query_command(query_command_id, concept_learner):
    request = {}
    logging.debug("[concept_learner_helper] :: processing query command")

    added_facts = None
    added_pattern = None

    for i in range(0, query_command_id.GetNumberChildren()):
        child_id = query_command_id.GetChild(i)
        if child_id.GetAttribute() == "facts":
            request['facts'] = translate_soar_facts_to_tuple_list(child_id.ConvertToIdentifier())
            added_facts = True
        if child_id.GetAttribute() == "pattern":
            request['pattern'] = translate_soar_fact_to_tuple(child_id.ConvertToIdentifier())
            added_pattern = True

    if added_facts and added_pattern:
        logging.debug("[concept_learner_helper] :: request concept memory {}".format(request))
        response = concept_learner.query(request)
        logging.debug("[concept_learner_helper] :: response from concept memory {}".format(response))
        return {'status': 'success', 'matches': response['matches']}
    else:
        logging.error("[output_reader] :: incomplete query command. facts:{}, pattern:{}".format(added_facts, added_pattern))
        return {'status': 'failure'}


def translate_soar_facts_to_tuple_list(facts_id):
    facts = []
    for i in range(0, facts_id.GetNumberChildren()):
        fact_id = facts_id.GetChild(i).ConvertToIdentifier()
        fact_tuple = translate_soar_fact_to_tuple(fact_id)
        facts.append(fact_tuple)
    return facts


def translate_soar_fact_to_tuple(fact_id):
    fact_tuple = [None, None, None]
    for j in range(0, fact_id.GetNumberChildren()):
        child = fact_id.GetChild(j)
        if child.GetAttribute() == 'lfirst':
            fact_tuple[0] = child.GetValueAsString()
        if child.GetAttribute() == 'lsecond':
            fact_tuple[1] = child.GetValueAsString()
        if child.GetAttribute() == 'lthird':
            fact_tuple[2] = child.GetValueAsString()
    return fact_tuple

