def translate_to_soar_structure(parses, parses_link):
    for parse in parses:
        parse_link = parses_link.CreateIdWME("parse")
        parse_item = parse[0]
        if parse_item == 'action':
            parse_item = translate_action_to_soar_structure(parse, parse_link)
        if parse_item == 'obj':  ### function is partially written, will only write obj parses to Soar
            parse_item = translate_obj_to_soar_structure(parse, parse_link)
        if isinstance(parse_item, list) and parse_item[0] == 'rel':
            parse_item = translate_rel_to_soar_structure(parse_item, parse_link)

def translate_action_to_soar_structure(parse, parse_link):
    action_ref_link = parse_link.CreateIdWME("action-ref")
    i = 1
    tag_string = ""
    while not isinstance(parse[i], list):
        tag_string = tag_string + parse[i]
        i = i + 1
    action_ref_link.CreateStringWME("tag", tag_string.rstrip())
    parse_item = parse[i]
    while isinstance(parse_item, list) and i in range(0, len(parse)):
        parse_item = parse[i]
        if parse_item[0] == 'obj':
            translate_obj_to_soar_structure(parse_item, action_ref_link)
        if parse_item[0] == 'rel':
            translate_rel_to_soar_structure(parse_item, action_ref_link)
        i = i + 1
    #return parse[i+1]


def translate_rel_to_soar_structure(parse, parse_link):
    rel_ref_link = parse_link.CreateIdWME("rel-ref")
    i = 1
    tag_string = ""
    parse_item = parse[i]
    while not isinstance(parse_item, list):
        tag_string = tag_string + parse_item
        i = i + 1
        parse_item = parse[i]
    rel_ref_link.CreateStringWME("tag", tag_string.rstrip())
    parse_item = parse[i]
    if isinstance(parse_item, list) and parse_item[0] == 'obj':
        translate_obj_to_soar_structure(parse_item, rel_ref_link)
    #return parse_item[i + 1]


def translate_obj_to_soar_structure(parse, parse_link):
    obj_ref_link = parse_link.CreateIdWME("obj-ref")
    i = 1
    while isinstance(parse[i], list):  ## property
        property = parse[i]
        assert property[0] == 'prop'
        prop_link = obj_ref_link.CreateIdWME('prop')
        prop_link.CreateStringWME('tag', property[1])
        i = i + 1
    obj_ref_link.CreateStringWME('tag', parse[i])
    if i + 1 >= len(parse):
        return None
    else:
        return parse[i+1]



