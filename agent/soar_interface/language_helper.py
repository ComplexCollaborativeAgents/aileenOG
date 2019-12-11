def translate_to_soar_structure(parses, parses_link):
    for parse in parses:
        parse_link = parses_link.CreateIdWME("parse")
        parse_item = parse[0]
        if parse_item == 'obj':  ### function is partially written, will only write obj parses to Soar
            parse_item = translate_obj_to_soar_structure(parse, parse_link)
        if isinstance(parse_item, list) and parse_item[0] == 'rel':
            parse_item = translate_rel_to_soar_structure(parse_item, parse_link)

def translate_rel_to_soar_structure(parse_item, parse_link):
    rel_ref_link = parse_link.CreateIdWME("rel-ref")
    i = 1
    tag_string = ""
    while not isinstance(parse_item[i], list):
        tag_string = tag_string + parse_item[i]
        i = i + 1
    rel_ref_link.CreateStringWME("tag", tag_string.rstrip())
    if isinstance(parse_item[i], list) and parse_item[i][0] == 'obj':
        translate_obj_to_soar_structure(parse_item[i], rel_ref_link)
    return i + 1


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



