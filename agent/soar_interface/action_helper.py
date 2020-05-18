import settings
from log_config import logging

try:
    from qsrlib_io.world_trace import Object_State, World_State
    from qsrlib import qsr_realization
    from qsrlib.qsr_realization import compute_region_for_relations
    from shapely.geometry import Point, box
except ImportError:
    logging.fatal("[aileen_scene] :: cannot find spatial reasoning library")
    exit()


def process_pick_command(commandID):
    action_dict = {}
    for i in range(0, commandID.GetNumberChildren()):
        child = commandID.GetChild(i)
        if child.GetAttribute() == 'name':
            action_dict['name'] = child.GetValueAsString()
        if child.GetAttribute() == 'id':
            action_dict['id'] = child.GetValueAsString()
    return action_dict


def process_place_command(commandID):
    action_dict = {}
    config = []
    reference = {}
    target = {}
    for i in range(0, commandID.GetNumberChildren()):
        child = commandID.GetChild(i)
        if child.GetAttribute() == 'name':
            action_dict['name'] = child.GetValueAsString()
        if child.GetAttribute() == 'relation':
            config.append(process_relation_attribute(child.ConvertToIdentifier()))
        if child.GetAttribute() == 'reference':
            reference = process_object(child.ConvertToIdentifier())
        if child.GetAttribute() == 'target':
            target = process_object(child.ConvertToIdentifier())

    logging.debug("[action_helper] :: target is {}".format(target))
    logging.debug("[action_helper] :: reference is {}".format(reference))
    logging.debug("[action_helper] :: configuration definition is {}".format(config))

    action_dict['location'] = place_object_in_configuration_with(target, reference, config)
    return action_dict


def process_object(objectID):
    object = {}
    for i in range(0, objectID.GetNumberChildren()):
        child = objectID.GetChild(i)
        if child.GetAttribute() == 'id':
            object['name'] = child.GetValueAsString()
        if child.GetAttribute() == 'xpos':
            object['xpos'] = child.GetValueAsString()
        if child.GetAttribute() == 'ypos':
            object['ypos'] = child.GetValueAsString()
        if child.GetAttribute() == 'zpos':
            object['zpos'] = child.GetValueAsString()
        if child.GetAttribute() == 'xsize':
            object['xsize'] = child.GetValueAsString()
        if child.GetAttribute() == 'zsize':
            object['zsize'] = child.GetValueAsString()
    return object


def process_relation_attribute(relationID):
    rel_def = [None, None, None]
    for i in range(0, relationID.GetNumberChildren()):
        child = relationID.GetChild(i)
        if child.GetAttribute() == 'qsr':
            rel_def[0] = child.GetValueAsString()
        if child.GetAttribute() == 'root':
            rel_def[1] = child.GetValueAsString()
        if child.GetAttribute() == 'target':
            rel_def[2] = child.GetValueAsString()
    return rel_def


def process_point_command(commandID):
    action_dict = {}
    for i in range(0, commandID.GetNumberChildren()):
        child = commandID.GetChild(i)
        if child.GetAttribute() == 'name':
            action_dict['name'] = child.GetValueAsString()
        if child.GetAttribute() == 'id':
            action_dict['id'] = child.GetValueAsString()
    return action_dict


def place_object_in_configuration_with(target, reference, configuration_definition):
    logging.debug("[action_helper] :: target is {}".format(target))
    logging.debug("[action_helper] :: reference is {}".format(reference))
    logging.debug("[action_helper] :: configuration definition is {}".format(configuration_definition))


    table = box(settings.OBJECT_POSITION_MIN_X,
                settings.OBJECT_POSITION_MIN_Z,
                settings.OBJECT_POSITION_MAX_X,
                settings.OBJECT_POSITION_MAX_Z)

    found_target_object_position = None
    while found_target_object_position is None:
        world = World_State(0.0)
        qsr_reference_object = Object_State(name=str(reference['name']), timestamp=0,
                                            x=float(reference['xpos']),
                                            y=float(reference['zpos']),
                                            xsize=float(reference['xsize']),
                                            ysize=float(reference['zsize']))
        world.add_object_state(qsr_reference_object)
        logging.debug(
            "[aileen_scene] :: added reference object {} to QSRLib scene".format(str(reference['name'])))

        position = [0, 0, 0]
        qsr_target_object = Object_State(name=str(target['name']), timestamp=0,
                                         x=float(position[0]),
                                         y=float(position[2]),
                                         xsize=float(target['xsize']),
                                         ysize=float(reference['zsize']))

        try:
            found_target_object_position = qsr_realization.sample_position_from_region(
                compute_region_for_relations(world, configuration_definition, qsr_target_object, table))
            position = [found_target_object_position.x, settings.OBJECT_POSITION_MAX_Y, found_target_object_position.y]
        except (ValueError, AttributeError), e:
            logging.error(
                "[aileen_scene] :: cannot place {} in configuration with {}".format(target['name'], reference['name']))
            raise e
    return position
