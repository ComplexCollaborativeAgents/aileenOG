from random import uniform
import settings
from shapely.geometry import box
from log_config import logging

try:
    from qsrlib_io.world_trace import Object_State, World_State
    from qsrlib import qsr_realization
    from qsrlib.qsr_realization import compute_region_for_relations, check_for_3d_qsrs
    from shapely.geometry import Point
except ImportError:
    logging.fatal("[aileen_scene] :: cannot find spatial reasoning library")
    exit()


class AileenScene:

    def __init__(self):
        self._objects = []

    def add_object(self, aileen_object):
        self._objects.append(aileen_object)

    def generate_scene_world_config(self):
        description = []
        for scene_object in self._objects:
            description.append(scene_object.get_object_description())
        return description

    @staticmethod
    def place_two_objects_in_configuration(target_object_name, reference_object_name, scene_objects, configuration_definition):
        translations = {}
        reference_object = scene_objects[reference_object_name]
        target_object = scene_objects[target_object_name]

        table = box(settings.OBJECT_POSITION_MIN_X,
                    settings.OBJECT_POSITION_MIN_Z,
                    settings.OBJECT_POSITION_MAX_X,
                    settings.OBJECT_POSITION_MAX_Z)
        found_target_object_position = None
        while found_target_object_position is None:
            world = World_State(0.0)

            new_reference_object_position = AileenScene.randomizer.get_random_position_on_table()

            qsr_reference_object = Object_State(name=str(reference_object_name), timestamp=0,
                                            x=new_reference_object_position[0],
                                            y=new_reference_object_position[2],
                                            z=new_reference_object_position[1],
                                            xsize=reference_object._width_x,
                                            ysize=reference_object._width_z,
                                            zsize=reference_object._height_y)

            world.add_object_state(qsr_reference_object)
            logging.debug("[aileen_scene] :: added reference object {} to QSRLib scene".format(str(reference_object_name)))
            translations[reference_object_name] = new_reference_object_position

            position = AileenScene.randomizer.get_random_position_on_table()
            qsr_target_object = Object_State(name=str(target_object_name), timestamp=0,
                                                 x=position[0],
                                                 y=position[2],
                                                 z=position[1],
                                                 xsize=target_object._width_x,
                                                 ysize=target_object._width_z,
                                                 zsize=target_object._height_y)

            try:
                region = compute_region_for_relations(world, configuration_definition, qsr_target_object, table)
                found_target_object_position = AileenScene.randomizer.sample_position_from_region(region)
                position = [found_target_object_position.x, settings.OBJECT_POSITION_TABLE_Y, found_target_object_position.y]
                #this will only overwrite if a 3d qsr is found
                position = AileenScene.randomizer.check_for_3d_qsrs(world, configuration_definition, qsr_target_object, position)
                logging.debug('[aileen_scene] :: found position for object: {}'.format(position))
                position[1] = AileenScene.bound_position_for_demo(qsr_reference_object.z, qsr_reference_object.zsize, position[1])
                logging.debug('[aileen_scene] :: bounded position: {}'.format(position))
                translations[target_object_name] = position
            except (ValueError, AttributeError):
                logging.error("[aileen_scene] :: Could not place target object!")
                pass
        return translations

    @staticmethod
    def bound_position_for_demo(z, dz, position_y, offset = .1):
        top_pos = z + dz/2 + offset
        highest = top_pos + 0.2
        if position_y < top_pos:
            return top_pos
        if position_y > highest:# print('dz {}'.format(dz))
        # print('two object widths sum {}'.format(data1.zsize/2 + data2.zsize/2))
            return highest
        return position_y


    @staticmethod
    def place_object_in_configuration_with(target_object_name, reference_object_name, scene_objects, configuration_definition):
        translations = {}
        reference_object = scene_objects[reference_object_name]
        target_object = scene_objects[target_object_name]

        table = box(settings.OBJECT_POSITION_MIN_X,
                    settings.OBJECT_POSITION_MIN_Z,
                    settings.OBJECT_POSITION_MAX_X,
                    settings.OBJECT_POSITION_MAX_Z)

        found_target_object_position = None
        while found_target_object_position is None:
            world = World_State(0.0)
            qsr_reference_object = Object_State(name=str(reference_object_name), timestamp=0,
                                                x=reference_object._translation[0],
                                                y=reference_object._translation[2],
                                                z=reference_object._translation[1],
                                                xsize=reference_object._width_x,
                                                ysize=reference_object._width_z,
                                                zsize=reference_object._height_y)
            world.add_object_state(qsr_reference_object)
            logging.debug(
                "[aileen_scene] :: added reference object {} to QSRLib scene".format(str(reference_object_name)))

            position = AileenScene.randomizer.get_random_position_on_table()
            qsr_target_object = Object_State(name=str(target_object_name), timestamp=0,
                                             x=position[0],
                                             y=position[2],
                                             z=position[1],
                                             xsize=target_object._width_x,
                                             ysize=target_object._width_z,
                                             zsize=target_object._height_y)

            try:
                found_target_object_position = AileenScene.randomizer.sample_position_from_region(
                    compute_region_for_relations(world, configuration_definition, qsr_target_object, table))
                position = [found_target_object_position.x, settings.OBJECT_POSITION_TABLE_Y,
                            found_target_object_position.y]
                position = AileenScene.randomizer.check_for_3d_qsrs(world, configuration_definition, qsr_target_object, position)
                logging.debug('[aileen_scene] :: found position for object: {}'.format(position))
                position[1] = AileenScene.bound_position_for_demo(qsr_reference_object.z, qsr_reference_object.zsize, position[1])
                logging.debug('[aileen_scene] :: bounded position: {}'.format(position))
                translations[target_object_name] = position
            except (ValueError, AttributeError), e:
                logging.error("[aileen_scene] :: cannot place {} in configuration with {}".format(target_object_name, reference_object_name))
                raise e
        return position

    @staticmethod
    def place_three_objects_in_configuration(target_object_name, first_reference_object_name, second_reference_object_name, scene_objects, configuration_definition):
        table = box(settings.OBJECT_POSITION_MIN_X,
                    settings.OBJECT_POSITION_MIN_Z,
                    settings.OBJECT_POSITION_MAX_X,
                    settings.OBJECT_POSITION_MAX_Z)

        logging
        first_reference_object = scene_objects[first_reference_object_name]
        second_reference_object = scene_objects[second_reference_object_name]
        target_object = scene_objects[target_object_name]

        configuration_definition_subset = AileenScene.subset_configuration_definition_for_object(target_object_name, configuration_definition)

        found_target_object_position = None

        while found_target_object_position is None:
            world = World_State(0.0)
            translations = AileenScene.place_two_objects_in_configuration(target_object_name=second_reference_object_name,
                                                                          reference_object_name=first_reference_object_name,
                                                                          scene_objects=scene_objects,
                                                                          configuration_definition=configuration_definition_subset)
            qsr_first_reference_object = Object_State(name=str(first_reference_object_name), timestamp=0,
                                                      x=translations[first_reference_object_name][0],
                                                      y=translations[first_reference_object_name][2],
                                                      z=translations[first_reference_object_name][1],
                                                      xsize=first_reference_object._width_x,
                                                      ysize=first_reference_object._width_z,
                                                      zsize=first_reference_object._height_y)
            world.add_object_state(qsr_first_reference_object)

            qsr_second_reference_object = Object_State(name=str(second_reference_object_name), timestamp=0,
                                                      x=translations[second_reference_object_name][0],
                                                      y=translations[second_reference_object_name][2],
                                                      z=translations[second_reference_object_name][1],
                                                      xsize=second_reference_object._width_x,
                                                      ysize=second_reference_object._width_z,
                                                      zsize=second_reference_object._height_y)
            world.add_object_state(qsr_second_reference_object)
            logging.debug("[aileen_scene] :: added reference objects {} and {} to QSRLib scene".format(first_reference_object_name, second_reference_object_name))

            position = AileenScene.randomizer.get_random_position_on_table()
            # position = AileenScene.place_object_in_configuration_with(target_object_name=target_object_name,
            #                                                           reference_object_name=second_reference_object_name,
            #                                                           scene_objects=scene_objects,
            #                                                           configuration_definition=configuration_definition)
            qsr_target_object = Object_State(name=str(target_object_name), timestamp=0,
                                             x=position[0],
                                             y=position[2],
                                             z=position[1],
                                             xsize=target_object._width_x,
                                             ysize=target_object._width_z,
                                             zsize=target_object._height_y)
            logging.error('[aileen_scene] :: Entering object placement three objs')
            try:
                found_target_object_position = AileenScene.randomizer.sample_position_from_region(
                    compute_region_for_relations(world, configuration_definition, qsr_target_object, table))
                position = [found_target_object_position.x, settings.OBJECT_POSITION_TABLE_Y,
                            found_target_object_position.y]
                translations[target_object_name] = position
            except ValueError:
                point = None
        return translations

    @staticmethod
    def subset_configuration_definition_for_object(object_name, configuration_def):
        subset_def = []
        for disjunctive_element in configuration_def:
            new_disjunctive_element = []
            for conjunctive_element in disjunctive_element:
                print conjunctive_element
                if object_name not in conjunctive_element:
                    new_disjunctive_element.append(conjunctive_element)
            if len(new_disjunctive_element) >= 1:
                subset_def.append(new_disjunctive_element)
        if len(subset_def) == 0:
            logging.error("[aileen_scene] :: configuration cannot be reduced by removing {}".format(object_name))
            raise ValueError
        else:
            logging.debug("[aileen_scene] :: subset configuration after removing {} is {}".format(object_name, subset_def))
            return subset_def


    class Randomizer:

        def get_random_position_on_table(self):
            position = [uniform(settings.OBJECT_POSITION_MIN_X, settings.OBJECT_POSITION_MAX_X),
                        uniform(settings.OBJECT_POSITION_TABLE_Y, settings.OBJECT_POSITION_TABLE_Y),
                        uniform(settings.OBJECT_POSITION_MIN_Z, settings.OBJECT_POSITION_MAX_Z)]
            return position

        def get_random_position_general(self):
            position = [uniform(settings.OBJECT_POSITION_MIN_X, settings.OBJECT_POSITION_MAX_X),
                        uniform(settings.OBJECT_POSITION_MIN_Y, settings.OBJECT_POSITION_MAX_Y),
                        uniform(settings.OBJECT_POSITION_MIN_Z, settings.OBJECT_POSITION_MAX_Z)]

        def sample_position_from_region(self, region):
            return qsr_realization.sample_position_from_region(region)

        def check_for_3d_qsrs(self, world, configuration_definition, qsrs, position):
            return qsr_realization.check_for_3d_qsrs(world, configuration_definition, qsrs, position)

    randomizer = Randomizer()
