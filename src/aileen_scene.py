from random import uniform
import constants
from shapely.geometry import box
from aileen_object import AileenObject
from log_config import logging

from configuration import Configuration

try:
    from qsrlib_io.world_trace import Object_State, World_State
    from qsrlib.qsr_realization import compute_region_for_relations, sample_position_from_region
    from shapely.geometry import Point
except:
    logging.fatal("[aileen_scene] :: cannot find spatial reasoning library")
    exit()


class AileenScene:

    test_id = None; # Class variable for unit tests

    def __init__(self):
        self._objects = []

    def add_object(self, aileen_object):
        self._objects.append(aileen_object)

    def generate_scene_description(self):
        description = []
        for scene_object in self._objects:
            description.append(scene_object.get_object_description())
        return description

    @staticmethod
    def get_random_position_on_table():
        position = [uniform(constants.OBJECT_POSITION_MIN_X, constants.OBJECT_POSITION_MAX_X),
                    uniform(constants.OBJECT_POSITION_MIN_Y, constants.OBJECT_POSITION_MAX_Y),
                    uniform(constants.OBJECT_POSITION_MIN_Z, constants.OBJECT_POSITION_MAX_Z)]
        if AileenScene.test_id == 1:
            position = [0.586304972021, 0.45, 0.238382561155]
            AileenScene.test_id += 1;
        elif AileenScene.test_id == 2:
            position = [0.477095092251, 0.45, -0.1671282021741]
            AileenScene.test_id += 1;
        return position

    @staticmethod
    def place_two_objects_in_configuration(target_object_name, reference_object_name, scene_objects, configuration_definition):
        translations = {}
        reference_object = scene_objects[reference_object_name]
        target_object = scene_objects[target_object_name]

        table = box(constants.OBJECT_POSITION_MIN_X,
                    constants.OBJECT_POSITION_MIN_Z,
                    constants.OBJECT_POSITION_MAX_X,
                    constants.OBJECT_POSITION_MAX_Z)
        found_target_object_position = None

        if AileenScene.test_id == 1:
            translations[target_object_name] = [0.676147498734, 0.45, -0.0202334240995]
            translations[reference_object_name] = [0.750422886282, constants.OBJECT_POSITION_MAX_Y, 0.177034392513]
            AileenScene.test_id += 1;
            found_target_object_position = True
        elif AileenScene.test_id == 2:
            translations[target_object_name] = [0.586304972021, 0.45, 0.238382561155]
            translations[reference_object_name] = [0.477095092251, constants.OBJECT_POSITION_MAX_Y, -0.167128202174]
            AileenScene.test_id += 1;
            found_target_object_position = True

        while found_target_object_position is None:
            world = World_State(0.0)

            new_reference_object_position = AileenScene.get_random_position_on_table()

            qsr_reference_object = Object_State(name=str(reference_object_name), timestamp=0,
                                            x=new_reference_object_position[0],
                                            y=new_reference_object_position[2],
                                            xsize=reference_object._width_x,
                                            ysize=reference_object._width_z)
            world.add_object_state(qsr_reference_object)
            logging.debug("[aileen_scene] :: added reference object {} to QSRLib scene".format(str(reference_object_name)))
            translations[reference_object_name] = new_reference_object_position

            position = AileenScene.get_random_position_on_table()
            qsr_target_object = Object_State(name=str(target_object_name), timestamp=0,
                                                 x=position[0],
                                                 y=position[2],
                                                 xsize=target_object._width_x,
                                                 ysize=target_object._width_z)

            try:
                found_target_object_position = sample_position_from_region(compute_region_for_relations(world, configuration_definition, qsr_target_object, table))
                position = [found_target_object_position.x, constants.OBJECT_POSITION_MAX_Y, found_target_object_position.y]
                translations[target_object_name] = position
            except ValueError:
                point = None
        return translations

    @staticmethod
    def place_object_in_configuration_with(target_object_name, reference_object_name, scene_objects, configuration_definition):
        reference_object = scene_objects[reference_object_name]
        target_object = scene_objects[target_object_name]

        table = box(constants.OBJECT_POSITION_MIN_X,
                    constants.OBJECT_POSITION_MIN_Z,
                    constants.OBJECT_POSITION_MAX_X,
                    constants.OBJECT_POSITION_MAX_Z)

        found_target_object_position = None
        while found_target_object_position is None:
            world = World_State(0.0)
            qsr_reference_object = Object_State(name=str(reference_object_name), timestamp=0,
                                                x=reference_object._translation[0],
                                                y=reference_object._translation[2],
                                                xsize=reference_object._width_x,
                                                ysize=reference_object._width_z)
            world.add_object_state(qsr_reference_object)
            logging.debug(
                "[aileen_scene] :: added reference object {} to QSRLib scene".format(str(reference_object_name)))

            position = AileenScene.get_random_position_on_table()
            qsr_target_object = Object_State(name=str(target_object_name), timestamp=0,
                                             x=position[0],
                                             y=position[2],
                                             xsize=target_object._width_x,
                                             ysize=target_object._width_z)

            try:
                found_target_object_position = sample_position_from_region(
                    compute_region_for_relations(world, configuration_definition, qsr_target_object, table))
                position = [found_target_object_position.x, constants.OBJECT_POSITION_MAX_Y,
                            found_target_object_position.y]
            except ValueError:
                logging.error("[aileen_scene] :: cannot place {} in configuration with {}".format(target_object_name, reference_object_name))
                raise ValueError
        return position


    @staticmethod
    def place_three_objects_in_configuration(target_object_name, first_reference_object_name, second_reference_object_name, scene_objects, configuration_definition):
        table = box(constants.OBJECT_POSITION_MIN_X,
                    constants.OBJECT_POSITION_MIN_Z,
                    constants.OBJECT_POSITION_MAX_X,
                    constants.OBJECT_POSITION_MAX_Z)
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
                                                      xsize=first_reference_object._width_x,
                                                      ysize=first_reference_object._width_z)
            world.add_object_state(qsr_first_reference_object)

            qsr_second_reference_object = Object_State(name=str(second_reference_object_name), timestamp=0,
                                                      x=translations[second_reference_object_name][0],
                                                      y=translations[second_reference_object_name][2],
                                                      xsize=second_reference_object._width_x,
                                                      ysize=second_reference_object._width_z)
            world.add_object_state(qsr_second_reference_object)
            logging.debug("[aileen_scene] :: added reference objects {} and {} to QSRLib scene".format(first_reference_object_name, second_reference_object_name))

            position = AileenScene.get_random_position_on_table()
            qsr_target_object = Object_State(name=str(target_object_name), timestamp=0,
                                             x=position[0],
                                             y=position[2],
                                             xsize=target_object._width_x,
                                             ysize=target_object._width_z)

            try:
                found_target_object_position = sample_position_from_region(
                    compute_region_for_relations(world, configuration_definition, qsr_target_object, table))
                position = [found_target_object_position.x, constants.OBJECT_POSITION_MAX_Y,
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