from random import uniform
import constants
from shapely.geometry import box
from aileen_object import AileenObject
from log_config import logging

from configuration import Configuration

try:
    from qsrlib_io.world_trace import Object_State, World_State
    from qsrlib.qsr_realization import compute_region_for_relations, sample_position_from_region
except:
    logging.fatal("[aileen_scene] :: cannot find spatial reasoning library")
    exit()


class AileenScene:
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
        return position

    @staticmethod
    def place_objects_in_configuration(arg1_object, arg2_object, configuration_def):
        """
        This function uses qsrlib to find a point in the world such that arg1_object
        and arg2_object are in a configuration as defined in configuration_def
        :param arg1_object: an instance of AileenObject
        :param arg2_object: an instance of AileenObject
        :param configuration_def: a list of rccXXXX properties
        """

        table = box(constants.OBJECT_POSITION_MIN_X,
                    constants.OBJECT_POSITION_MIN_Z,
                    constants.OBJECT_POSITION_MAX_X,
                    constants.OBJECT_POSITION_MAX_Z)

        point = None

        while point is None:
            world = World_State(0.0)
            position1 = AileenScene.get_random_position_on_table()
            arg1 = Object_State(name="o1", timestamp=0,
                                x=position1[0],
                                y=position1[2],
                                xsize=arg1_object._width_x,
                                ysize=arg1_object._width_z)

            world.add_object_state(arg1)

            position2 = AileenScene.get_random_position_on_table()

            arg2 = Object_State(name="o2", timestamp=0,
                                x=position2[0],
                                y=position2[2],
                                xsize=arg2_object._width_x,
                                ysize=arg2_object._width_z)

            qsrs = []
            for item in configuration_def:
                qsrs.append([item, "o1", "o2"])

            try:
                point = sample_position_from_region(compute_region_for_relations(world, qsrs, arg2, table))
            except ValueError:
                point = None

        return position1, [point.x, constants.OBJECT_POSITION_MAX_Y, point.y]

    @staticmethod
    def place_object_in_configuration_with(arg1_object, arg2_object, configuration_def):
        """
        This function uses qsrlib to find a point in the world such that arg1_object
        and arg2_object are in a configuration as defined in configuration_def
        :param arg1_object: an instance of AileenObject
        :param arg2_object: an instance of AileenObject
        :param configuration_def: a list of rccXXXX properties
        """

        table = box(constants.OBJECT_POSITION_MIN_X,
                    constants.OBJECT_POSITION_MIN_Z,
                    constants.OBJECT_POSITION_MAX_X,
                    constants.OBJECT_POSITION_MAX_Z)

        world = World_State(0.0)
        position1 = arg1_object._translation
        arg1 = Object_State(name="o1", timestamp=0,
                            x=position1[0],
                            y=position1[2],
                            xsize=arg1_object._width_x,
                            ysize=arg1_object._width_z)

        world.add_object_state(arg1)

        position2 = AileenScene.get_random_position_on_table()

        arg2 = Object_State(name="o2", timestamp=0,
                            x=position2[0],
                            y=position2[2],
                            xsize=arg2_object._width_x,
                            ysize=arg2_object._width_z)

        qsrs = []
        for item in configuration_def:
            qsrs.append([item, "o1", "o2"])

        try:
            point = sample_position_from_region(compute_region_for_relations(world, qsrs, arg2, table))
        except ValueError:
            logging.error("[aileen_scene] :: cannot find a point that satisfies the relationship")
            raise ValueError

        return [point.x, constants.OBJECT_POSITION_MAX_Y, point.y]
