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
    def place_objects_in_configuration_recursively(scene_objects, configuration_def):
        table = box(constants.OBJECT_POSITION_MIN_X,
                    constants.OBJECT_POSITION_MIN_Z,
                    constants.OBJECT_POSITION_MAX_X,
                    constants.OBJECT_POSITION_MAX_Z)

        world = World_State(0.0)

        if len(scene_objects) == 1:
            return AileenScene.get_random_position_on_table()
        else:
            # assume that the last object in the list is the target object
            scene_objects_subset = scene_objects.copy()
            target_object_tuple = scene_objects_subset.popitem(True)
            target_object_name = target_object_tuple(0)
            target_scene_object = target_object_tuple(1)

            AileenScene.place_objects_in_configuration_recursively(scene_objects_subset, configuration_def_subset)

            world = World_State(0.0)

            for object_name in scene_objects_subset:
                scene_object = scene_objects[object_name]
                world_object = Object_State(name=str(object_name), timestamp=0,
                                            x=scene_object._translation[0],
                                            y=scene_object._translation[2],
                                            xsize=scene_object._width_x,
                                            ysize=scene_object._width_z)
                world.add_object_state(world_object)
                logging.debug("[aileen_scene] :: added object {} to scene".format(str(object_name)))

            position = AileenScene.get_random_position_on_table()
            target_object = Object_State(name=str(target_object_name), timestamp=0,
                                         x=position[0],
                                         y=position[2],
                                         xsize=target_scene_object._width_x,
                                         ysize=target_scene_object._width_z)

            try:
                point = sample_position_from_region(
                    compute_region_for_relations(world, configuration_def, target_object, table))
                target_scene_object.set_translation(point.x, constants.OBJECT_POSITION_MAX_Y, point.y)
            except ValueError:
                logging.info("[aileen_scene] :: cannot find a point that satisfies the relationship in current config")
                AileenScene.place_objects_in_configuration_recursively(scene_objects_subset, configuration_def_subset)



    @staticmethod
    def place_objects_in_configuration(scene_objects, configuration_def):
        table = box(constants.OBJECT_POSITION_MIN_X,
                    constants.OBJECT_POSITION_MIN_Z,
                    constants.OBJECT_POSITION_MAX_X,
                    constants.OBJECT_POSITION_MAX_Z)

        point = None

        while point is None:
            logging.debug("[aileen_scene] :: attempting new config for {}".format(configuration_def))
            transitions = {}
            world = World_State(0.0)
            i = 0
            for scene_object_name in scene_objects:
                if i is not 0:
                    scene_object = scene_objects[scene_object_name]
                    position = AileenScene.get_random_position_on_table()
                    world_object = Object_State(name=str(scene_object_name), timestamp=0,
                                                x=position[0],
                                                y=position[2],
                                                xsize=scene_object._width_x,
                                                ysize=scene_object._width_z)
                    world.add_object_state(world_object)
                    transitions[scene_object_name] = position
                    logging.debug("[aileen_scene] :: added object {} to scene".format(str(scene_object_name)))
                else:
                    query_object_name = scene_object_name
                i = i + 1

            position = AileenScene.get_random_position_on_table()
            query_scene_object = scene_objects[query_object_name]
            query_object = Object_State(name=str(query_object_name), timestamp=0,
                                        x=position[0],
                                        y=position[2],
                                        xsize=query_scene_object._width_x,
                                        ysize=query_scene_object._width_z)

            try:
                point = sample_position_from_region(
                    compute_region_for_relations(world, configuration_def, query_object, table))
                position = [point.x, constants.OBJECT_POSITION_MAX_Y, point.y]
                transitions[query_object_name] = position
            except ValueError:
                point = None

        return transitions


    @staticmethod
    def place_object_in_configuration_with(target_object_name, scene_objects, configuration_def):
        table = box(constants.OBJECT_POSITION_MIN_X,
                    constants.OBJECT_POSITION_MIN_Z,
                    constants.OBJECT_POSITION_MAX_X,
                    constants.OBJECT_POSITION_MAX_Z)

        world = World_State(0.0)

        for object_name in scene_objects:
            if object_name != target_object_name:
                scene_object = scene_objects[object_name]
                world_object = Object_State(name=str(object_name), timestamp=0,
                                            x=scene_object._translation[0],
                                            y=scene_object._translation[2],
                                            xsize=scene_object._width_x,
                                            ysize=scene_object._width_z)
                world.add_object_state(world_object)
                logging.debug("[aileen_scene] :: added object {} to scene".format(str(object_name)))

        position = AileenScene.get_random_position_on_table()
        target_scene_object = scene_objects[target_object_name]
        target_object = Object_State(name=str(target_object_name), timestamp=0,
                                    x=position[0],
                                    y=position[2],
                                    xsize=target_scene_object._width_x,
                                    ysize=target_scene_object._width_z)

        try:
            point = sample_position_from_region(compute_region_for_relations(world, configuration_def, target_object, table))
        except ValueError:
            logging.error("[aileen_scene] :: cannot find a point that satisfies the relationship")
            raise ValueError

        return [point.x, constants.OBJECT_POSITION_MAX_Y, point.y]
