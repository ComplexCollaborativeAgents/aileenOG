from world.log_config import logging
import settings

class ActionExecutor:
    def __init__(self, supervisor):
        self._held_node = None
        self._supervisor = supervisor
        self._requestor = None

    def process_action_command(self, action):
        logging.info("[action_executor] :: processing action {}".format(action['name']))
        self._requestor = action['requester']
        if action['name'] == 'pick-up' and 'id' in action:
            return self.pick_up_object(action['id'])
        else:
            if action['name'] == 'pick-up' and 'uuid' in action:
                return self.pick_up_object_by_uuid(action['uuid'])
            else:
                if action['name'] == 'place':
                    return self.place_object(action['location'])
                else:
                    if action['name'] == 'indicate' and 'id' in action:
                        return self.indicate_object(action['id'])
                    else:
                        if action['name'] == 'indicate' and 'uuid' in action:
                            return self.indicate_object_by_uuid(action['uuid'])
                        else:
                            if action['name'] == 'stack' and 'ids' in action:
                                """
                                    I only support stack with two plain ids at this time.  ids value should be a list of length 2.  first item is  the bottom object, the 2nd is the one that goes on top
                                """
                                return self.stack_objects(action['ids'])
                            else:
                                logging.error("[action_executor] :: receive a malformed action request")

    def indicate_object_by_uuid(self, object_uuid):
        for i in range(self._supervisor._children.getCount()):
            child_node = self._supervisor._children.getMFNode(i)
            child_node_uuid_field = child_node.getField('name')
            if child_node_uuid_field is not None:
                child_node_uuid = child_node_uuid_field.getSFString()
                print child_node_uuid
                if child_node_uuid == object_uuid:
                    child_node_id = child_node.getId()
                    self.indicate_object(child_node_id)
                    return True

    def pick_up_object_by_uuid(self, object_uuid):
        logging.debug("[action_executor] :: finding the id for object with uuid {}".format(object_uuid))
        num_children = self._supervisor._children.getCount()
        for i in range(0, num_children):
            child_node = self._supervisor._children.getMFNode(i)
            child_node_uuid_field = child_node.getField('name')
            if child_node_uuid_field is not None:
                child_node_uuid = child_node_uuid_field.getSFString()
                print child_node_uuid
                if child_node_uuid == object_uuid:
                    child_node_id = child_node.getId()
                    self.pick_up_object(child_node_id)
                    return True

    def indicate_object(self, object_id):
        node = self._supervisor.getFromId(int(object_id))
        logging.debug("[action_executor] :: indicating object id {}".format(object_id))
        object_loc = node.getField('translation')
        self._supervisor.indicate_object(object_loc)
        return True

    def pick_up_object(self, object_id):
        node = self._supervisor.getFromId(int(object_id))
        logging.debug("[action_executor] :: picking up object id {}".format(object_id))
        translation = node.getField('translation')
        pos = translation.getSFVec3f()
        if settings.SIMULATE_ACTIONS:
            if self._requestor == 'agent':
                self._supervisor.pick_object(pos)
            else:
                self._supervisor.pick_object_instructor(node, pos)
        else:
            translation.setSFVec3f(settings.ROBOT_PLATE_LOCATION)
        logging.debug("[action_executor] :: object {} moved to {}".format(object_id, node.getPosition()))
        self._supervisor.set_held_node(node)
        return True

    def place_object(self, location):
        """
            Location defines the point where I want the center of the object to go.
        """
        node = self._supervisor.get_held_node()
        if node is not None:
            logging.debug("[action_executor] :: currently holding node {}".format(node.getId()))
            if location == 'proxy':
                translation = node.getField('translation')
                if settings.SIMULATE_ACTIONS:
                    if self._requestor == 'agent':
                        self._supervisor.place_object(settings.TEST_LOCATION)
                    else:
                        self._supervisor.place_object_instructor(settings.TEST_LOCATION)
                else:
                    translation.setSFVec3f(settings.TEST_LOCATION)
            else:
                translation = node.getField('translation')
                if settings.SIMULATE_ACTIONS:
                    if self._requestor == 'agent':
                        self._supervisor.place_object(location)
                    else:
                        self._supervisor.place_object_instructor(node, location)
                else:
                    translation.setSFVec3f(location)
            self._supervisor.set_held_node(None)
        else:
            logging.error("[action_executor] :: asked to place when no object is held")
        return True

    def stack_objects(self, id_list):
        """
            obj1 is the bottom.  obj2 goes on top.
        """
        obj1 = self._supervisor.getFromId(int(id_list[0]))
        translation = obj1.getField('translation')
        place_loc = translation.getSFVec3f()
        place_loc[1] += settings.OBJECT_STANDARD_HEIGHT
        self.pick_up_object(id_list[1])
        self.place_object(place_loc)
        return True
