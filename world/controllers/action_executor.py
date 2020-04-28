from world.log_config import logging
import settings

class ActionExecutor:
    def __init__(self, supervisor):
        self._held_node = None
        self._supervisor = supervisor

    def process_action_command(self, action):
        logging.info("[action_executor] :: processing action {}".format(action['name']))
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
        translation.setSFVec3f(settings.ROBOT_PLATE_LOCATION)
        """
        Need to implement picking action here
        """
        logging.debug("[action_executor] :: object {} moved to {}".format(object_id, node.getPosition()))
        self._supervisor.set_held_node(node)
        return True

    def place_object(self, location):
        node = self._supervisor.get_held_node()
        if node is not None:
            logging.debug("[action_executor] :: currently holding node {}".format(node.getId()))
            if location == 'proxy':
                translation = node.getField('translation')
                translation.setSFVec3f(settings.TEST_LOCATION)
            else:
                translation = node.getField('translation')
                translation.setSFVec3f(location)
            self._supervisor.set_held_node(None)
        else:
            logging.error("[action_executor] :: asked to place when no object is held")

        return True
