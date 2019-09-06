
class SVSHelper(object):
    def __init__(self):
        pass

    @staticmethod
    def convert_to_string(vector):
        return "{} {} {}".format(vector[0], vector[1], vector[2])

    @staticmethod
    def get_bbox_vertices_as_string():
        return "0.1 0.1 0.1 " \
               "0.1 0.1 -0.1 " \
               "0.1 -0.1 0.1 " \
               "0.1 -0.1 -0.1 " \
               "-0.1 0.1 0.1 " \
               "-0.1 0.1 -0.1 " \
               "-0.1 -0.1 0.1 " \
               "-0.1 -0.1 -0.1 "

    @staticmethod
    def get_svs_command_for_add_box(object_id, position=None, rotation=None, scale=None):
        base_string = "add {} world v {}".format(object_id, SVSHelper.get_bbox_vertices_as_string())
        if position is not None:
            base_string = base_string + " p {}".format(SVSHelper.convert_to_string(position))
        if rotation is not None:
            base_string = base_string + " r {}".format(SVSHelper.convert_to_string(rotation))
        if scale is not None:
            base_string = base_string + " s {}".format(SVSHelper.convert_to_string(scale))
        return base_string

    @staticmethod
    def get_svs_command_for_change_position(object_id, position):
        return "change {} p {}".format(object_id, SVSHelper.convert_to_string(position))

    @staticmethod
    def get_svs_command_for_change_rotation(object_id, rotation):
        return "change {} r {}".format(object_id, SVSHelper.convert_to_string(rotation))

    @staticmethod
    def get_svs_command_for_change_scale(object_id, scale):
        return "change {} s {}".format(object_id, SVSHelper.convert_to_string(scale))


    @staticmethod
    def get_svs_command_for_delete_object(object_id):
        return "delete {}".format(object_id)

    @staticmethod
    def get_svs_command_for_add_tag_to_object(object_id, tag_name, tag_value):
        return "tag add {} {} {}".format(object_id, tag_name, tag_value)

    @staticmethod
    def get_svs_command_for_change_tag_for_object(object_id, tag_name, tag_value):
        return "tag change {} {} {}".format(object_id, tag_name, tag_value)