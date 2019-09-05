
class SVSHelper(object):
    def __init__(self):
        pass

    @staticmethod
    def convert_to_string(vector):
        return "{} {} {}".format(vector[0], vector[1], vector[2])

    @staticmethod
    def get_bbox_vertices_as_string():
        return "0.5 0.5 0.5 0.5 0.5 -0.5 0.5 -0.5 0.5 0.5 -0.5 -0.5 -0.5 0.5 0.5 -0.5 0.5 -0.5 -0.5 -0.5 0.5 -0.5 " \
               "-0.5 -0.5 "

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
