from log_config import logging


class AileenObject:

    def __init__(self, shape, color = [0,0,1], height_y=0.1, width_x=0.1, width_z=0.1):
        self._shape = shape
        self._color = color
        self._height_y = height_y
        self._width_x = width_x
        self._width_z = width_z
        self._translation = None
        logging.debug("[aileen_object] :: created a new object")

    def get_object_description(self):

        if self._translation is None:
            logging.error("[aileen_object] :: asking to generate a description of an object whose translation is unknown. doing nothing.")
            return

        description = "Solid {\n"
        description += "   translation {} {} {}\n".format(self._translation[0],
                                                   self._translation[1],
                                                   self._translation[2])
        description += "   children [\n"
        description += "       Shape {\n"
        description += "          appearance PBRAppearance {\n"
        description += "          baseColor {} {} {}\n".format(self._color[0],
                                                               self._color[1],
                                                               self._color[2])
        description += "          metalness 0\n"
        description += "          emissiveColor {} {} {}\n".format(self._color[0],
                                                                   self._color[1],
                                                                   self._color[2])
        description += "        }\n"
        description += "        geometry {}\n".format(self.get_geometry_description())
        description += "        castShadows FALSE\n"
        description += "        }\n"
        description += "    ]\n"
        description += "   boundingObject {}".format(self.get_bounding_object_description())
        description += "}"

        logging.debug("[aileen_object] :: added string {}".format(description))

        return description

    def get_bounding_object_description(self):
        description = "Box {\n"
        description += "     size {} {} {}\n".format(self._width_x, self._height_y, self._width_z)
        description += "   }\n"
        return description

    def get_geometry_description(self):
        description = "{} {{\n".format(self._shape.title())

        if self._shape == "cone":
            description += "          bottomRadius {}\n".format(self._width_x / 2)
            description += "          height {}\n".format(self._height_y)
            description += "        }"
            return description

        if self._shape == "box":
            description += "          size {} {} {}\n".format(self._width_x, self._height_y, self._width_z)
            description += "        }"
            return description

        if self._shape == "cylinder":
            description += "          radius {}\n".format(self._width_x / 2)
            description += "          height {}\n".format(self._height_y)
            description += "        }"
            return description

    def set_translation(self, position_vector):
        logging.debug("[aileen_object] :: setting translation of object to {}".format(position_vector))
        self._translation = position_vector



