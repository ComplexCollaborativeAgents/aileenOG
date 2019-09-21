#
# Code to control writing scene files
#
import os

# Assume worlds directory is parallel to the script path.
worlds_path = os.path.join(os.path.dirname(__file__), "worlds")
generated_scene_file = os.path.join(worlds_path, "aileen_lesson.wbt")
template_file = os.path.join(worlds_path, "template.wbt")
TEMPLATE_OBJECTS_MARKER = '<objects>'

def write_scene(objects):
    out = open(generated_scene_file, "w")
    with open(template_file) as f:
        for line in f:
            cleaned_line = line.strip()
            if cleaned_line.startswith(TEMPLATE_OBJECTS_MARKER):
                for obj in objects:
                    out.write(obj)
                    out.write('\n')
            else:
                out.write(line)
    out.close()

