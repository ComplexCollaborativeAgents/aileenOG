# Matthew Shreve, PARC 2019
# Generates random arrangements of N shapes in webots simulator
# Saves annotated images to disk and generates training files

from aileen_object import AileenObject
from aileen_scene import AileenScene
from log_config import logging
from language_generator import LanguageGenerator
import cv2
import numpy as np
import settings
import random
import os

# create output folder if it does not exist
if not os.path.isdir(settings.TRAINING_DATA_FOLDER):
    os.makedirs(settings.TRAINING_DATA_FOLDER)


class TrainingImage:
    def __init__(self):
        self._scene = AileenScene()
        self._language = None

    def generate_lesson(self, num_objects=1):
        lesson = {}
        self.generate_scene(num_objects)
        lesson['scene'] = self._scene.generate_scene_world_config()
        lesson['interaction'] = self._language
        return lesson

    def generate_scene(self, num_objects=1):
        logging.debug("[aileen_visual_word_lesson] :: generating a new scene for visual word learning")
        for i in range(0, num_objects):
            scene_object = AileenObject.generate_random_object()
            scene_object.set_translation(AileenScene.randomizer.get_random_position_on_table())
            self._scene.add_object(scene_object)
            self._language = LanguageGenerator.generate_language_for_object(scene_object)

    @staticmethod
    def generate_scenes(world_server, agent_server):

        if not os.path.exists(settings.TRAINING_DATA_FOLDER):
            os.mkdir(settings.TRAINING_DATA_FOLDER)

        # iw = input_writer.InputWriter(agent_server, world_server)
        counter = 1
        while True:
            num_obj = random.randint(1, 6)
            lesson = TrainingImage().generate_lesson(num_obj)
            scene_acknowledgement = world_server.set_scene(
                {'configuration': lesson['scene'], 'label': lesson['interaction']})
            logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))
            data = world_server.get_image()
            binary_image = data['image']
            im = cv2.imdecode(np.fromstring(binary_image.data, dtype=np.uint8), 1)
            cv2.imwrite(settings.TRAINING_DATA_FOLDER + '/frame_' + "{:0>6d}".format(counter) + '.jpg', im)
            meta = world_server.get_all()

            for j in range(0, len(meta['objects'])):

                obj = meta['objects'][j]
                bb = obj['bounding_box_camera']
                bbw = obj['bounding_box']
                shape = obj['shape'].split('CV')[1]
                shape = shape.lower()
                color = obj['color'].split('CV')[1]
                color = color.lower()
                
                # for calculating transform between 2D and 3D:

                # Debugging
                # cv2.rectangle(im,
                #               (int(512 * bb[0]), int(512 * bb[1])),
                #               (int(512 * bb[2]), int(512 * bb[3])),
                #               (0, 255, 0), 1)
                #
                # cx = (bb[0] + bb[2]) / 2.0
                # cy = (bb[1] + bb[3]) / 2.0
                # w = bb[2] - bb[0]
                # h = bb[3] - bb[1]
                #
                # bb = [cx-w/2, cy-h/2, cx+w/2, cy+h/2]
                #
                # cv2.rectangle(im,
                #               (int(512 * bb[0])+5, int(512 * bb[1])+5),
                #               (int(512 * bb[2])+5, int(512 * bb[3])+5),
                #               (0, 255, 0), 1)
                #
                # cv2.imshow('Frame', im)
                # cv2.waitKey(0)
                # cv2.destroyAllWindows()

                # Bounding box is written out as (centroid_x, centroid_y, width, height)
                with open(settings.TRAINING_DATA_FOLDER + '/frame_' + "{:0>6d}".format(counter) + '.txt', 'a+') as f:
                    f.write("%d %f %f %f %f\n" % (settings.SHAPE_SET.index(shape),
                                                  (bb[0] + bb[2]) / 2.0,
                                                  (bb[1] + bb[3]) / 2.0,
                                                  abs(bb[2]-bb[0]),
                                                  abs(bb[3]-bb[1])
                                                  )
                            )

                # Bounding box is written out as (centroid_x, centroid_y, width, height)
                with open(settings.TRAINING_DATA_FOLDER + '/frame_' + "{:0>6d}".format(counter) + '_allgt.txt', 'a+') as f:
                    f.write("%d %f %f %f %f %s\n" % (settings.SHAPE_SET.index(shape),
                                                     (bb[0] + bb[2]) / 2.0,
                                                     (bb[1] + bb[3]) / 2.0,
                                                     abs(bb[2] - bb[0]),
                                                     abs(bb[3] - bb[1]),
                                                     color
                                                     )
                            )

                with open(settings.TRAINING_DATA_FOLDER + '/allcoords.txt', 'a+') as f:
                    f.write("%d %f %f %f %f %f %f %f %f %f %f\n" % (settings.SHAPE_SET.index(shape),
                                                                    (bb[0] + bb[2]) / 2.0,
                                                                    (bb[1] + bb[3]) / 2.0,
                                                                    abs(bb[2] - bb[0]),
                                                                    abs(bb[3] - bb[1]),
                                                                    bbw[0], bbw[1],
                                                                    bbw[2], bbw[3],
                                                                    bbw[4], bbw[5]))

            # Split the training/validation data 80/20
            if random.randint(0, 100) < 80:
                with open(settings.TRAIN_FILES, 'a+') as f:
                    f.write(settings.TRAINING_DATA_FOLDER + '/frame_' + "{:0>6d}".format(counter) + '.jpg\n')
            else:
                with open(settings.TEST_FILES, 'a+') as f:
                    f.write(settings.TRAINING_DATA_FOLDER + '/frame_' + "{:0>6d}".format(counter) + '.jpg\n')

            counter += 1

            if counter > 2000:
                break


if __name__ == '__main__':
    lesson1 = TrainingImage()
    print(lesson1.generate_lesson())
