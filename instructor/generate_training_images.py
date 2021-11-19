# Matthew Shreve, PARC 2019
# Generates random arrangements of N shapes in webots simulator
# Saves annotated images to disk and generates training files
# Updated by Yonghui Fan, PARC 2021

from aileen_object import AileenObject
from aileen_scene import AileenScene
from log_config import logging
from language_generator import LanguageGenerator
import cv2
import numpy as np
import settings
import random
import os, shutil
import time
from scipy.stats import mode
# create output folder if it does not exist
if not os.path.isdir(settings.TRAINING_DATA_FOLDER):
    os.makedirs(settings.TRAINING_DATA_FOLDER)


class TrainingImage:
    def __init__(self):
        self._scene = AileenScene()
        self._language = None

    @staticmethod
    def clean_scenes(world_server):
        logging.debug("[aileen_visual_word_lesson] :: cleaning table")
        lesson = {}
        lesson['scene'] = []
        lesson['interaction'] = []
        scene_acknowledgement = world_server.set_scene(
            {'configuration': lesson['scene'], 'label': lesson['interaction']})
        logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))

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
            # if scene_object.get_shape() == 'capsule':
            scene_object.set_rotation(AileenScene.randomizer.get_random_rotation(scene_object.get_shape()))
            self._scene.add_object(scene_object)
            self._language = LanguageGenerator.generate_language_for_object(scene_object)

    @staticmethod
    def generate_scenes(world_server, agent_server):
        if os.path.exists(settings.TRAINING_DATA_FOLDER):
            for filename in os.listdir(settings.TRAINING_DATA_FOLDER):
                file_path = os.path.join(settings.TRAINING_DATA_FOLDER, filename)
                try:
                    if os.path.isfile(file_path) or os.path.islink(file_path):
                        os.unlink(file_path)
                    elif os.path.isdir(file_path):
                        shutil.rmtree(file_path)
                except Exception as e:
                    print('Failed to delete %s. Reason: %s' % (file_path, e))
        else:
            os.mkdir(settings.TRAINING_DATA_FOLDER)
        if not os.path.exists(settings.TRAINING_DATA_FOLDER):
            os.mkdir(settings.TRAINING_DATA_FOLDER)

        # iw = input_writer.InputWriter(agent_server, world_server)
        counter = 0
        while counter < 101:
            if counter == 0:
                data = world_server.get_image()
                binary_image = data['image']
                im = cv2.imdecode(np.fromstring(binary_image.data, dtype=np.uint8), 1)
                cv2.imwrite(settings.TRAINING_DATA_FOLDER + '/background_image.jpg', im)
                binary_image = data['rec_seg_image']
                im = cv2.imdecode(np.fromstring(binary_image.data, dtype=np.uint8), 1)
                cv2.imwrite(settings.TRAINING_DATA_FOLDER + '/background_rec_seg_image.jpg', im)
                counter += 1
                continue

            num_obj = random.randint(1, 3)
            lesson = TrainingImage().generate_lesson(num_obj)
            scene_acknowledgement = world_server.set_scene(
                {'configuration': lesson['scene'], 'label': lesson['interaction']})
            time.sleep(0.5)
            logging.info("[aileen_instructor] :: received from world {}".format(scene_acknowledgement))
            meta = world_server.get_all()
            obj_num = meta['obj_num']
            # if obj_num != num_obj:
            #     continue
            gt_num = np.shape(meta['objects'])[0]
            if num_obj == gt_num:
                data = world_server.get_image()
                binary_image = data['image']
                im = cv2.imdecode(np.fromstring(binary_image.data, dtype=np.uint8), 1)
                cv2.imwrite(settings.TRAINING_DATA_FOLDER + '/frame_' + "{:0>6d}".format(counter) + '.jpg', im)
                binary_image = data['rec_seg_image']
                im = cv2.imdecode(np.fromstring(binary_image.data, dtype=np.uint8), 1)
                cv2.imwrite(settings.TRAINING_DATA_FOLDER + '/frame_rec_seg_' + "{:0>6d}".format(counter) + '.jpg', im)

                for j in range(0, gt_num):
                    obj = meta['objects'][j]
                    print obj
                    position = obj['bbposition']
                    size = obj['bbsize']
                    shape = obj['shape'].split('CV')[1]
                    shape = shape.lower()
                    color = obj['color'].split('CV')[1]
                    color = color.lower()

                    # Bounding box is written out as (centroid_x, centroid_y, width, height)
                    with open(settings.TRAINING_DATA_FOLDER + '/frame_' + "{:0>6d}".format(counter) + '.txt', 'a+') as f:
                        f.write("%d %f %f %f %f %s\n" % (settings.SHAPE_SET.index(shape),
                                                    position[0],
                                                    position[1],
                                                    size[0],
                                                    size[1], color)
                                )
                rec_filename = settings.TRAINING_DATA_FOLDER + '/frame_rec_seg_' + "{:0>6d}".format(counter)
                mask_img = cv2.imread(rec_filename + '.jpg')
                ## image
                img = cv2.imread(settings.TRAINING_DATA_FOLDER + '/frame_rec_seg_' + "{:0>6d}".format(counter) + '.jpg')
                ## label
                with open(settings.TRAINING_DATA_FOLDER + '/frame_' + "{:0>6d}".format(counter) + '.txt', 'r') as f:
                    labels = f.readlines()
                # cnt = 1
                # mask_output = np.zeros(img.shape[:2], dtype=float)
                for row in labels:
                    row = row.split(' ')
                    color = row[5]
                    row = np.array(row[:5], dtype=float)
                    shape = row[0] + 1

                    cx = int(row[1])
                    cy = int(row[2])
                    w = int(row[3])
                    h = int(row[4])
                    tx = int(cx - w / 2)
                    ty = int(cy - h / 2)
                    bx = int(cx + w / 2)
                    by = int(cy + h / 2)

                    gray = cv2.cvtColor(mask_img, cv2.COLOR_BGR2GRAY)
                    mask = np.zeros(gray.shape[:2], dtype="uint8")
                    cv2.rectangle(mask, (int(tx), int(ty)), (int(bx), int(by)), 255, -1)

                    target = gray[cy, cx]
                    tmp = cv2.bitwise_and(gray, gray, mask=mask)
                    non_zero_tmp = tmp[np.nonzero(tmp)]
                    if mode(non_zero_tmp)[0]:
                        value = mode(non_zero_tmp)[0]
                        area = np.float(mode(non_zero_tmp)[1])
                        # print target
                        up = value + 5
                        low = value - 5
                        if low < 0:
                            low = 0
                        loc = np.where(np.logical_and(tmp > low, tmp <= up))
                        tmp[loc] = value
                        tmp[np.where(tmp != value)] = 0
                        tmp[tmp > 0] = shape

                        # print np.shape(tmp)
                        # cv2.imshow('tmp', tmp)
                        # cv2.waitKey(0)

                        x, y, w, h = cv2.boundingRect(tmp)

                        cv2.rectangle(img, (int(tx), int(ty)), (int(bx), int(by)), (255, 255, 0), 4)
                        # cv2.rectangle(img, (xmin, ymin), (xmax, ymax), (0, 255, 0), 1)
                        cv2.rectangle(img, (x, y), (x + w, y + h), (0, 255, 0), 4)
                        # top left corner coordinates, bbox width and height
                        with open(settings.TRAINING_DATA_FOLDER + '/frame_' + "{:0>6d}".format(counter) + '_annotation.txt', 'a+') as f:
                            f.write("%d %f %f %f %f %f %s" % (shape,
                                                          x,
                                                          y,
                                                          w,
                                                          h, area, color)
                                )

                    # cv2.imwrite(settings.TRAINING_DATA_FOLDER + '/frame_' + "{:0>6d}".format(counter) + '_mask_grayscale.jpg',
                    #             mask_output)
                    # cnt += 1
                    cv2.imwrite(settings.TRAINING_DATA_FOLDER + '/frame_' + "{:0>6d}".format(counter) + '_bbox.jpg', img)

                counter += 1



if __name__ == '__main__':
    lesson1 = TrainingImage()
    print(lesson1.generate_lesson())
