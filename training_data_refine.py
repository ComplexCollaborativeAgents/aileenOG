import cv2
import numpy as np
from scipy.stats import mode
import settings


counter = 1001
while counter < 2001:
    id = "{:0>6d}".format(counter)
    filename = './vision_training_data/frame_'+id
    rec_filename = './vision_training_data/frame_rec_seg_'+id
    bck_img = cv2.imread('./vision_training_data/background_image.jpg')
    mask_img = cv2.imread(rec_filename + '.jpg')
    ## image
    img = cv2.imread(filename + '.jpg')
    ## label
    with open(filename+'.txt', 'r') as f:
        labels = f.readlines()

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
        value = mode(non_zero_tmp)[0]
        area = np.float(mode(non_zero_tmp)[1])

        # print target
        up = value + 5
        low = value - 5
        if low < 0:
            low = 0
        tmp[np.where(np.logical_and(tmp > low, tmp <= up))] = value
        tmp[np.where(tmp != value)] = 0
        x, y, w, h = cv2.boundingRect(tmp)

        cv2.rectangle(img, (int(tx), int(ty)), (int(bx), int(by)), (255, 255, 0), 4)
        # cv2.rectangle(img, (xmin, ymin), (xmax, ymax), (0, 255, 0), 1)
        cv2.rectangle(img, (x, y), (x + w, y + h), (0, 255, 0), 4)
        with open(filename + '_annotation.txt', 'a+') as f:
            f.write("%d %f %f %f %f %f %s" % (shape,
                                          x,
                                          y,
                                          w,
                                          h, area, color)
                )
    counter += 1
# cv2.imshow('bbox', img)
    cv2.imwrite(filename + '_bbox.jpg', img)
#cv2.waitKey(0)