import numpy as np

# this script finds the polynomial that maps 2D camera coordinates to 3D camera coordinates

# This file is generated the generate_training_images.py script in instructor
allcoords_file = '/home/mshreve/Desktop/allcoords.txt'

allcoords = np.loadtxt(allcoords_file)

XI = (allcoords[:, 1] + allcoords[:, 3]) / 2.0
YI = (allcoords[:, 2] + allcoords[:, 4]) / 2.0

XW = (allcoords[:, 5] + allcoords[:, 8]) / 2.0
YW = (allcoords[:, 7] + allcoords[:, 10]) / 2.0
ZW = (allcoords[:, 6] + allcoords[:, 9]) / 2.0

i2w_X = np.polyfit(allcoords[:, 2], allcoords[:, 6], 1)
i2w_Y = np.polyfit(allcoords[:, 3], allcoords[:, 8], 1)
w2i_X = np.polyfit(allcoords[:, 6], allcoords[:, 2], 1)
w2i_Y = np.polyfit(allcoords[:, 8], allcoords[:, 3], 1)

print('here')

