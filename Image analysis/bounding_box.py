# %% Import the necessary packages
import cv2
import numpy as np

# %% Load the reference picture
img = cv2.imread('Image analysis/bounding_box.jpg')

# %% Draw the pixels
# Define box's color
box_col = (0, 255, 0)

# Define box's limit
BoundingArray = np.array([4074, 1413, 2900])
ymin = BoundingArray[0]
ymax = img.shape[0]-10 #end of the picture
xmin = BoundingArray[1]
xmax = BoundingArray[2]
offset = 10

# Set up the conditions
img[ymin-offset:ymin+offset, xmin:xmax] = box_col
img[ymax-offset:ymin+offset, xmin:xmax] = box_col
img[ymin:ymax, xmax-offset:xmax+offset] = box_col
img[ymin:ymax, xmin-offset:xmin+offset] = box_col

# %% Export the picture
cv2.imwrite('Writing/figures/OK.jpg',img)