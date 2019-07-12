# -*- coding: utf-8 -*-
import glob
import cv2
import numpy as np
import pandas as pd
/* cSpell: disable * /
"""
Image analysis code.
    Root system area as a percentage of the total area of the cropped
    picture frame.

    Author: Alexandre Bohyn
    Email: alexandre.bohyn@gmail.com
    Date: July 2019
"""

# %% Packages importation

# %% Create the functions


def area_percentage(img_path, thresh_val, flipBool, BoundingArray):
    """Compute the area of the root system as the percentage of black pixels
    in the whole image after binarizing it. Crops the image to a specified
    rectangle and flips it if needed.

    Parameters:
    img_path (str): Path of the image
    thresh_val (int): Threshold value for the binarization of the image
    flipBool (bool): Boolean specifying whether the image should be flipped 
    BoundingArray: Array with the X, Y, Width, Length of the bounding box for
        the cropping

    Returns:
    float: percentage of total area occupied by the root system

   """
    img = cv2.imread(img_path, 0)
    if not img:
        return float('nan')
    else:
        if(flipBool):
            img = cv2.flip(img, 1)
        img = img[BoundingArray[0]:BoundingArray[0]+BoundingArray[2],
                  BoundingArray[1]:BoundingArray[1]+BoundingArray[3]]
        _, thresh = cv2.threshold(img, thresh_val, 255, cv2.THRESH_BINARY_INV)
        perc = cv2.countNonZero(thresh)/(np.size(img))*100
        return perc


# %% Read the data and apply the function

# Read the data and extract the last 990 photos
data_filtered = pd.read_csv('EntryFile.txt', sep='\t', header=0)
# data_filtered = data.query('Folder == "13-03-19"'); # Uncomment this line to analyze the final files only
# data_filtered.index = np.arange(data_filtered.size); # Use only when filtering the data

# %% Create an empty array of NaN of 990*3 (percentage,filename,camera)
index = np.arange(data_filtered.size)
columns = ['percentage', 'filename', 'camera', 'QRcode']
final = pd.DataFrame(index=index, columns=columns)


# %% Define starting variables
BoundingArray = np.array([4390, 1518, 10910, 1446])
thresh_val = 140
path = 'F:\\EPPN_JRA2_201903\\Images\\'

# %% Create files list
files = [f for f in glob.glob(path + "**/*.jpg", recursive=True)]

# %%   1. Flip if needed
for i in range(data_filtered.size):
    print('Analysis: image # ' + str(i+1))
    folder = '2019-' + \
        data_filtered.Folder[i][3:5] + '-' + data_filtered.Folder[i][:2]
    img_path = 'F:\\EPPN_JRA2_201903\\Images\\Camera ' + data_filtered.Camera[i] + '\\' \
        + folder + '\\' + data_filtered.Filename[i]
    flipBool = data_filtered.verticalFlip[i]
#   2. Apply the function
    final.percentage[i] = area_percentage(
        img_path, thresh_val, flipBool, BoundingArray)
    final.filename[i] = data_filtered.Filename[i]
    final.camera[i] = data_filtered.Camera[i]
    final.QRcode[i] = data_filtered.QR[i]

# Export the final dataset to csv
final.to_csv(r'final.csv', '\t')
