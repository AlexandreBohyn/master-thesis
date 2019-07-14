# %% Import packages
import cv2
import glob
import numpy as np
import pandas as pd
import os

# %% Define global variables
path = 'F:\\EPPN_JRA2_201903\\Images\\'
BoundingArray = np.array([4390, 1518, 10910, 1446])
thresh_val = 140

# %% Create the files list
files = pd.DataFrame([os.path.split(f)
                      for f in glob.glob(path + "**/*.jpg", recursive=True)])
files.columns = ['Filepath', 'Filename']
final = pd.DataFrame(index=range(len(files)), columns=[
                     'filename', 'QR', 'area'])
entryFile = pd.read_csv('Image analysis\EntryFile.txt', sep='\t', header=0)
files = pd.merge(files,
                 entryFile[['Filename', 'verticalFlip', 'QR']],
                 left_on='Filename',
                 right_on='Filename',
                 how='left')
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
    if img is None:
        return float('nan')
    else:
        if(flipBool):
            img = cv2.flip(img, 1)
        img = img[BoundingArray[0]:BoundingArray[0]+BoundingArray[2],
                  BoundingArray[1]:BoundingArray[1]+BoundingArray[3]]
        _, thresh = cv2.threshold(img, thresh_val, 255, cv2.THRESH_BINARY_INV)
        perc = cv2.countNonZero(thresh)/(np.size(img))*100
        return perc


# %% Loop through all the files
for i in range(len(files)):
    print("Analyzing image " + str(i+1))
    final.filename[i] = files.Filename[i]
    final.QR[i] = files.QR[i]
    Filepath = files.Filepath[i] + "\\" + files.Filename[i]
    final.area[i] = area_percentage(
        Filepath, thresh_val, files.verticalFlip[i], BoundingArray)


# %% Save the resutl to csv file
final.to_csv(r'full_analysis.csv', '\t')
