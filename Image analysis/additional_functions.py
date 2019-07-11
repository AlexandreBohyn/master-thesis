# -*- coding: utf-8 -*-
"""
File with additional functions for the convex hull area of the pictures
"""
import cv2
import numpy as np

def PolyArea2D(pts):
    """Compute the area of a 2D polygon formed by a set of points

    Parameters:
    pts (array): Array of the vertices of the polygon

    Returns:
    float: area of the polygon

   """
    lines = np.hstack([pts,np.roll(pts,-1,axis=0)])
    area = 0.5*abs(sum(x1*y2-x2*y1 for x1,y1,x2,y2 in lines))
    return area;

def convex_hull_area(thresh_img,thresh_val):
    """Compute the area of the convex hull polygon formed by a set of points

    Parameters:
    thresh_img (array): Array of the pixel values of the image
    thresh_val (int): value of the threshold for the binarization

    Returns:
    int: area of the polygon

   """
    img_canny = cv2.Canny(thresh_img,thresh_val,thresh_val*2);
    contours, hierarchy = cv2.findContours(img_canny,cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE);
    hull_list = [None]*(len(contours));
    hull_area = [None]*(len(contours));
    for i in range(len(contours)):
        hull_list[i] = cv2.convexHull(contours[i]);
        hull_area[i] = cv2.contourArea(hull_list[i]);
    return sum(hull_area);

def area_percentage(img_path,thresh_val,flipBool,BoundingArray):
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
    img = cv2.imread(img_path,0);
    if(flipBool):
        img = cv2.flip(img, 1 );
    img = img[BoundingArray[0]:BoundingArray[0]+BoundingArray[2],\
              BoundingArray[1]:BoundingArray[1]+BoundingArray[3]];
    _,thresh = cv2.threshold(img,thresh_val,255, cv2.THRESH_BINARY_INV);
    perc = cv2.countNonZero(thresh)/(np.size(img))*100;
    return perc;
