# -*- coding: utf-8 -*-
"""
File with additional functions for the convex hull area of the pictures
"""
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
 


import cv2
import numpy as np

img = cv2.imread('wallet.jpg',)

cv2.namedWindow('image', cv2.WINDOW_NORMAL)
cv2.imshow('image',img)
cv2.waitKey(0)
cv2.destroyAllWindows()
 
gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
_, thresh = cv2.threshold(gray, 140, 255, cv2.THRESH_BINARY_INV)
thresh = cv2.GaussianBlur(thresh, (3,3), 0)

cv2.imshow('image',thresh)
cv2.waitKey(0)
cv2.destroyAllWindows()

contours, hierarchy = cv2.findContours(thresh,cv2.RETR_EXTERNAL,cv2.CHAIN_APPROX_SIMPLE)
cv2.drawContours(img, contours, 0, (0,255,0), 2)

cv2.imshow('image',img)
cv2.waitKey(0)
cv2.destroyAllWindows()

# create hull array for convex hull points
hull = []
 
# calculate points for each contour
for i in range(len(contours)):
    # creating convex hull object for each contour
    hull.append(cv2.convexHull(contours[i], False))
    
# create an empty black image
drawing = np.zeros((thresh.shape[0], thresh.shape[1], 3), np.uint8)
 
# draw contours and hull points
for i in range(len(contours)):
    color_contours = (0, 255, 0) # green - color for contours
    color = (255, 0, 0) # blue - color for convex hull
    # draw ith contour
    cv2.drawContours(drawing, contours, i, color_contours, 1, 8, hierarchy)
    # draw ith convex hull object
    cv2.drawContours(drawing, hull, i, color, 1, 8)

cv2.imshow('image',drawing)
cv2.waitKey(0)
cv2.destroyAllWindows()