# Install required packages
# !pip install tensorflow==2.18.0 opencv-python numpy matplotlib requests retina-face deepface

import cv2
import numpy as np
from pathlib import Path
import matplotlib.pyplot as plt
import os
from retinaface import RetinaFace
from deepface import DeepFace

# Load an image
image_path = "faces/example_image.jpg"  # Replace with your image path
img = cv2.imread(image_path)

# Plot the original image
plt.imshow(cv2.cvtColor(img, cv2.COLOR_BGR2RGB))
plt.show()

# Detect faces
faces = RetinaFace.detect_faces(image_path)

# Draw rectangles around detected faces
for face in faces.values():
    facial_area = face['facial_area']
    x, y, w, h = facial_area
    cv2.rectangle(img, (x, y), (w, h), (127, 255, 0), 10)
    
    # Crop the face from the image
    face_img = img[y:h, x:w]
    
    # Analyze the face for gender
    analysis = DeepFace.analyze(face_img, actions=['gender'], enforce_detection=False)
    gender = analysis[0]['dominant_gender']
    
    # Annotate the image with the detected gender
    cv2.putText(img, gender, (x, y - 10), cv2.FONT_HERSHEY_SIMPLEX, 0.9, (36,255,12), 2)

# Plot the image with detected faces and gender
plt.imshow(cv2.cvtColor(img, cv2.COLOR_BGR2RGB))
plt.show()

# Save the annotated image
output_path = "faces/annotated_image.jpg"  # Replace with your desired output path
cv2.imwrite(output_path, img)
