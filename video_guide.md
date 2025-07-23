#  Spider Video Activity Analysis Guide

## Overview

This repository contains code and instructions for processing and analyzing spider locomotor activity using **video recordings**. Videos are collected under controlled light/dark cycles to assess behavioral rhythms. The analysis pipeline includes:
1. **Binarizing activity** from video timestamps  
2. **Generating raster plots** to visualize activity across days  
3. **Computing Lomb-Scargle periodograms** to estimate free-running periods

---

# Experimental Context

Spiders are housed in individual vials and filmed continuously under a 12:12 Light:Dark (LD) cycle. Lights are **ON from 8:00 AM to 8:00 PM (ZT 0–12)** and **OFF from 8:00 PM to 8:00 AM (ZT 12–24)**.

##  Required Software

Before running any of the analysis scripts, make sure you have the following software installed:

### R and RStudio

- **R** 
   Download: [https://cran.rstudio.com/](https://cran.rstudio.com/)

- **RStudio**
   Download: [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)

###  Box Drive (BoxTools)

- **Box Drive** 
   Download: [https://www.box.com/resources/downloads](https://www.box.com/resources/downloads)

- Once installed and signed in, you can navigate to your Box folders just like any regular folder on your computer (e.g., `C:/Users/YourName/Box/YourFolderName/`)

##  Step-by-Step Analysis Guide

###  Step 1: Binarize Your Video Data

To begin the analysis, you'll first convert your video timestamps into binary activity data.

1. **Download** the script: `VIDEO_BINARIZED.R`  
   (You can find it in this repository)

2. **Open** the file in RStudio.

3. **Paste your folder path** into the `full_path <-` variable at the bottom of the script.  
   Example:  
   ```r
   full_path <- "C:/Users/yourname/Box/Summer2025_Circadian_Students/Spider behavioral data and analysis/Video recording data/Raw data/filename"
It should look something like this 
<img width="1877" height="1393" alt="image" src="https://github.com/user-attachments/assets/7829d7a1-2429-4253-b6c2-4a0854f5d253" />
The binarized csv file will be automatically saved in Box Drive 

###  Step 2: Generate a Raster Plot (Actogram)

Once your video data has been binarized, the next step is to visualize the spider's activity as a raster plot (actogram).

1. **Download** the script: `video rasterplot.R`  
   (Located in this repository)

2. **Open** the file in RStudio.

3. **Set the path** to your binarized CSV file at the bottom of the script:  
   Example:  
   ```r
   full_path <- "C:/Users/yourname/Box/Summer2025_Circadian_Students/Spider behavioral data and analysis/Video recording data/Raw data/filename"
It should look something like this: 
<img width="3427" height="1231" alt="image" src="https://github.com/user-attachments/assets/61c7a46a-b562-454e-8ccb-f55f61ecdcea" />

###  Step 3: Generate a Lomb-Scargle Periodogram

To analyze the periodicity of your spider’s activity, use the Lomb-Scargle periodogram script.

1. **Download** the script: `lombscargle_video.R`  
   (Included in this repository)

2. **Open** the file in RStudio.

3. **Set the path** to the folder that contains your binarized CSV file:  
   Example:  
   ```r
   full_path <- "C:/Users/yourname/Box/Summer2025_Circadian_Students/Spider behavioral data and analysis/Video recording data/Raw data/filename"

   It should look something like this:
   <img width="3440" height="1302" alt="image" src="https://github.com/user-attachments/assets/ca6d742b-18dc-4b37-8b6e-9cfd944895cc" />
**Both the Raster Plots and Lomb-Scargle Periodograms are automatically saved as .png files to their corresponding folders on your Box Drive.**
<img width="634" height="1057" alt="image" src="https://github.com/user-attachments/assets/342d5ba6-3889-40eb-800a-dc2450f3661d" />
<img width="1890" height="1046" alt="image" src="https://github.com/user-attachments/assets/a7232008-b76e-472e-b3ea-020d8c3af3cb" />


