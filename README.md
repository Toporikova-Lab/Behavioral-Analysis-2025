# Behavioral-Analysis-2025

This repository contains code for collection and analysis of spider locomotor activity

## Experiment Description

Using the DAMSystem3 data collection software, we are monitoring the locomotor activity of spiders in relation to light exposure. An automated timer controls the light cycle, turning the light on from 12:00 AM to 12:00 PM and off from 12:00 PM to 12:00 AM. During these periods, the spiders’ activity is being recorded by a monitoring system that tracks the number of times each spider crosses a hole in its vial.

## Required R Libraries

This project depends on the following R packages:

-   ggplot2
-   lubridate
-   dplyr

## Instructions for Using the Daily Script

*NOTE: these instructions are written with Windows in mind*

First, you will need to install the following software on your device, if you don't already have it installed:

-   git: <https://git-scm.com/downloads>
-   R: <https://cran.rstudio.com/>
-   RStudio: <https://posit.co/download/rstudio-desktop/>

### Getting the Repository on Your Machine

Start by cloning the github repository to your computer. In the directory where you want your repository, open a terminal and run this command:

```         
git clone https://github.com/Toporikova-Lab/Behavioral-Analysis-2025
```

If git prompts you for authentication, use your browser to sign into github.

If the clone was successful, you should see this in your terminal:

![Windows terminal window with the git clone command having been run](https://github.com/user-attachments/assets/61564ab2-4585-4ecf-9487-ab96da6e61d3)

### Project Setup

After the repository has been cloned to your computer, navigate to the directory it was cloned into. This should be called **Behavioral-Analysis-2025/**. From this directory, open the **Behavioral-Analysis-2025.Rproj** file in RStudio.

You should see a screen that looks something like this:

![Newly opened project in RStudio](https://github.com/user-attachments/assets/ce9e0d8c-0d4c-4a9d-abac-53a8190d35dc)

To install the required R packages, run this command in the interactive console on the left:

```         
install.packages("tidyverse")
```

In the bottom right file viewer, you should see a file called **daily_script.R**. Open this file in the editor.

Locate the monitor data file that you want to use, and copy its file path.

Paste this path in place of the "--FILENAME HERE--" at the top of the **daily_script.R** file. Make sure to use forward slashes instead of backslashes in the path.

In place of the "--SUBFOLDER NAME HERE--", put the experiment label. It should be of the form:

`[ID]_[start yyyy-mm-dd]_[data collection mm-dd]_[monitor number]`

For example, this experiment that was run on spiders of species *Larinioides cornutus* from 24 September 2024 to 10 October 2024 in Monitor #2 should be labeled `LC_2024-09-24_10-10_2`.

The top of your **daily_script.R** file should look something like this:

![Lines of the R file with the data path and subfolder name set](https://github.com/user-attachments/assets/da9a8ee6-631d-4261-adfa-c2b7a73066ab)

### Running the Script

To run the script, use `Ctrl` + `Shift` + `Enter` while in the editor.

The script will generate text in your R console, as well as raster plots in a folder named **output/generated_plots/**.

"Active" means that the spider moved within the last 24 hours before the data was collected, and so is almost certainly alive.

"Inactive" means that the spider has not moved in this period, and might be dead. Check the generated raster plot for further confirmation.

Example output:

![Console output listing each tube as active, inactive, or empty; as well as a folder with 13 generated raster plots](https://github.com/user-attachments/assets/dd54181a-e6f9-447e-a799-914567cf8e8a)

One of the generated raster plots might look something like this:

![Raster plot with ZT (hours) on the x-axis and Day on the y-axis showing seven days of constant darkness followed by 9 days of light-dark cycle](https://github.com/user-attachments/assets/a8b6644a-8f87-4faa-8cb4-4f84c4ad8837)
