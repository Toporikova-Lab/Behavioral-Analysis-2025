# Behavioral-Analysis-2025

This repository contains code for collection and analysis of spider locomotor activity

## Experiment Description

Using the DAMSystem3 data collection software, we are monitoring the locomotor activity of spiders in relation to light exposure. An automated timer controls the light cycle, turning the light on from 12:00 AM to 12:00 PM and off from 12:00 PM to 12:00 AM. During these periods, the spiders’ activity is being recorded by a monitoring system that tracks the number of times each spider crosses a hole in its vial.

## Required R Libraries

This project depends on the following R packages:

-   ggplot2 (part of tidyverse)
-   cowplot

## Instructions for Using the Daily Script

*NOTE: these instructions are written with Windows in mind*

Start by cloning the github repository to your computer. If you do not have git installed, it can be downloaded from <https://git-scm.com/downloads>. In the directory where you want your repository, open a terminal and run this command:

```         
git clone https://github.com/Toporikova-Lab/Behavioral-Analysis-2025
```

After the repository has been cloned to your computer, navigate to the directory it was cloned into. This should be called **Behavioral-Analysis-2025/**. From this directory, open the **Behavioral-Analysis-2025.Rproj** file in RStudio.

To install the required R packages, run this command in the interactive console:

```         
install.packages(c('ggplot2', 'cowplot'))
```

Open the **daily_script.R** file in the editor.

Locate the monitor data file that you want to use, and copy its file path.

Paste this path in place of the "--FILENAME HERE--" at the top of the **daily_script.R** file. Make sure to use forward slashes instead of backslashes in the path.

To run the script, use `Ctrl` + `Shift` + `Enter` while in the editor.

The script will generate text in your R console, as well as raster plots in a folder named **output/generated_plots/**.
