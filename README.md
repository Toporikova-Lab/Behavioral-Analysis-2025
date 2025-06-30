# Behavioral-Analysis-2025

This repository contains code for collection and analysis of spider locomotor activity

## Experiment Description

Using the DAMSystem3 data collection software, we are monitoring the locomotor activity of spiders in relation to light exposure. An automated timer controls the light cycle, turning the light on from 12:00 AM to 12:00 PM and off from 12:00 PM to 12:00 AM. During these periods, the spiders’ activity is being recorded by a monitoring system that tracks the number of times each spider crosses a hole in its vial.

## Required R Libraries

This project depends on the following R packages:

-   tidyverse
    -   ggplot2
    -   dplyr
    -   lubridate
    -   stringr
-   lomb
-   gridExtra

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
install.packages(c('tidyverse', 'gridExtra', 'lomb'))
```

In the bottom right file viewer, you should see a file called **daily_script.R**. Open this file in the editor.

Locate the monitor data file that you want to use, and copy its file path. This can be quickly done in RStudio by opening the data file in the editor, right clicking on the tab name at the top, and clicking "Copy Path":

Set this path as the `filename` variable at the top of the **daily_script.R** file. Make sure to use forward slashes instead of backslashes in the path.

You can also set whether you want to only generate actograms, or whether you want to generate combined plots that also include periodograms. If you want to generate combined plots, set `rasterplots_only` to FALSE. If you only want actograms, set it to TRUE.

If you are generating combined plots, set the `section2_start_day` variable. For example, if you are looking at this LD to DD experiment, day 7 is the first day of DD:

![Actogram showing 6 days LD and the 5 days DD, with an arrow pointing to day 7](https://github.com/user-attachments/assets/9cdd79ee-9431-4247-bfa6-3dbb19a1fd0f)

So set the variable to 7.

If you are only generating actograms, then this variable is unused, so setting it does not matter.

The top of your **daily_script.R** file should look something like this:

![Definitions for filename and section2_start_day variables](https://github.com/user-attachments/assets/50289d17-ae12-47f8-a400-1e50424551ef)

### Running the Script

To run the script, use `Ctrl` + `Shift` + `Enter` while in the editor.

The script will generate text in your R console, as well as combined actograms/periodograms in a folder named **output/**.

If you set `rasterplots_only` to FALSE, the program will also generate two more csv files with the data split into the two sections you defined.

"ACTIVE" means that the spider moved within the last 24 hours before the data was collected, and so is almost certainly alive.

"INACTIVE" means that the spider has not moved in this period, and might be dead. Check the generated plot for further confirmation.

Example output:

![Console output listing each tube as active, inactive, or empty; as well as a folder with 13 generated plots](https://github.com/user-attachments/assets/dd54181a-e6f9-447e-a799-914567cf8e8a)

One of the generated combined plots might look like this:

![{8342BC35-C404-4F3C-A1C4-9D446612DF08}](https://github.com/user-attachments/assets/e51465a2-46d5-4138-9c26-0a0f4c99bd3b)
