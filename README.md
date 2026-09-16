# Interactive Photosuynthesis App

Shiny application that allows simulating steady-state response curves using the 
C3 and C4 photosynthesis models by Yin & Struik (2009, NJAS). 

The application runs by sourcing the file `RUNME.R` within an R session. An 
internet connection will be required the first time the application runs if 
dependencies are not available on the local environment. A step-by-step setup
is available in `Setup.md`. 

The app should be fairly intuitive but it can be overwhelming at the start. I 
have some description of the buttons and functionality below.

Early development of this app was funded by the Biosolar Cells Program and I do
all of this under employment by Wageningen University & Research. Check LICENSE
for details on conditions to use and adapt it.

## General

The app works on scenarios. A scenario is a response curve defined by:

- The environmental factor that drives the curve  
- The type of photosynthesis system (C3 or C4) 

Within a scenario we can modify all relevant inputs:

- The parameter values of the photosynthesis system
- The environmental conditions being modified (only the one driving the curve 
can have multiple values)


## Managing scenarios

The menus on the top are to manage scenarios (add, remove, rename, etc). 

### Creating a scenario

Click on the button Add. This will open a new menu where you can edit:

- The name of the scenario
- The type of photosynthesis system
- The type of environmental factor that drives the curve

Once you have defined then name and type of scenario, click on the button Create.
This will do two things:

- Add the scenario to the list of scenarios in the current session (the dropdown menu)
- Activate this scenario (i.e., select it in the drop down menu)

### Managing scenarios

You can delete the active scenario with the button Remove.

You can rename the active scenario with the button Rename.

You can reset all the environmental values and parameters of the active scenario
with the button Reset.

If you want to start again from scratch (without having to restart the app) you
can just click on Remove All.

### Saving and loading scenarios

You can save the active scenario by clicking on the button Save as. This will 
create a text file that contains all the environmental variables and parameters
of the scenario. If you have also calculated the scenario (see below) it will
also store all the values calculated at the end as a comma-separated table. You
can always copy-paste that table into any spreadsheet software.

These text files are also useful because you can load any scenario that you ran
before into the app using the button Open. Make sure you did not manually edited
the file as it may have become corrupted and will load incorrectly.

### Calculating a scenario

You can run calculations on the active scenario (this means you calculate
scenarios one at a time).

First, you can edit any environmental condition and/or parameter in the active
scenario, by selecting from the dropdown menus and editing the values in the
textboxes.

To save the changes you must always click on the button Update from the 
corresponding menu. If you make mistakes and want to reset the value of a
parameter or environmental factor to the default, click on the corresponding
Reset button (if you want to reset all values the Reset button on the top is
easiest).

Once you have defined all the relevant values for the scenario, you can click
on the button Calculate to do all the calculations for your scenario. This will
add the scenario to the list of curves (or if you already did a calculation, the
curve will be updated).

### Visualizing a scenario

All scenarios that have been calculated will be added to the list under the 
header Visualize Curves. To avoid cluttering, you can turn on and off the
visualization for each scenario by click on its name. 

The app can generate up to 6 figures simultaneously (by default only one). You
can increase the number of figures with the box on the right (No. figures). For
each figure you can edit both the X and Y axis by selecting from a dropdown menu
with all the variables generated in the calculation. 

Every time that you add a new scenario or re-calculate a scenario, the curves
will update. Also, color scales and legends are added automatically. Note that
if you turn off a scenario, all colors will be reassigned to the currently 
visualized scenarios, so you may have a scenario changing colors.

Finally, you can right click on any plot and save it or copy (just like you
would with a picture on a website).



