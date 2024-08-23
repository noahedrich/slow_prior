# Task
Contains the task code for the project: **An inductive bias for slowly changing features in human reinforcement learning**. 


## Contents
*  `code`: This folder contains the code for the 'slowness prior' task. The task was built using the [jsPsych](https://github.com/jspsych/jsPsych) library version version 6.1.0. 

*  `the_gem_hunter_task.jzip`: The JATOS study summary file. 

## How to run the study
The task is set up to be hosted through [JATOS](https://www.jatos.org/), so to try it out you will need to: 

1. [Install JATOS](https://www.jatos.org/Installation.html) and run it locally. 
2. From within the JATOS GUI in your browser, import the file `the_gem_hunter_task.jzip` (do not unzip it). 
3. Click 'Run'. 
Done :) 


If you don't want to run it through JATOS, you need to remove the code pertaining to JATOS from the `index.html` file and then run the task from a local server. For example, set up a local server using python by running `python3 -m http.server` from the command line, open [localhost:8000/](localhost:8000/) in a browser (Chrome & Firefox are best) and then navigate to the file `index.html`.
(Or use any other server of your choice.)


## Resources
*  The stimuli are constructed by combining the shapes from the [Validated Circular Shape space](https://osf.io/d9gyf/) with colours based on CIELAB colour space.


## Note
On occasion, the stimuli shapes do not display correctly. The debrief questions at the end of the experiment include a check of whether this was the case. Make sure to check the responses to this question when collecting data with this task. 


## License
All code in this repository was written by Noa L. Hedrich at the Max Planck Institute for Human Development, Berlin, Germany and is licensed under the Creative Commons Attribution NonCommercial ShareAlike 4.0 International Public License.
Please see https://creativecommons.org/licenses/by-nc-sa/4.0/ for details.
