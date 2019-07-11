# MASTER THESIS CODE

Code for the data processing and data analysis of my thesis: Comparison of statistical methods for analyzing data coming from a high throughput phenotyping platform: Comparison of statistical methods and designs for a high throughput phenotyping experiment.

The code is presented as a map of the different scripts, then a detailed explanation of the folders, scripts, datasets and what they contain.

## Project roadmap

In this section, the project roadmap is presented in different sections that correspond to experimental planning, data processing, data analysis, data visualization. In each section the analysis performed, with the intended goals, is described. A more detailed explanation of the scripts is presented the _Scripts explanation_ section.

## Scripts explanation

Before starting, it is important to specify that data visualization from each part of the project (design, results, ...) are always created at the end of the related script and not in a separate script on the side. This section explains each script in details, specifying which data are used as inputs, are outputted, and which plot are being created.

### DESIGN

In this script, the JMP design for the experience is uploaded and replicated to suit the platform's size. It is then updated after the germination took place, and after the seeds are placed onto the platform. The planned design, as well as the updated design are created as tables. Two figures are plotted: the layout of the design in the tanks and the repartition of genotypes among strips, tanks and position.

Inputs:

- [Table] JMP design
- [Table] Border runs and NA runs on the platform

Outputs:

- [Dataset] Design table (tank,strip,position,QRcode,planned genotype,real genotype)
- [Table] Design repartition
- [Figure] Still and mowing tanks design layouts
- [Figure] Genotype distribution among strips, positions and tanks

### METRICS

In this script, the different metrics characterizing the plant growth are added to the design table.
The metrics are: dry and fresh weight for the root and leaf system and area of the root system. The area metric comes from the `area_percentage.py` script in the *Image analysis* folder.

Inputs:

- [Dataset] Design table
- [Table] Weights tables (dry and fresh weights)
- [Table] Area percentage file (.txt)

Outputs:

- [Dataset] Data table (all metrics with the related genotype)
- [Figure] Correlation plot of all the metrics
