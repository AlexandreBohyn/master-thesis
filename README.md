# MASTER THESIS CODE

Code for the data processing and data analysis of my thesis: Comparison of statistical methods for analyzing data coming from a high throughput phenotyping platform: Aeroponics at the UCL.

The code is presented as a map of the different scripts, then a detailed explanation of the folders, scripts, datasets and what they contain.

## Project roadmap

In this section, the project roadmap is presented in different sections that correspond to experimental planning, data processing, data analysis, data visualization. In each section the analysis performed, with the intended goals, is described. A more detailed explanation of the scripts is presented the *Scripts explanation* section.

## Scripts explanation

Before starting, it is important to specify that data visualization from each part of the project (design, results, ...) are always created at the end of the related script and not in a separate script on the side. This section explains each script in details, specifying which data are used as inputs, are outputted, and which plot are being created.

### DESIGN

In this script, the JMP design for the experience is uploaded and replicated to suit the platform's size. It is then updated after the germination took place, and after the seeds are placed onto the platform. The planned design, as well as the updated design are created as tables. Two figures are plotted: the layout of the design in the tanks and the repartition of genotypes among strips, tanks and position.

Inputs:

- [Table] JMP design
- [Table] Germination info (weight, number of germ. seeds)
- [Table] Border runs and NA runs on the platform

Outputs:

- [Table] planned_design_table
- [Table] design_table
- [Figure] design_layout
- [Figure] design_repartition

### WEIGHT
