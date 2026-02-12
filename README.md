# STA380GroupProject
# Bootstrap Analysis of Physiological Traits in Florida Scrub Lizards

## Project Overview
This interactive R Shiny application applies **Two-Sample Bootstrap methods** to investigate physiological differences in the Florida Scrub Lizard (*Sceloporus woodi*). 

Instead of relying on parametric assumptions, this tool uses resampling techniques to estimate the sampling distribution of the **difference in means** between biological groups. This allows for a robust validation of hypotheses regarding:
1.  **Sexual Dimorphism:** Comparing Snout-Vent Length (SVL) between Males and Females.
2.  **Thermoregulation & Conservation:** Comparing Body Temperature between Gravid (pregnant) and Non-Gravid females to understand the physiological costs of reproduction in the face of climate change.

## Key Features
-   **Comparative Inference:** Visualizes the bootstrap distribution of the difference in means ($\mu_1 - \mu_2$).
-   **Interactive Controls:** Users can dynamicall adjust the number of bootstrap iterations ($B$), random seed, and hypothesis to test.
-   **Statistical Rigor:** Displays 95% Confidence Intervals to visually assess statistical significance.

## Dataset
Data sourced from:  
Gainsbury, Alison (2021). *Size, sex, reproductive status and body temperature dataset*. Dryad. [https://doi.org/10.5061/dryad.dbrv15f00](https://doi.org/10.5061/dryad.dbrv15f00)

## Technologies
-   R
-   R Shiny
-   `tidyverse` / `ggplot2`
