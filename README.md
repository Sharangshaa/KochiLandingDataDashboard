Shark Landing Data Dashboard – Kochi

A Shiny app presenting mock data on elasmobranch (shark) landings at Kochi, India.

Overview

This dashboard visualises and summarises species-level catch data for sharks landed at Kochi. The data were collected during 2021–2023, anonymised, randomised and noise-added for demonstration purposes.
The app is built with R and Shiny, using interactive visualisations (via Plotly) to explore species composition, size-class distribution, sex ratios, and other landing metrics.

Features

Sidebar navigation to switch between views.

Summary metrics (e.g., total individuals, species count, family count, sampling days).

Visualisations such as:

Number of species in each Red List category

Number of species in each family

Sex ratios of the top 5 species

Size-class distributions of the top 5 species

A searchable table listing morphometrics of species present in the dataset.

Data

Period: 2021–2023

Location: Kochi (India)

The dataset has been anonymised, randomised, and noise-added in order to protect privacy and for demonstration only.

The data should not be used for regulatory or scientific conclusions without validating with original raw data.

Purpose & Scope

Demonstrate how species-level catch/landing data for sharks can be visualised and summarised.

Enable users (researchers, managers, educators) to explore patterns in shark landings: taxonomic composition, size structure, sex ratio, etc.

Provide an example of using R/Shiny + Plotly for interactive dashboard development in fisheries/eco-monitoring contexts.

Note: Because this is mock/processed data, the patterns shown do not reflect actual fishery conditions.

Installation & Deployment

Clone or download this repository.

Ensure you have R (version ≥ 4.x) installed.

Install required packages (e.g., shiny, plotly, dplyr, DT, etc.)

install.packages(c("shiny", "plotly", "dplyr", "DT", etc))


Run the app locally:

library(shiny)
runApp("path/to/app")


To deploy, you can publish to shinyapps.io
 or host on your own Shiny server.

File Structure
/app/
  app.R      
  data/
    landing_data.csv   – processed mock landing data    
  README.md        – this file  

Usage Instructions

Open the app; you will see summary metrics on the home page.

Use the sidebar to navigate through different tabs:

Data Insight: Explore visualisations and tables.

Use filters (if available) to focus on particular species/families.

Hover over chart points to view details (thanks to Plotly interactivity).



Credits & Acknowledgements

Data collected by Sharang (me) while working as Project Assistant at Wildlife Conservation Society India (WCS-India) during 2021–2023.

Visualisation and app framework built using R and Shiny.

Thanks to the WCS-India team for guidance and support during data collection.

Disclaimer

The dataset has been anonymised, randomised, and noise-added to protect privacy and for demonstration purposes. The results should not be taken as definitive scientific or management advice.
