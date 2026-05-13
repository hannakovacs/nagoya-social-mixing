# Nagoya Urban Social Mixing Analysis

This repository contains the data processing, spatial feature engineering, and regression analysis scripts used in my thesis: **Accessibility, Livability, and Attractivity of Nagoya**.

The goal is to investigate how the spatial distribution of urban amenities and transit accessibility affect social mixing between different income groups across different temporal categories (Work, Daytime Leisure, Evening Leisure, and Weekend). Spatial econometrics, specifically Spatial Error Models and Spatial Lag Models are employed to account for spatial autocorrelation.

# Repository Structure
The analytical pipeline is divided into the following R scripts: 
* `[01_data_cleaning_mobility.R]`: cleanes raw mobility data, keeps only real stops, and identifies weekends.
* `[02_identifying_coordinates_and_downloading_income.R]`: identifies real coordinates of the anonymized mobility data and downloads municipality level rent as an income proxy.
* `[03_homes_and_attractivity_pois.R]`: identifies home and work locations of each individual, downloads amenities that represent Attractivity.
* `[04_transit.R]`: loads raw transit data, GTFS files for buses, and railways from OpenStreetMap.
* `[05_ala_scores.R]`: calculates Livability, Attractivity, and Accessibility Scores. Normalizes the scores, applies  Gaussian Kernel smoothing, and plots the spatial distributions of each score.
* `[06_entropy]`: identifies different temporal categories, calculates entropy (social mixing index) in each grid cell for each temporal category.
* `[07_air_pollution]`: loads Air Quality Monitoring Station data, calculates Environmental Score, performs Inverse Distance Weighting.
* `[08_regression]`: calculates Moran's I, Lagrange Multiplier diagnostics, Spatial Error Models, and Spatial Lag Model.

# Data sources
* **Human Mobility:** YJMob100K dataset provided by Yahoo Japan Corporation, containing longitudinal GPS trajectories of 100,000 anonymized users.
* **Urban Amenities & Rail Network:** OpenStreetMap (OSM), used for extracting Points of Interest (POIs) to calculate Livability and Attractivity scores, as well as mapping subway and railway stations.
* **Bus Transit Network:** Standardized GTFS-JP data sourced from the Public Transportation Open Data Center, covering both commercial routes and community buses.
* **Administrative & Socioeconomic Data:** The e-Stat portal (official statistics portal of Japan) for municipality-level boundaries and average residential rent prices (utilized as a proxy for income groups).
* **Environmental Quality:** The Environmental Numerical Database of the National Institute for Environmental Studies (NIES), Japan, providing continuous air quality measurements ($NO_2$, $O_x$, $PM_{2.5}$).

