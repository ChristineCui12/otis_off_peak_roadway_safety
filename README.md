# Off-Peak Roadway Safety in Philadelphia
### When Empty Roads Become Deadly

> *"Congestion may contribute to fender benders, but excess capacity might be contributing to catastrophic outcomes."*

A predictive modeling and planning tool built for the **City of Philadelphia's Office of Transportation and Infrastructure Systems (OTIS)**, Office of Multimodal Planning — Spring 2026.

---

## Live Application

**[Launch the Roadway Safety Explorer →](https://chihyunkim.github.io/otis_off_peak_roadway_safety/html/landing.html)**

Explore predicted speeding risk across Philadelphia's road network, simulate the effect of road design interventions, and compare outcomes by time of day.

## Project Report

**[Read the Full Analysis Report →](https://chihyunkim.github.io/otis_off_peak_roadway_safety/PROJECT_WRITEUP.html)**

Complete methodology, exploratory data analysis, model documentation, and planning recommendations.

---

## The Problem

Philadelphia's multilane arterials are designed for peak-hour capacity. Once traffic thins after 10 PM, these same roads become unobstructed corridors where a meaningful share of drivers exceed posted speed limits by 10+ mph. Evening hours (8 PM–3 AM) represent only **14% of daily traffic** but account for **46% of pedestrian KSI (Killed or Seriously Injured) crashes** — a 3× overrepresentation.

This project investigates the structural causes of off-peak speeding and builds a data system to identify which road segments are most at risk — and which interventions would help most.

---

## What We Built

### 1. Predictive Model
A **Random Forest regression** trained on 9,049 segment-period observations predicts the percentage of vehicles speeding during each of four daily time periods. The model achieves **R² = 0.920** on a held-out test set, confirming that speeding is structurally determined — not random driver behavior.

**Key predictors:** time of day, road classification, lane width, curb-to-curb width, bike lane status, traffic calming, sidewalk presence, parking, transit access, crash history.

### 2. Scenario Simulation
24 pre-computed intervention scenarios (lane reductions, bike lane upgrades, traffic calming) estimate the predicted change in speeding rate for each eligible segment across all four time periods.

### 3. Interactive Planning Application
A web application allows OTIS planners to explore risk and evaluate interventions at the segment level — without requiring statistical expertise.

---

## Repository Structure

```
├── PROJECT_WRITEUP.Rmd       # Full analysis report (R Markdown source)
├── PROJECT_WRITEUP.html      # Compiled report (rendered HTML)
├── modeling/
│   └── modeling_workflow.R   # Model training and evaluation pipeline
├── exploratory_data_analysis/ # EDA notebooks and crash analysis
├── joins/                    # Spatial join scripts
├── data_check/               # Data validation scripts
├── html/                     # Web app pages (served by GitHub Pages)
├── css/ / js/ / assets/      # Web app styles, logic, and data
└── DATA_SOURCES.md           # Data catalog and Box file IDs
```

---

## Data Sources

All raw data are stored in a shared Box repository. See [`DATA_SOURCES.md`](DATA_SOURCES.md) for the complete catalog. Primary sources:

| Dataset | Source |
|---|---|
| Speed & volume data | Philadelphia County Speed CSVs (PennDOT / Box) |
| Crash records (2020–2024) | PennDOT Statewide / OpenDataPhilly |
| Street centerlines | OpenDataPhilly |
| Road geometry | DVRPC Complete Streets / RMS Admin |
| Bike network | OpenDataPhilly |
| Traffic calming | PennDOT Open Data |
| Transit stops | SEPTA |

---

## Team

**MUSA 801 Practicum — Spring 2026**
Weitzman School of Design, University of Pennsylvania

| Name | Role |
|---|---|
| Chi-Hyun Kim | Web application, modeling |
| Kavana Raju | Crash analysis, data wrangling |
| Demi Yang | Network construction, road geometry |
| Christine Cui | Feature engineering, traffic calming, scenario design |

**Client:** Office of Transportation and Infrastructure Systems (OTIS), City of Philadelphia
