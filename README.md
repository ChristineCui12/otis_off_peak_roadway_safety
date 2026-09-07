# Off-Peak Roadway Safety in Philadelphia
### When Empty Roads Become Deadly

> *"Congestion may contribute to fender benders, but excess capacity might be contributing to catastrophic outcomes."*

A predictive modeling and planning tool built for the **City of Philadelphia's Office of Transportation and Infrastructure Systems (OTIS)** — Spring 2026.

---

## Live Application

**[Launch the Roadway Safety Explorer →](https://chihyunkim.github.io/otis_off_peak_roadway_safety/landing.html)**

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
├── PROJECT_WRITEUP.Rmd            # Full analysis report (R Markdown source)
├── PROJECT_WRITEUP.html           # Compiled report (rendered HTML)
├── DATA_SOURCES.md                # Data catalog with Box file IDs
│
├── landing.html                   # App landing page (GitHub Pages entry)
├── index.html                     # Interactive risk map
├── table.html                     # Segment data table
├── about.html                     # Methodology & team
├── css/                           # Web app stylesheets
├── js/                            # Web app logic
├── assets/                        # Web app GeoJSON and scenario data
├── images/                        # App screenshots and case study images
│
├── modeling/
│   └── modeling_workflow.R        # Model training, tuning, and evaluation
│
├── data/
│   ├── model/
│   │   ├── modeling_data_v9.rds                     # Final modeling dataset (9,049 obs)
│   │   ├── final_model_fit_v5.rds                   # Trained Random Forest model object
│   │   ├── model_predicted_data_v5.rds              # Predicted speeding, existing conditions
│   │   ├── model_scenario_predicted_data_v7.geojson # Scenario simulation outputs
│   │   ├── model_scenario_predicted_data_v7.csv
│   │   └── Modeling data source data dictionary.txt
│   └── processed/
│       ├── Street_Centerline.RDS                    # Philadelphia street network
│       ├── street_segment_crash_attributes.csv      # Crash counts and KSI rates by segment
│       ├── data_additions_complete.csv              # Hand-verified road geometry data
│       ├── base_network_clean.rds
│       ├── scenario_input_data_v6.csv
│       └── segment_parcels.rds
│
├── app_data/
│   ├── modeling_data_v9.rds
│   ├── model_scenario_predicted_data_draft_v6.csv
│   └── Scenario_Generation.R      # Scenario simulation script
│
├── exploratory_data_analysis/
│   ├── Crash_Data_Analysis_Clean.qmd
│   ├── speed_volume_data_eda.Rmd
│   ├── Supplementary Dataset.Rmd
│   ├── Data/                      # Raw data: crash records 2020–2024, street centerlines, RMS Admin
│   └── road line data/            # Road network GeoJSON (Complete Streets, Bike Network)
│
├── joins/                         # Spatial join and feature engineering scripts
└── data_check/                    # Data validation scripts
```

---

## Data Sources

All raw data are stored in a shared Box repository and loaded via the `boxr` API. See [`DATA_SOURCES.md`](DATA_SOURCES.md) for the complete catalog including Box file IDs.

| Dataset | Source | Role | In Repo |
|---|---|---|---|
| Speed & volume data | PennDOT / DVRPC (Box) | **Primary outcome** — % vehicles speeding per segment per period | Box only — see [`DATA_SOURCES.md`](DATA_SOURCES.md) |
| Crash records (2020–2024) | PennDOT Statewide / OpenDataPhilly | KSI validation; crash rate predictors | [`exploratory_data_analysis/Data/`](exploratory_data_analysis/Data) · [`data/processed/street_segment_crash_attributes.csv`](data/processed/street_segment_crash_attributes.csv) |
| Street centerlines | OpenDataPhilly | Base network; spatial join key | [`exploratory_data_analysis/Data/Street_Centerline/`](exploratory_data_analysis/Data/Street_Centerline) · [`data/processed/Street_Centerline.RDS`](data/processed/Street_Centerline.RDS) |
| Road geometry (RMS Admin) | PennDOT | Surface width, segment geometry | [`exploratory_data_analysis/Data/RMS_-_ADMIN_-_All/`](exploratory_data_analysis/Data/RMS_-_ADMIN_-_All) |
| Road typology (DVRPC Complete Streets) | DVRPC | Lane count, roadway width, divided roadway | [`exploratory_data_analysis/road line data/CompleteStreets.geojson`](<exploratory_data_analysis/road line data/CompleteStreets.geojson>) |
| Bike network | OpenDataPhilly | `bike_lane_status` — hand-verified into 4 categories | [`exploratory_data_analysis/road line data/Bike_Network.geojson`](<exploratory_data_analysis/road line data/Bike_Network.geojson>) |
| Hand-verified road geometry | Team field check (Google Street View) | Lane widths, parking, sidewalk, bike lane status | [`data/processed/data_additions_complete.csv`](data/processed/data_additions_complete.csv) |
| Traffic calming devices | PennDOT Open Data (Box) | `traffic_calming` binary indicator | Box only — see [`DATA_SOURCES.md`](DATA_SOURCES.md) |
| Intersection controls | OpenDataPhilly | Count of stop signs & signals per segment | — |
| Bus stops & transit | SEPTA | `count_transit` — pedestrian activity proxy | — |
| Land parcels | OpenDataPhilly | `parcel_density` — land-use activity proxy | [`data/processed/segment_parcels.rds`](data/processed/segment_parcels.rds) |
| OpenStreetMap | `osmdata` R package | Supplemental road attributes (pre-hand-check fallback) | — |

---

## Team

**MUSA 801 Practicum — Spring 2026 · Weitzman School of Design, University of Pennsylvania**

<table>
  <tr>
    <td align="center" width="25%">
      <b>Kavana Raju</b><br>
      Project Manager<br>
      <sub>MCP '26</sub>
    </td>
    <td align="center" width="25%">
      <b>Christine Cui</b><br>
      GitHub and R Lead<br>
      <sub>MUSA '26</sub>
    </td>
    <td align="center" width="25%">
      <b>Demi Yang</b><br>
      Application Lead<br>
      <sub>MUSA '26</sub>
    </td>
    <td align="center" width="25%">
      <b>Chi-Hyun Kim</b><br>
      Modelling Lead<br>
      <sub>MUSA '26</sub>
    </td>
  </tr>
</table>

**Client:** Office of Transportation and Infrastructure Systems (OTIS), City of Philadelphia
