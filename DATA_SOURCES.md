# Data Sources

Complete catalog of datasets used in the Off-Peak Roadway Safety project. All processed files are stored in the shared Box repository. Raw files that cannot be redistributed are noted.

---

## Primary Modeling Data

### Speed & Volume Data
- **Source:** Philadelphia County Speed CSV files — PennDOT / DVRPC (via client Box folder)
- **Coverage:** 121 monitored road segments, 2022–2025
- **Content:** Hourly vehicle counts by speed bin (5 mph increments), volume totals
- **Role:** Primary modeling outcome — percentage of vehicles exceeding the posted speed limit per segment per time period
- **Box ID (processed):** `2193174265310`

### Crash Records (2020–2024)
- **Source:** PennDOT Statewide Crash Database / OpenDataPhilly
- **Coverage:** ~16,000+ crashes in Philadelphia County, five years
- **Content:** Crash location, severity (KSI indicator), time of day, contributing factors, roadway context
- **Role:** KSI rate validation; crash count and KSI rate joined to modeling segments as contextual predictors
- **Box ID (network-joined):** `2195155235316`

---

## Road Network & Geometry

### Street Centerlines
- **Source:** OpenDataPhilly — Philadelphia Street Centerline dataset
- **Content:** Full road network geometry, `seg_id` join key, road classification
- **Role:** Base network; spatial join anchor for all segment-level features
- **Box ID:** `2139915462983`
- **Hand-checked version Box ID:** `2205423482212`

### RMS Admin Segments
- **Source:** `GISDATA_RMSADMIN.shp` — PennDOT (via client Box folder)
- **Content:** Administrative road segments with surface width (`surfawidth`), geometry
- **Role:** Primary source for road geometry; highest-priority source in feature merge hierarchy

### DVRPC Complete Streets
- **Source:** Delaware Valley Regional Planning Commission
- **Content:** Road typology, lane counts (`NEW_LANES`), roadway width (`NEW_WID`), divided roadway indicator (`DIV_RDWY`)
- **Role:** Road classification and geometry — second-priority source in feature merge hierarchy

### Main Street Network (processed)
- **Box ID:** `2151757279199`
- **Content:** Merged network with road geometry, classification, and all supplementary features joined

---

## Supplementary Network Features

### Traffic Calming Devices
- **Source:** PennDOT Open Data / client Box folder
- **Content:** Speed humps, speed cushions, raised crosswalks — point locations
- **Role:** Binary indicator (`traffic_calming`) — whether any calming device is present on segment
- **Box ID (supplementary network):** `2175268420062`

### Intersection Controls
- **Source:** OpenDataPhilly
- **Content:** Stop signs, traffic signals — point locations
- **Role:** Count of controls within 30 ft of segment endpoints (`count_intersection_ctrl`)

### Bike Network
- **Source:** OpenDataPhilly
- **Content:** Bike lane type (advisory, painted, separated, shared-use path)
- **Role:** `bike_lane_status` — hand-verified and consolidated into four model categories: None / Sharrow / Painted / Separated

### Bus Stops & Transit
- **Source:** SEPTA
- **Content:** Bus stop locations citywide
- **Role:** Count of stops on segment (`count_transit`) — pedestrian activity proxy

### Land Parcels
- **Source:** OpenDataPhilly
- **Content:** Parcel polygons
- **Role:** `parcel_density` — adjacent parcel count as land-use activity proxy
- **Box ID:** `2178038226815`

### Red Light Cameras
- **Source:** Philadelphia Parking Authority (PPA) / OpenDataPhilly
- **Content:** Fixed camera locations
- **Role:** Enforcement signal; included in initial feature set, dropped in final model due to low importance

### PA TIP Projects
- **Source:** DVRPC / PennDOT Transportation Improvement Program
- **Content:** Planned and recently completed construction projects
- **Role:** Construction context; used to flag recently modified segments

### OpenStreetMap (OSM)
- **Source:** `osmdata` R package
- **Content:** Supplemental road characteristics (lanes, speed limits, surface type)
- **Role:** Pre-hand-check fallback for missing road attributes; superseded by hand-checked values where available

---

## Speed Data Processing

### Processed Speed Data
- **Box ID:** `2193174265310`
- **Content:** 9,052 segment-period observations with all speeding and volume metrics computed
- **Derived fields:** `all_speeding_percent`, `high_speeding_percent`, `speed_measurement_period`, volume totals

### Modeling Dataset
- **Local path:** `app_data/modeling_data_v9.rds`
- **Content:** Final modeling dataset with all predictors joined, used for Random Forest training
- **Observations:** 9,049 rows (segment × time period)

### Scenario Prediction Data
- **Local path:** `app_data/model_scenario_predicted_data_draft_v6.csv`
- **Box ID:** `2218094430533`
- **Content:** Pre-computed predictions for 24 intervention scenarios across all eligible segments and four time periods

### Final Model Object
- **Box ID:** `2215931771424`
- **Content:** Trained Random Forest model (ranger engine), saved as `.rds`
- **Performance:** R² = 0.920, RMSE = 0.074, MAE = 0.042 (held-out 25% test set)

---

## Reference Documents

The following reports informed the project's analytical framing and scenario design:

| Document | Source |
|---|---|
| Arterial Typology & Speed Management Framework (2022) | DVRPC |
| Evaluating the Off-Peak Impacts of Designing for Peak Hour Operations (December 2024) | OTIS Office of Multimodal Planning / TESC (internal report) |
| Safe Waves: Signal Timing for Speed Management (2024) | Furth et al., Northeastern University |
| Mental Frameworks Underlying Driver Behavior in Urban Contexts (2021) | Tice et al., University of Central Florida |

---

*Last updated: Spring 2026 — MUSA 801 Practicum, University of Pennsylvania*
