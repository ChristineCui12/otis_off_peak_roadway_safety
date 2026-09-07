/**
 * model.js
 * Approximation of the trained Random Forest model for off-peak speeding prediction.
 * R² = 0.890, MAE ≈ 6.1% on held-out test data.
 *
 * This is a heuristic surrogate — not the actual forest — designed to produce
 * directionally correct, policy-relevant predictions for the intervention UI.
 * Coefficients are calibrated to match the partial-dependence plots from the
 * fitted ranger model.
 */

(function (global) {
  "use strict";

  /* ------------------------------------------------------------------ *
   *  Road-classification base rates (derived from PDP plots)           *
   *  These represent the model's marginal prediction at each class,    *
   *  holding other features at their training-set medians.             *
   * ------------------------------------------------------------------ */
  const CLASS_BASE = {
    "Major Arterial":         0.28,
    "Minor Arterial":         0.22,
    "Collector Residential":  0.14,
    "Local Residential":      0.09,
    // FHWA equivalents (road_classification_fhwa)
    "Principal Arterial - Other Freeways and Expressways": 0.30,
    "Principal Arterial - Other":  0.27,
    "Minor Arterial":              0.22,
    "Major Collector":             0.16,
    "Minor Collector":             0.12,
    "Local":                       0.09,
  };
  const CLASS_BASE_DEFAULT = 0.15;

  /* ------------------------------------------------------------------ *
   *  Hour-of-day multiplier (from PDP: speed_measurement_hour)        *
   *  Off-peak (late night / early morning) consistently shows higher   *
   *  predicted speeding rates.                                          *
   * ------------------------------------------------------------------ */
  const HOUR_MULT = [
    1.45, 1.55, 1.60, 1.58, 1.50, 1.30,   // 0–5  (midnight to 5 AM)
    1.05, 0.88, 0.82, 0.80, 0.80, 0.82,   // 6–11
    0.83, 0.82, 0.82, 0.84, 0.88, 0.95,   // 12–17
    1.00, 1.05, 1.12, 1.20, 1.30, 1.38,   // 18–23
  ];

  /* ------------------------------------------------------------------ *
   *  Helper utilities                                                   *
   * ------------------------------------------------------------------ */
  function clamp(v, lo, hi) {
    return Math.max(lo, Math.min(hi, v));
  }

  function lerp(a, b, t) {
    return a + (b - a) * clamp(t, 0, 1);
  }

  /**
   * Predict the off-peak speeding percentage (0–1 scale) for a given
   * road segment and set of (potentially modified) intervention values.
   *
   * @param {Object} seg          — original segment properties from GeoJSON
   * @param {Object} interventions — current UI control values (may differ from seg)
   * @param {number} hour         — 0–23 hour of day (from the map slider)
   * @returns {number} predicted speeding fraction in [0, 1]
   */
  function predict(seg, interventions, hour) {
    // 1. Classification base rate ----------------------------------------
    const roadClass = interventions.road_classification_city
      || seg.road_classification_city
      || seg.road_classification_fhwa
      || "";
    let base = CLASS_BASE[roadClass] !== undefined
      ? CLASS_BASE[roadClass]
      : CLASS_BASE_DEFAULT;

    // 2. Lane effect (PDP: monotone increase, ~+3 pp per extra lane) ------
    const lanes = parseFloat(interventions.lanes) || parseFloat(seg.lanes) || 2;
    // Baseline is ~2 lanes for most roads; effect is roughly +0.030 per lane
    base += (lanes - 2) * 0.030;

    // 3. Road width effect (PDP: positive relationship, diminishing) ------
    const width = parseFloat(interventions.width) || parseFloat(seg.width) || 40;
    // Normalised around median ~45 ft; effect caps at ~+0.06 for wide roads
    const widthEffect = clamp((width - 45) / 75, -0.08, 0.08);
    base += widthEffect;

    // 4. Speed limit effect -----------------------------------------------
    const speedLimit = parseFloat(interventions.speed_limit) || parseFloat(seg.speed_limit) || 35;
    // 25 mph → −0.04; 35 mph → 0; 45 mph → +0.04
    base += (speedLimit - 35) / 10 * 0.04;

    // 5. Traffic calming (binary + count) ---------------------------------
    const calming = interventions.traffic_calming
      || (seg.traffic_calming === true || seg.traffic_calming === "true");
    if (calming) {
      const count = parseInt(interventions.calming_count || 1, 10);
      // Each device provides ~−0.012 reduction, diminishing after 4
      base -= clamp(count, 1, 8) * 0.012 * (1 / (1 + Math.max(0, count - 4) * 0.15));
    }

    // 6. Bike lane type ---------------------------------------------------
    const bikeMap = { None: 0, Sharrow: -0.005, Painted: -0.012, Separated: -0.025 };
    // Support both field names: bike_lane_status (raw data) and bike_lane_type_simple (legacy)
    const bikeType = interventions.bike_lane_type_simple
      || interventions.bike_lane_status
      || seg.bike_lane_status
      || seg.bike_lane_type_simple
      || "None";
    base += (bikeMap[bikeType] !== undefined ? bikeMap[bikeType] : 0);

    // 7. Parking (on-street parking narrows effective travel lane width) --
    const parkMap = { None: 0.012, "One side": 0, "Both sides": -0.018 };
    const parking = interventions.parking || seg.parking || "None";
    base += (parkMap[parking] !== undefined ? parkMap[parking] : 0);

    // 8. Divided roadway (median / barrier separation) --------------------
    const divided = interventions.divided_roadway !== undefined
      ? interventions.divided_roadway
      : (seg.divided_roadway === true || seg.divided_roadway === "true");
    if (divided) base -= 0.025;

    // 9. Sidewalk coverage ------------------------------------------------
    const swMap = { none: 0.02, one_side: 0, both_sides: -0.01 };
    const sidewalk = interventions.sidewalk_status || seg.sidewalk_status || "both_sides";
    base += (swMap[sidewalk] !== undefined ? swMap[sidewalk] : 0);

    // 10. Transit stop density (more stops → more friction → less speeding)
    const transit = parseFloat(seg.count_transit) || 0;
    base -= clamp(transit * 0.004, 0, 0.04);

    // 11. Intersection control density ------------------------------------
    const intersect = parseFloat(seg.count_intersection_ctrl) || 0;
    base -= clamp(intersect * 0.005, 0, 0.05);

    // 12. Parcel density (higher density → slower traffic environment) ----
    const parcelDensity = parseFloat(seg.parcel_density) || 0;
    base -= clamp(parcelDensity * 0.0008, 0, 0.05);

    // 13. Hour-of-day multiplier ------------------------------------------
    const h = clamp(Math.round(hour), 0, 23);
    base *= HOUR_MULT[h];

    return clamp(base, 0, 1);
  }

  /**
   * Return a risk category label and CSS class for a predicted fraction.
   */
  function riskCategory(fraction) {
    if (fraction >= 0.30) return { label: "Very High", cls: "very-high" };
    if (fraction >= 0.20) return { label: "High",      cls: "high"      };
    if (fraction >= 0.10) return { label: "Moderate",  cls: "moderate"  };
    return                       { label: "Low",        cls: "low"       };
  }

  /**
   * Return the Leaflet polyline colour for a given fraction or attribute.
   */
  function riskColor(fraction) {
    if (fraction >= 0.30) return "#cc3000";
    if (fraction >= 0.20) return "#f99300";
    if (fraction >= 0.10) return "#f3c613";
    return "#58c04d";
  }

  function roadClassColor(cls) {
    const map = {
      "Major Arterial":         "#3d2280",
      "Minor Arterial":         "#6544c0",
      "Collector Residential":  "#c0aef5",
      "Local Residential":      "#cfcfcf",
    };
    return map[cls] || "#cfcfcf";
  }

  function speedLimitColor(limit) {
    const l = parseFloat(limit) || 25;
    if (l >= 40) return "#cc3000";
    if (l >= 35) return "#f99300";
    if (l >= 30) return "#f3c613";
    return "#58c04d";
  }

  function lanesColor(lanes) {
    const n = parseFloat(lanes);
    if (isNaN(n)) return "#a1a1a1";
    if (n >= 5) return "#3d2280";
    if (n >= 4) return "#6544c0";
    if (n >= 3) return "#9a82dd";
    if (n >= 2) return "#58c04d";
    if (n >= 1) return "#f3c613";
    return "#a1a1a1";
  }

  // Expose public API
  global.OTISModel = {
    predict,
    riskCategory,
    riskColor,
    roadClassColor,
    speedLimitColor,
    lanesColor,
  };

}(window));
