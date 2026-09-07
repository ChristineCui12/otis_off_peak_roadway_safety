/**
 * data.js — OTIS Data Manager
 *
 * Loads and indexes two GeoJSON assets:
 *   basic_info.geojson          — 446 segs × 4 periods; road attrs + observed/predicted speeding
 *   model_scenario_predicted_data_v7.geojson — same segs × 4 periods × 25 scenarios (adds traffic calming)
 *
 * Period keys used in the UI:
 *   "night_offpeak"  → "Off-peak (night)"
 *   "morning_peak"   → "Peak (morning)"
 *   "midday_offpeak" → "Off-peak (midday)"
 *   "evening_peak"   → "Peak (evening)"
 */

(function (global) {
  "use strict";

  var scriptEl = document.currentScript;
  var scriptBase = scriptEl ? scriptEl.src : "";
  var BASIC_INFO_PATH = new URL("../assets/basic_info.geojson", scriptBase).href;
  var SCENARIO_PATH   = new URL("../assets/model_scenario_predicted_data_v7.geojson", scriptBase).href;

  /* UI period key → data period string */
  var PERIOD_MAP = {
    night_offpeak:  "Off-peak (night)",
    morning_peak:   "Peak (morning)",
    midday_offpeak: "Off-peak (midday)",
    evening_peak:   "Peak (evening)",
  };

  /* Scenario name → human-readable label */
  var SCENARIO_LABELS = {
    existing_conditions:                        "Existing Conditions",
    add_painted_bike_lane_one_side:             "Add painted bike lane (one side)",
    add_separated_bike_lane_one_side:           "Add separated bike lane (one side)",
    upgrade_painted_to_separated_one_side:      "Upgrade painted → separated (one side)",
    add_painted_bike_lane_both_sides:           "Add painted bike lane (both sides)",
    add_separated_bike_lane_both_sides:         "Add separated bike lane (both sides)",
    upgrade_painted_to_separated_both_sides:    "Upgrade painted → separated (both sides)",
    lane_diet:                                  "Lane diet (−1 lane)",
    lane_diet_add_painted_one_side:             "Lane diet + painted bike lane (one side)",
    lane_diet_add_separated_one_side:           "Lane diet + separated bike lane (one side)",
    lane_diet_upgrade_to_separated_one_side:    "Lane diet + upgrade to separated (one side)",
    lane_diet_add_painted_both_sides:           "Lane diet + painted bike lane (both sides)",
    lane_diet_add_separated_both_sides:         "Lane diet + separated bike lane (both sides)",
    lane_diet_upgrade_to_separated_both_sides:  "Lane diet + upgrade to separated (both sides)",
    lane_diet_add_parking:                      "Lane diet + add parking (both sides)",
    major_road_diet_to_2_lanes:                 "Major road diet → 2 lanes",
    major_road_diet_add_painted_one_side:       "Major road diet + painted bike lane (one side)",
    major_road_diet_add_separated_one_side:     "Major road diet + separated bike lane (one side)",
    major_road_diet_add_painted_both_sides:     "Major road diet + painted bike lane (both sides)",
    major_road_diet_add_separated_both_sides:   "Major road diet + separated bike lane (both sides)",
    remove_parking_one_side:                    "Remove parking (one side)",
    remove_parking_both_sides:                  "Remove parking (both sides)",
    add_parking_one_side:                       "Add parking (one side)",
    add_parking_both_sides:                     "Add parking (both sides)",
    add_traffic_calming:                        "Add traffic calming",
  };

  /* Scenario categories with ordered member lists */
  var SCENARIO_CATEGORIES = [
    {
      id: "existing",
      label: "Existing Conditions",
      scenarios: ["existing_conditions"],
    },
    {
      id: "bike",
      label: "Bike Lane Improvement",
      scenarios: [
        "add_painted_bike_lane_one_side",
        "add_separated_bike_lane_one_side",
        "upgrade_painted_to_separated_one_side",
        "add_painted_bike_lane_both_sides",
        "add_separated_bike_lane_both_sides",
        "upgrade_painted_to_separated_both_sides",
      ],
    },
    {
      id: "lane_diet",
      label: "Lane Diet",
      scenarios: ["lane_diet"],
    },
    {
      id: "lane_diet_bike",
      label: "Lane Diet + Bike Lane",
      scenarios: [
        "lane_diet_add_painted_one_side",
        "lane_diet_add_separated_one_side",
        "lane_diet_upgrade_to_separated_one_side",
        "lane_diet_add_painted_both_sides",
        "lane_diet_add_separated_both_sides",
        "lane_diet_upgrade_to_separated_both_sides",
        "lane_diet_add_parking",
      ],
    },
    {
      id: "major_diet",
      label: "Major Road Diet",
      scenarios: [
        "major_road_diet_to_2_lanes",
        "major_road_diet_add_painted_one_side",
        "major_road_diet_add_separated_one_side",
        "major_road_diet_add_painted_both_sides",
        "major_road_diet_add_separated_both_sides",
      ],
    },
    {
      id: "parking",
      label: "Parking Change",
      scenarios: [
        "remove_parking_one_side",
        "remove_parking_both_sides",
        "add_parking_one_side",
        "add_parking_both_sides",
      ],
    },
    {
      id: "calming",
      label: "Traffic Calming",
      scenarios: ["add_traffic_calming"],
    },
  ];

  /* ── internal indices ──────────────────────────────────────────── */
  /* seg_id (string) → { period_str → props } */
  var _basicIndex    = {};
  /* seg_id (string) → { period_str → { scenario_name → config } } */
  var _scenarioIndex = {};
  /* deduplicated FeatureCollection for map rendering */
  var _networkGeoJSON = null;

  var _basicLoaded    = false;
  var _scenarioLoaded = false;
  var _basicCbs       = [];
  var _allCbs         = [];

  /* ── low-level JSON loader ─────────────────────────────────────── */
  function _fetch(path, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open("GET", path, true);
    xhr.onreadystatechange = function () {
      if (xhr.readyState !== 4) return;
      if (xhr.status === 200 || xhr.status === 0) {
        try { cb(null, JSON.parse(xhr.responseText)); }
        catch (e) { cb(e); }
      } else {
        cb(new Error("HTTP " + xhr.status + " for " + path));
      }
    };
    xhr.send();
  }

  function _fire(list) {
    while (list.length) list.shift()();
  }

  /* ── loaders ───────────────────────────────────────────────────── */
  function _loadBasic(done) {
    _fetch(BASIC_INFO_PATH, function (err, geojson) {
      if (err) { console.error("[OTISData] basic_info:", err); return; }
      var dedupFeats = [];
      (geojson.features || []).forEach(function (f) {
        var p   = f.properties || {};
        var sid = String(p.seg_id);
        if (!_basicIndex[sid]) {
          _basicIndex[sid] = {};
          dedupFeats.push({ type: "Feature", geometry: f.geometry, properties: p });
        }
        _basicIndex[sid][p.speed_measurement_period] = p;
      });
      _networkGeoJSON = { type: "FeatureCollection", features: dedupFeats };
      _basicLoaded = true;
      _fire(_basicCbs);
      if (_scenarioLoaded) _fire(_allCbs);
      if (done) done();
    });
  }

  function _loadScenarios(done) {
    _fetch(SCENARIO_PATH, function (err, geojson) {
      if (err) { console.error("[OTISData] scenarios:", err); return; }
      (geojson.features || []).forEach(function (f) {
        var p      = f.properties || {};
        var sid    = String(p.seg_id);
        var period = p.speed_measurement_period;
        var sn     = p.scenario_name;
        if (!_scenarioIndex[sid])         _scenarioIndex[sid] = {};
        if (!_scenarioIndex[sid][period]) _scenarioIndex[sid][period] = {};
        var entry = {
          predicted_speeding_percent: p.predicted_speeding_percent,
          lanes:                  p.lanes,
          bike_lane_status:       p.bike_lane_status,
          parking:                p.parking,
          sidewalk_status:        p.sidewalk_status,
          traffic_lanes_width:    p.traffic_lanes_width,
          width_per_traffic_lane: p.width_per_traffic_lane,
          shoulder_width:         p.shoulder_width,
          curb_to_curb_width:     p.curb_to_curb_width,
        };
        if (sn === "existing_conditions") {
          entry.crashes             = p.crashes;
          entry.ksi_rate            = p.ksi_rate;
          entry.all_speeding_percent  = p.all_speeding_percent;
          entry.high_speeding_percent = p.high_speeding_percent;
        }
        _scenarioIndex[sid][period][sn] = entry;
      });
      _scenarioLoaded = true;
      if (_basicLoaded) _fire(_allCbs);
      if (done) done();
    });
  }

  /* ── public API ────────────────────────────────────────────────── */
  function getSegmentInfo(segId, periodKey) {
    var ps  = PERIOD_MAP[periodKey] || periodKey;
    var idx = _basicIndex[String(segId)];
    return (idx && idx[ps]) || null;
  }

  function getScenarioPredictions(segId, periodKey) {
    var ps  = PERIOD_MAP[periodKey] || periodKey;
    var idx = _scenarioIndex[String(segId)];
    return (idx && idx[ps]) || {};
  }

  function getBestIntervention(segId, periodKey) {
    var preds = getScenarioPredictions(segId, periodKey);
    var best  = null, bestPct = Infinity;
    Object.keys(preds).forEach(function (sn) {
      if (sn === "existing_conditions") return;
      var pct = preds[sn] && preds[sn].predicted_speeding_percent;
      if (pct !== null && pct !== undefined && pct < bestPct) {
        bestPct = pct;
        best    = { scenario_name: sn, predicted_speeding_percent: pct, config: preds[sn] };
      }
    });
    return best;
  }

  function getAvailableScenarios(segId, periodKey) {
    return Object.keys(getScenarioPredictions(segId, periodKey));
  }

  global.OTISData = {
    load: function (onBasic, onAll) {
      _loadBasic(onBasic || null);
      _loadScenarios(function () { if (onAll) onAll(); });
    },
    onBasicReady:           function (cb) { if (_basicLoaded) cb(); else _basicCbs.push(cb); },
    onAllReady:             function (cb) { if (_basicLoaded && _scenarioLoaded) cb(); else _allCbs.push(cb); },
    isScenarioReady:        function () { return _scenarioLoaded; },
    getNetworkGeoJSON:      function () { return _networkGeoJSON; },
    getSegmentInfo:         getSegmentInfo,
    getScenarioPredictions: getScenarioPredictions,
    getBestIntervention:    getBestIntervention,
    getAvailableScenarios:  getAvailableScenarios,
    getExistingConditions: function (segId, periodKey) {
      var ps  = PERIOD_MAP[periodKey] || periodKey;
      var idx = _scenarioIndex[String(segId)];
      return (idx && idx[ps] && idx[ps]["existing_conditions"]) || null;
    },
    PERIOD_MAP:             PERIOD_MAP,
    SCENARIO_LABELS:        SCENARIO_LABELS,
    SCENARIO_CATEGORIES:    SCENARIO_CATEGORIES,
  };

}(window));
