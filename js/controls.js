/**
 * controls.js — right panel renderer + map search
 *
 * Section 1 — Current Road Design (cross-section + road design fields)
 * Section 2 — Predicted Speeding Impact (current vs best + best XS)
 * Section 3 — Road Intervention Scenarios (checkboxes + per-category sub-options)
 * Search     — segment search above "Color roads by" overlay
 *
 * Depends on: model.js, data.js
 */

(function (global) {
  "use strict";

  var PERIOD_DISPLAY = {
    night_offpeak:  "Night Off-Peak",
    morning_peak:   "Morning Peak",
    midday_offpeak: "Midday Off-Peak",
    evening_peak:   "Evening Peak",
  };

  /* Intervention checkbox definitions */
  var IV_CHECKS = [
    { id: "bike",      label: "Bike Lane"        },
    { id: "lane_diet", label: "Lane Diet"         },
    { id: "major",     label: "Major Road Diet"   },
    { id: "parking",   label: "Parking"           },
    { id: "calming",   label: "Traffic Calming"   },
  ];

  /* Bike sub-option descriptors */
  var BIKE_OPTIONS = [
    { value: "painted_one",      label: "Add painted bike lane (one side)" },
    { value: "separated_one",    label: "Add separated bike lane (one side)" },
    { value: "upgrade_sep_one",  label: "Upgrade painted → separated (one side)" },
    { value: "painted_both",     label: "Add painted bike lane (both sides)" },
    { value: "separated_both",   label: "Add separated bike lane (both sides)" },
    { value: "upgrade_sep_both", label: "Upgrade painted → separated (both sides)" },
  ];

  var PARKING_OPTIONS = [
    { value: "remove_one",  label: "Remove parking (one side)" },
    { value: "remove_both", label: "Remove parking (both sides)" },
    { value: "add_one",     label: "Add parking (one side)" },
    { value: "add_both",    label: "Add parking (both sides)" },
  ];

  /* Maps bike option value → scenario name by context */
  var BIKE_SCENARIO_MAP = {
    alone: {
      painted_one:      "add_painted_bike_lane_one_side",
      separated_one:    "add_separated_bike_lane_one_side",
      upgrade_sep_one:  "upgrade_painted_to_separated_one_side",
      painted_both:     "add_painted_bike_lane_both_sides",
      separated_both:   "add_separated_bike_lane_both_sides",
      upgrade_sep_both: "upgrade_painted_to_separated_both_sides",
    },
    with_lane_diet: {
      painted_one:      "lane_diet_add_painted_one_side",
      separated_one:    "lane_diet_add_separated_one_side",
      upgrade_sep_one:  "lane_diet_upgrade_to_separated_one_side",
      painted_both:     "lane_diet_add_painted_both_sides",
      separated_both:   "lane_diet_add_separated_both_sides",
      upgrade_sep_both: "lane_diet_upgrade_to_separated_both_sides",
    },
    with_major: {
      painted_one:    "major_road_diet_add_painted_one_side",
      separated_one:  "major_road_diet_add_separated_one_side",
      painted_both:   "major_road_diet_add_painted_both_sides",
      separated_both: "major_road_diet_add_separated_both_sides",
    },
  };

  var PARKING_SCENARIO_MAP = {
    alone: {
      remove_one:  "remove_parking_one_side",
      remove_both: "remove_parking_both_sides",
      add_one:     "add_parking_one_side",
      add_both:    "add_parking_both_sides",
    },
    with_lane_diet: {
      add_both: "lane_diet_add_parking",
    },
  };

  var MAJOR_BIKE_SNS = [
    "major_road_diet_add_painted_one_side",  "major_road_diet_add_separated_one_side",
    "major_road_diet_add_painted_both_sides","major_road_diet_add_separated_both_sides",
  ];

  /* ── state ─────────────────────────────────────────────────────── */
  var _segment      = null;
  var _segId        = null;
  var _period       = "night_offpeak";
  var _checkedIvs   = [];
  var _subSel       = {};
  var _currentAvSet = {};
  var _searchIndex  = [];

  /* ── DOM helpers ────────────────────────────────────────────────── */
  function $(id) { return document.getElementById(id); }
  function fmtPct(f) {
    if (f === null || f === undefined) return "—";
    return (f * 100).toFixed(1) + "%";
  }
  function fmtNum(v, decimals, suffix) {
    if (v === null || v === undefined) return "—";
    var n = parseFloat(v);
    if (isNaN(n)) return "—";
    return n.toFixed(decimals || 0) + (suffix || "");
  }
  function cap(s) { return String(s || "").replace(/\b\w/g, function (c) { return c.toUpperCase(); }); }
  function esc(s) { return String(s || "").replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;"); }
  function setText(id, v) { var el = $(id); if (el) el.textContent = v; }
  function setHtml(id, v) { var el = $(id); if (el) el.innerHTML  = v; }
  function _isMobilePanelMode() {
    return global.matchMedia && global.matchMedia("(max-width: 760px)").matches;
  }
  function _setMobileSegmentState(active) {
    var body = document.querySelector(".dashboard-body");
    if (body) body.classList.toggle("segment-selected", !!active);
  }
  function _collapseMobilePanel() {
    var panel = $("right-panel");
    if (!panel) return;
    if (_isMobilePanelMode()) panel.classList.remove("expanded");
  }
  function _expandMobilePanel() {
    var panel = $("right-panel");
    if (!panel) return;
    if (_isMobilePanelMode()) panel.classList.add("expanded");
  }

  /* Format width: integer if whole, else 1 decimal */
  function _fmtW(w) {
    var v = Math.round(w * 10) / 10;
    return (v % 1 === 0 ? Math.round(v).toString() : v.toFixed(1)) + "'";
  }

  /* ── open / close panel ─────────────────────────────────────────── */
  function selectSegment(feature) {
    _segment    = feature;
    _segId      = String((feature.properties || {}).seg_id || "");
    _checkedIvs = [];
    _subSel     = {};
    var panel = $("right-panel");
    if (panel) {
      panel.classList.add("open");
      if (_isMobilePanelMode()) panel.classList.remove("expanded");
      else panel.classList.add("expanded");
    }
    _setMobileSegmentState(true);
    var hint = $("map-hint");
    if (hint) hint.classList.add("hidden");
    _renderPanel();
  }

  function closePanel() {
    var panel = $("right-panel");
    if (panel) panel.classList.remove("open", "expanded");
    _setMobileSegmentState(false);
    _segment = null;
    _segId   = null;
    if (global.OTISMap) global.OTISMap.clearSelection();
  }

  function onPeriodChange(periodKey) {
    _period = periodKey;
    setText("rp-period-chip", PERIOD_DISPLAY[periodKey] || "");
    if (_segment) _renderPanel();
  }

  /* ── main render ────────────────────────────────────────────────── */
  function _renderPanel() {
    var p    = (_segment && _segment.properties) || {};
    var name = p.road_name || p.speed_measurement_road || "Road Segment";
    setText("rp-road-name",   cap(name));
    setText("rp-seg-id",      _segId || "—");
    setText("rp-period-chip", PERIOD_DISPLAY[_period] || "");

    var info = OTISData.getSegmentInfo(_segId, _period);
    _renderCurrentDesign(info);
    _renderPredictedImpact(info);
    _renderInterventionSelector();
  }

  /* ── Section 1: Current Road Design ─────────────────────────────── */
  function _renderCurrentDesign(info) {
    if (!info) {
      setHtml("rp-info-grid", '<div class="rp-no-data">No data for this period.</div>');
      return;
    }
    _drawCurrentXS(info);

    var rows = [
      ["Speed Limit",  info.speed_limit ? info.speed_limit + " mph" : "—"],
      ["Travel Lanes", info.lanes || "—"],
      ["Direction",    info.traffic_direction || "—"],
      ["Bike Lane",    info.bike_lane_status || "—"],
      ["Parking",      info.parking || "—"],
      ["Sidewalk",     info.sidewalk_status || "—"],
      ["Curb-to-curb", fmtNum(info.curb_to_curb_width, 0, " ft")],
      ["Lane Width",   fmtNum(info.width_per_traffic_lane, 1, " ft")],
      ["Shoulder",     fmtNum(info.shoulder_width, 0, " ft")],
    ];

    var html = rows.map(function (r) {
      return '<div class="rp-info-item">'
        + '<div class="rp-info-label">' + esc(r[0]) + '</div>'
        + '<div class="rp-info-value">'  + esc(r[1]) + '</div>'
        + '</div>';
    }).join("");

    setHtml("rp-info-grid", html);
  }

  /* ── Section 2: Predicted Speeding Impact ───────────────────────── */
  function _renderPredictedImpact(info) {
    var ecInfo     = OTISData.getExistingConditions ? OTISData.getExistingConditions(_segId, _period) : null;
    var currentPct = ecInfo ? ecInfo.predicted_speeding_percent
                    : (info ? info.predicted_speeding_percent : null);
    var best       = OTISData.getBestIntervention(_segId, _period);
    var bestPct    = best ? best.predicted_speeding_percent : null;
    var availableScenarios = OTISData.getAvailableScenarios ? OTISData.getAvailableScenarios(_segId, _period) : [];
    var hasInterventions = availableScenarios.some(function (sn) { return sn !== "existing_conditions"; });
    var hasBetterIntervention = currentPct !== null && bestPct !== null && bestPct < currentPct;

    var curEl = $("impact-pct-current");
    if (curEl) {
      curEl.textContent = fmtPct(currentPct);
      curEl.style.color = currentPct !== null ? OTISModel.riskColor(currentPct) : "#a1a1a1";
    }
    var curRiskEl = $("impact-risk-current");
    if (curRiskEl) {
      if (currentPct !== null) {
        curRiskEl.textContent = OTISModel.riskCategory(currentPct).label;
        curRiskEl.style.color = OTISModel.riskColor(currentPct);
      } else { curRiskEl.textContent = ""; }
    }

    var bestEl = $("impact-pct-best");
    if (bestEl) {
      bestEl.textContent = hasBetterIntervention ? fmtPct(bestPct) : "—";
      bestEl.style.color = hasBetterIntervention ? OTISModel.riskColor(bestPct) : "#a1a1a1";
    }
    var bestRiskEl = $("impact-risk-best");
    if (bestRiskEl) {
      if (hasBetterIntervention) {
        bestRiskEl.textContent = OTISModel.riskCategory(bestPct).label;
        bestRiskEl.style.color = OTISModel.riskColor(bestPct);
      } else { bestRiskEl.textContent = ""; }
    }

    var deltaEl = $("impact-delta");
    if (deltaEl) {
      if (hasBetterIntervention) {
        var diff = bestPct - currentPct;
        if (Math.abs(diff) < 0.0005) {
          deltaEl.textContent = "No change"; deltaEl.className = "impact-delta";
        } else if (diff < 0) {
          deltaEl.textContent = "▼ " + fmtPct(Math.abs(diff)) + " reduction in predicted speeding";
          deltaEl.className   = "impact-delta delta-good";
        } else {
          deltaEl.textContent = "▲ " + fmtPct(diff) + " increase";
          deltaEl.className   = "impact-delta delta-bad";
        }
      } else {
        deltaEl.textContent = ""; deltaEl.className = "impact-delta";
      }
    }

    var compareEl = document.querySelector(".impact-compare");
    if (compareEl) compareEl.classList.toggle("impact-compare--current-only", !hasBetterIntervention);

    /* Optimal design notice: no available intervention improves on current design */
    var optimalEl = $("impact-optimal-notice");
    if (optimalEl) {
      if (currentPct !== null && hasInterventions && !hasBetterIntervention) optimalEl.classList.remove("hidden");
      else                                                                   optimalEl.classList.add("hidden");
    }

    var nameEl = $("impact-best-name");
    var xsWrap = $("impact-best-xs");
    if (hasBetterIntervention && best && OTISData.SCENARIO_LABELS[best.scenario_name]) {
      if (nameEl) { nameEl.textContent = "Best: " + OTISData.SCENARIO_LABELS[best.scenario_name]; nameEl.style.display = ""; }
      if (xsWrap) xsWrap.classList.remove("hidden");
      _drawBestXS(best.scenario_name, best.config);
    } else {
      if (nameEl) { nameEl.textContent = ""; nameEl.style.display = "none"; }
      if (xsWrap) xsWrap.classList.add("hidden");
    }
  }

  /* ── Section 3: Intervention Scenarios ──────────────────────────── */

  /* Returns which checkboxes should be disabled given current selection + available data */
  function _getComboDisabled(checked, avSet) {
    var hasBike    = checked.indexOf("bike")      !== -1;
    var hasLane    = checked.indexOf("lane_diet") !== -1;
    var hasMajor   = checked.indexOf("major")     !== -1;
    var hasPark    = checked.indexOf("parking")   !== -1;
    var hasCalming = checked.indexOf("calming")   !== -1;
    var count = (hasBike?1:0) + (hasLane?1:0) + (hasMajor?1:0) + (hasPark?1:0) + (hasCalming?1:0);

    var d = {};

    /* Traffic calming is standalone — always incompatible with every other type */
    if (hasCalming) { d.bike = true; d.lane_diet = true; d.major = true; d.parking = true; }
    if (hasBike || hasLane || hasMajor || hasPark) d.calming = true;

    /* Catalog-level never-valid combos (no scenarios exist for these pairs) */
    if (hasBike)  d.parking   = true;
    if (hasPark)  d.bike      = true;
    if (hasLane)  d.major     = true;
    if (hasMajor) { d.lane_diet = true; d.parking = true; }
    if (hasPark)  d.major     = true;

    /* Data-driven: lane_diet + parking only via lane_diet_add_parking */
    if (hasLane && !avSet["lane_diet_add_parking"]) d.parking   = true;
    if (hasPark && !avSet["lane_diet_add_parking"]) d.lane_diet = true;

    /* Data-driven: bike + major only if major+bike scenarios available */
    var majorBikeOk = MAJOR_BIKE_SNS.some(function (s) { return avSet[s]; });
    if (hasBike  && !majorBikeOk) d.major = true;
    if (hasMajor && !majorBikeOk) d.bike  = true;

    /* Once 2 checked, disable everything else */
    if (count >= 2) {
      if (!hasBike)     d.bike      = true;
      if (!hasLane)     d.lane_diet = true;
      if (!hasMajor)    d.major     = true;
      if (!hasPark)     d.parking   = true;
      if (!hasCalming)  d.calming   = true;
    }

    return d;
  }

  /* Update disabled state of existing checkbox DOM elements (no re-render) */
  function _updateCheckboxStates(avSet) {
    var catAvail = {};
    OTISData.SCENARIO_CATEGORIES.forEach(function (cat) {
      catAvail[cat.id] = cat.scenarios.some(function (s) { return avSet[s]; });
    });
    function isDataEnabled(chkId) {
      if (chkId === "bike")      return !!(catAvail["bike"] || catAvail["lane_diet_bike"] || catAvail["major_diet"]);
      if (chkId === "lane_diet") return !!(catAvail["lane_diet"] || catAvail["lane_diet_bike"]);
      if (chkId === "major")     return !!(catAvail["major_diet"]);
      if (chkId === "parking")   return !!(catAvail["parking"] || avSet["lane_diet_add_parking"]);
      if (chkId === "calming")   return !!(catAvail["calming"]);
      return false;
    }

    var comboDisabled = _getComboDisabled(_checkedIvs, avSet);

    IV_CHECKS.forEach(function (chk) {
      var dataOk  = isDataEnabled(chk.id);
      var enabled = dataOk && !comboDisabled[chk.id];
      var inputEl = document.querySelector('[data-chk="' + chk.id + '"]');
      var labelEl = inputEl ? inputEl.closest(".iv-check-option") : null;
      if (!inputEl) return;

      /* Force-uncheck if now disabled */
      if (!enabled && inputEl.checked) {
        inputEl.checked = false;
        _checkedIvs = _checkedIvs.filter(function (x) { return x !== chk.id; });
        delete _subSel[chk.id];
      }

      inputEl.disabled = !enabled;
      if (labelEl) {
        if (enabled) labelEl.classList.remove("iv-check-option--disabled");
        else         labelEl.classList.add("iv-check-option--disabled");
        labelEl.title = !dataOk        ? "Not available for this segment"
                      : !enabled       ? "Incompatible with current selection"
                      : "";
      }
    });
  }

  function _renderInterventionSelector() {
    var checksDiv = $("iv-checks");
    var loading   = $("iv-loading");

    if (!OTISData.isScenarioReady()) {
      if (checksDiv) checksDiv.classList.add("hidden");
      if (loading)   loading.classList.remove("hidden");
      OTISData.onAllReady(function () { if (_segment) _renderInterventionSelector(); });
      return;
    }

    if (loading)   loading.classList.add("hidden");
    if (checksDiv) checksDiv.classList.remove("hidden");

    var available = OTISData.getAvailableScenarios(_segId, _period);
    var avSet = {};
    available.forEach(function (s) { avSet[s] = true; });
    _currentAvSet = avSet;

    var catAvail = {};
    OTISData.SCENARIO_CATEGORIES.forEach(function (cat) {
      catAvail[cat.id] = cat.scenarios.some(function (s) { return avSet[s]; });
    });
    function isDataEnabled(chkId) {
      if (chkId === "bike")      return !!(catAvail["bike"] || catAvail["lane_diet_bike"] || catAvail["major_diet"]);
      if (chkId === "lane_diet") return !!(catAvail["lane_diet"] || catAvail["lane_diet_bike"]);
      if (chkId === "major")     return !!(catAvail["major_diet"]);
      if (chkId === "parking")   return !!(catAvail["parking"] || avSet["lane_diet_add_parking"]);
      if (chkId === "calming")   return !!(catAvail["calming"]);
      return false;
    }

    var comboDisabled = _getComboDisabled(_checkedIvs, avSet);
    _checkedIvs = _checkedIvs.filter(function (id) {
      return isDataEnabled(id) && !comboDisabled[id];
    });
    comboDisabled = _getComboDisabled(_checkedIvs, avSet);

    var html = IV_CHECKS.map(function (chk) {
      var dataOk      = isDataEnabled(chk.id);
      var enabled     = dataOk && !comboDisabled[chk.id];
      var checked     = _checkedIvs.indexOf(chk.id) !== -1;
      var disabledCls = enabled ? "" : " iv-check-option--disabled";
      var title       = !dataOk  ? "Not available for this segment"
                      : !enabled ? "Incompatible with current selection" : "";
      return '<label class="iv-check-option' + disabledCls + '"' + (title ? ' title="' + title + '"' : '') + '>'
        + '<input type="checkbox" class="iv-check-input" data-chk="' + chk.id + '"'
        + (checked ? " checked"  : "")
        + (enabled ? ""          : " disabled")
        + ' />'
        + '<span class="iv-check-label">' + esc(chk.label) + '</span>'
        + '</label>';
    }).join("");
    setHtml("iv-checks", html);

    checksDiv.querySelectorAll(".iv-check-input").forEach(function (cb) {
      cb.addEventListener("change", function () {
        var id = this.dataset.chk;
        if (this.checked) { if (_checkedIvs.indexOf(id) === -1) _checkedIvs.push(id); }
        else              { _checkedIvs = _checkedIvs.filter(function (x) { return x !== id; }); delete _subSel[id]; }
        _onChecksChange(_currentAvSet);
      });
    });

    _onChecksChange(avSet);
  }

  function _onChecksChange(avSet) {
    _currentAvSet = avSet;
    _updateCheckboxStates(avSet);

    var subOptDiv = $("iv-sub-options");
    var resultDiv = $("iv-result");

    if (!_checkedIvs.length) {
      if (subOptDiv) subOptDiv.innerHTML = "";
      if (resultDiv) resultDiv.classList.add("hidden");
      return;
    }

    _renderSubOptions(avSet);
  }

  /* Build per-category sub-option UI */
  function _renderSubOptions(avSet) {
    var subOptDiv = $("iv-sub-options");
    if (!subOptDiv) return;

    var hasBike    = _checkedIvs.indexOf("bike")      !== -1;
    var hasLane    = _checkedIvs.indexOf("lane_diet") !== -1;
    var hasMajor   = _checkedIvs.indexOf("major")     !== -1;
    var hasPark    = _checkedIvs.indexOf("parking")   !== -1;
    var hasCalming = _checkedIvs.indexOf("calming")   !== -1;

    var html = "";

    if (hasCalming) {
      html += '<div class="iv-sub-section">'
        + '<div class="iv-sub-header">Traffic Calming</div>'
        + '<div class="iv-sub-fixed">Add traffic calming devices</div>'
        + '</div>';
    }

    if (hasLane) {
      html += '<div class="iv-sub-section">'
        + '<div class="iv-sub-header">Lane Diet</div>'
        + '<div class="iv-sub-fixed">Lane diet (−1 lane)</div>'
        + '</div>';
    }

    if (hasMajor && !hasBike) {
      html += '<div class="iv-sub-section">'
        + '<div class="iv-sub-header">Major Road Diet</div>'
        + '<div class="iv-sub-fixed">Major road diet → 2 lanes</div>'
        + '</div>';
    }

    if (hasBike) {
      var bikeCtx = hasMajor ? "with_major" : hasLane ? "with_lane_diet" : "alone";
      var bikeMap = BIKE_SCENARIO_MAP[bikeCtx];
      var bikeOpts = BIKE_OPTIONS.filter(function (o) {
        return !!bikeMap[o.value] && avSet[bikeMap[o.value]];
      });
      var headerLabel = hasMajor ? "Major Road Diet + Bike Lane" : "Bike Lane";
      html += '<div class="iv-sub-section"><div class="iv-sub-header">' + headerLabel + '</div>';
      if (!bikeOpts.length) {
        html += '<div class="iv-sub-fixed iv-sub-none">No options available for this segment</div>';
      } else {
        var curBike = _subSel.bike;
        if (!curBike || !bikeMap[curBike] || !avSet[bikeMap[curBike]]) {
          _subSel.bike = bikeOpts[0].value; curBike = bikeOpts[0].value;
        }
        html += '<select class="iv-sub-select" id="iv-sub-bike">'
          + bikeOpts.map(function (o) {
            return '<option value="' + o.value + '"' + (o.value === curBike ? " selected" : "") + '>' + esc(o.label) + '</option>';
          }).join("") + '</select>';
      }
      html += '</div>';
    }

    if (hasPark) {
      var parkCtx = hasLane ? "with_lane_diet" : "alone";
      var parkMap = PARKING_SCENARIO_MAP[parkCtx];
      var parkOpts = PARKING_OPTIONS.filter(function (o) {
        return !!parkMap[o.value] && avSet[parkMap[o.value]];
      });
      html += '<div class="iv-sub-section"><div class="iv-sub-header">Parking</div>';
      if (!parkOpts.length) {
        html += '<div class="iv-sub-fixed iv-sub-none">No options available for this segment</div>';
      } else {
        var curPark = _subSel.parking;
        if (!curPark || !parkMap[curPark] || !avSet[parkMap[curPark]]) {
          _subSel.parking = parkOpts[0].value; curPark = parkOpts[0].value;
        }
        html += '<select class="iv-sub-select" id="iv-sub-parking">'
          + parkOpts.map(function (o) {
            return '<option value="' + o.value + '"' + (o.value === curPark ? " selected" : "") + '>' + esc(o.label) + '</option>';
          }).join("") + '</select>';
      }
      html += '</div>';
    }

    subOptDiv.innerHTML = html;

    var bikeSelect = $("iv-sub-bike");
    if (bikeSelect) {
      bikeSelect.addEventListener("change", function () {
        _subSel.bike = this.value;
        _showScenarioResult(_resolveScenarioName(avSet));
      });
    }
    var parkSelect = $("iv-sub-parking");
    if (parkSelect) {
      parkSelect.addEventListener("change", function () {
        _subSel.parking = this.value;
        _showScenarioResult(_resolveScenarioName(avSet));
      });
    }

    _showScenarioResult(_resolveScenarioName(avSet));
  }

  /* Determine final scenario name from current checked + sub-selections */
  function _resolveScenarioName(avSet) {
    var hasBike    = _checkedIvs.indexOf("bike")      !== -1;
    var hasLane    = _checkedIvs.indexOf("lane_diet") !== -1;
    var hasMajor   = _checkedIvs.indexOf("major")     !== -1;
    var hasPark    = _checkedIvs.indexOf("parking")   !== -1;
    var hasCalming = _checkedIvs.indexOf("calming")   !== -1;

    /* Traffic calming is standalone */
    if (hasCalming) return avSet["add_traffic_calming"] ? "add_traffic_calming" : null;

    if (hasMajor && hasBike) {
      var bv = _subSel.bike;
      if (bv && BIKE_SCENARIO_MAP.with_major[bv] && avSet[BIKE_SCENARIO_MAP.with_major[bv]])
        return BIKE_SCENARIO_MAP.with_major[bv];
      return avSet["major_road_diet_to_2_lanes"] ? "major_road_diet_to_2_lanes" : null;
    }
    if (hasMajor) return "major_road_diet_to_2_lanes";

    if (hasLane && hasBike) {
      var bv2 = _subSel.bike;
      if (bv2 && BIKE_SCENARIO_MAP.with_lane_diet[bv2] && avSet[BIKE_SCENARIO_MAP.with_lane_diet[bv2]])
        return BIKE_SCENARIO_MAP.with_lane_diet[bv2];
      return avSet["lane_diet"] ? "lane_diet" : null;
    }

    if (hasLane && hasPark) {
      var pv = _subSel.parking;
      if (pv && PARKING_SCENARIO_MAP.with_lane_diet[pv] && avSet[PARKING_SCENARIO_MAP.with_lane_diet[pv]])
        return PARKING_SCENARIO_MAP.with_lane_diet[pv];
      return avSet["lane_diet_add_parking"] ? "lane_diet_add_parking" : (avSet["lane_diet"] ? "lane_diet" : null);
    }

    if (hasLane) return "lane_diet";

    if (hasBike) {
      var bv3 = _subSel.bike;
      if (bv3 && BIKE_SCENARIO_MAP.alone[bv3] && avSet[BIKE_SCENARIO_MAP.alone[bv3]])
        return BIKE_SCENARIO_MAP.alone[bv3];
      return null;
    }

    if (hasPark) {
      var pv2 = _subSel.parking;
      if (pv2 && PARKING_SCENARIO_MAP.alone[pv2] && avSet[PARKING_SCENARIO_MAP.alone[pv2]])
        return PARKING_SCENARIO_MAP.alone[pv2];
      return null;
    }

    return null;
  }

  function _showScenarioResult(scenarioName) {
    var predictions  = OTISData.getScenarioPredictions(_segId, _period);
    var scenarioData = scenarioName ? predictions[scenarioName] : null;
    var currentInfo  = OTISData.getSegmentInfo(_segId, _period);
    var resultDiv    = $("iv-result");

    if (!scenarioData) { if (resultDiv) resultDiv.classList.add("hidden"); return; }
    if (resultDiv) resultDiv.classList.remove("hidden");

    var ecInfo2     = OTISData.getExistingConditions ? OTISData.getExistingConditions(_segId, _period) : null;
    var currentPct  = ecInfo2 ? ecInfo2.predicted_speeding_percent
                     : (currentInfo ? currentInfo.predicted_speeding_percent : null);
    var scenarioPct = scenarioData.predicted_speeding_percent;

    var curEl = $("iv-res-current");
    if (curEl) { curEl.textContent = fmtPct(currentPct); curEl.style.color = currentPct !== null ? OTISModel.riskColor(currentPct) : "#a1a1a1"; }

    var sEl = $("iv-res-scenario");
    if (sEl) { sEl.textContent = fmtPct(scenarioPct); sEl.style.color = scenarioPct !== null ? OTISModel.riskColor(scenarioPct) : "#a1a1a1"; }

    var dEl = $("iv-res-delta");
    if (dEl) {
      if (currentPct !== null && scenarioPct !== null) {
        var diff = scenarioPct - currentPct;
        if (Math.abs(diff) < 0.0005) { dEl.textContent = "No change from current design"; dEl.className = "iv-res-delta"; }
        else if (diff < 0) { dEl.textContent = "▼ " + fmtPct(Math.abs(diff)) + " reduction"; dEl.className = "iv-res-delta delta-good"; }
        else               { dEl.textContent = "▲ " + fmtPct(Math.abs(diff)) + " increase";  dEl.className = "iv-res-delta delta-bad";  }
      } else { dEl.textContent = ""; }
    }

    _drawScenarioXS(scenarioName, scenarioData);
  }

  /* ── Cross-section renderer ─────────────────────────────────────── */
  function _buildSegments(config, oneSideBike) {
    var lanes    = parseInt(config.lanes, 10)    || 2;
    var laneW    = parseFloat(config.lane_width) || 10;
    var bikeType = config.bike_lane              || "None";
    var bikeW    = bikeType === "Separated" ? 6 : 5;
    var parkType = config.parking                || "None";
    var parkW    = 7;
    var swType   = config.sidewalk               || "None";
    var swW      = 5;

    var segs     = [];
    var hasBike  = bikeType !== "None" && bikeType !== "Sharrow";
    var hasParkL = parkType !== "None";
    var hasParkR = parkType === "Both sides";
    var hasSwL   = swType   !== "None";
    var hasSwR   = swType   === "Both sides";

    if (hasSwL)   segs.push({ label: "Sidewalk",  width: swW,   color: "#cfcfcf", type: "sidewalk" });
    if (hasParkL) segs.push({ label: "Parking",   width: parkW, color: "#f3c613", type: "parking"  });
    if (hasBike)  segs.push({ label: "Bike Lane", width: bikeW, color: "#b9f2b1", type: "bike"     });

    var leftLanes  = Math.ceil(lanes / 2);
    var rightLanes = Math.floor(lanes / 2);
    for (var i = 0; i < leftLanes; i++)
      segs.push({ label: "Travel Lane", width: laneW, color: "#e8e8e8", type: "lane" });
    if (rightLanes > 0) {
      segs.push({ label: "", width: 0.5, color: "#999", type: "divider", isLine: true });
      for (var j = 0; j < rightLanes; j++)
        segs.push({ label: "Travel Lane", width: laneW, color: "#e0e0e0", type: "lane" });
    }

    if (hasBike && !oneSideBike) segs.push({ label: "Bike Lane", width: bikeW, color: "#b9f2b1", type: "bike" });
    if (hasParkR)                segs.push({ label: "Parking",   width: parkW, color: "#f3c613", type: "parking"  });
    if (hasSwR)                  segs.push({ label: "Sidewalk",  width: swW,   color: "#cfcfcf", type: "sidewalk" });
    return segs;
  }

  function drawCrossSection(canvasId, legendId, config, oneSideBike) {
    var container = $(canvasId);
    if (!container) return;
    container.innerHTML = "";

    var segs    = _buildSegments(config, oneSideBike);
    var totalFt = segs.reduce(function (s, g) { return s + g.width; }, 0);
    var W       = container.clientWidth || 200;
    var H       = 58;
    var barH    = 38;
    var scale   = totalFt > 0 ? (W - 2) / totalFt : 1;
    var ns      = "http://www.w3.org/2000/svg";
    var svg     = document.createElementNS(ns, "svg");
    svg.setAttribute("width",   "100%");
    svg.setAttribute("height",  H);
    svg.setAttribute("viewBox", "0 0 " + W + " " + H);

    var x = 1;
    segs.forEach(function (seg) {
      var sw   = Math.max(seg.isLine ? 1 : 2, seg.width * scale);
      var rect = document.createElementNS(ns, "rect");
      rect.setAttribute("x", x); rect.setAttribute("y", 0);
      rect.setAttribute("width", sw); rect.setAttribute("height", barH);
      rect.setAttribute("fill", seg.color);
      rect.setAttribute("stroke", "#fff"); rect.setAttribute("stroke-width", "1");
      svg.appendChild(rect);
      if (!seg.isLine && sw > 14 && seg.width >= 1) {
        var ann = document.createElementNS(ns, "text");
        ann.setAttribute("x", x + sw / 2); ann.setAttribute("y", barH + 10);
        ann.setAttribute("text-anchor", "middle"); ann.setAttribute("font-size", "8");
        ann.setAttribute("fill", "#aaa"); ann.setAttribute("font-family", "Open Sans, sans-serif");
        ann.textContent = _fmtW(seg.width);
        svg.appendChild(ann);
      }
      x += sw;
    });

    var tot = document.createElementNS(ns, "text");
    tot.setAttribute("x", W / 2); tot.setAttribute("y", H - 1);
    tot.setAttribute("text-anchor", "middle"); tot.setAttribute("font-size", "8.5");
    tot.setAttribute("fill", "#b0b0b0"); tot.setAttribute("font-family", "Montserrat, sans-serif");
    tot.setAttribute("font-weight", "700");
    tot.textContent = _fmtW(totalFt).replace("'", " ft total");
    svg.appendChild(tot);
    container.appendChild(svg);

    if (legendId) {
      var legendEl = $(legendId);
      if (legendEl) {
        var seen = {}, items = [];
        segs.forEach(function (seg) {
          if (!seg.isLine && seg.type && !seen[seg.type]) {
            seen[seg.type] = true;
            items.push({ color: seg.color, label: seg.label });
          }
        });
        legendEl.innerHTML = items.map(function (item) {
          return '<div class="xs-legend-item">'
            + '<span class="xs-legend-swatch" style="background:' + item.color + '"></span>'
            + '<span class="xs-legend-label">'  + esc(item.label) + '</span>'
            + '</div>';
        }).join("");
      }
    }
  }

  function _drawCurrentXS(info) {
    if (!info) return;
    drawCrossSection("xs-current", "xs-legend-current", {
      lanes:      parseInt(info.lanes) || 2,
      lane_width: parseFloat(info.width_per_traffic_lane) || 10,
      bike_lane:  info.bike_lane_status || "None",
      parking:    info.parking || "None",
      sidewalk:   info.sidewalk_status || "None",
    }, true);
  }

  function _drawBestXS(scenarioName, config) {
    if (!config) return;
    var oneSide = !scenarioName || scenarioName.indexOf("both_sides") === -1;
    drawCrossSection("xs-best", "xs-legend-best", {
      lanes:      parseInt(config.lanes) || 2,
      lane_width: parseFloat(config.width_per_traffic_lane) || 10,
      bike_lane:  config.bike_lane_status || "None",
      parking:    config.parking || "None",
      sidewalk:   config.sidewalk_status || "None",
    }, oneSide);
  }

  function _drawScenarioXS(scenarioName, scenarioConfig) {
    if (!scenarioConfig) return;
    var oneSide = !scenarioName || scenarioName.indexOf("both_sides") === -1;
    drawCrossSection("xs-scenario", "xs-legend-scenario", {
      lanes:      parseInt(scenarioConfig.lanes) || 2,
      lane_width: parseFloat(scenarioConfig.width_per_traffic_lane) || 10,
      bike_lane:  scenarioConfig.bike_lane_status || "None",
      parking:    scenarioConfig.parking || "None",
      sidewalk:   scenarioConfig.sidewalk_status || "None",
    }, oneSide);
  }

  /* ── Search ─────────────────────────────────────────────────────── */
  function _buildSearchIndex() {
    var network = OTISData.getNetworkGeoJSON();
    if (!network || !network.features) return;
    _searchIndex = network.features.map(function (f) {
      var p    = f.properties || {};
      var name = (p.road_name || p.speed_measurement_road || "").replace(/\b\w/g, function (c) { return c.toUpperCase(); });
      return { segId: String(p.seg_id || ""), name: name };
    }).sort(function (a, b) {
      return a.name.localeCompare(b.name) || a.segId.localeCompare(b.segId, undefined, { numeric: true });
    });
  }

  function _renderSearchResults(term) {
    var resultsEl = $("map-search-results");
    if (!resultsEl) return;
    if (!term) { resultsEl.classList.add("hidden"); resultsEl.innerHTML = ""; return; }

    var lower   = term.toLowerCase();
    var matches = _searchIndex.filter(function (item) {
      return item.segId.toLowerCase().indexOf(lower) !== -1
          || item.name.toLowerCase().indexOf(lower) !== -1;
    }).slice(0, 12);

    if (!matches.length) {
      resultsEl.innerHTML = '<div class="search-no-results">No segments found</div>';
      resultsEl.classList.remove("hidden");
      return;
    }

    resultsEl.innerHTML = matches.map(function (item) {
      return '<div class="search-result-item" data-segid="' + esc(item.segId) + '">'
        + '<span class="search-result-name">' + esc(item.name) + '</span>'
        + '<span class="search-result-id">' + esc(item.segId) + '</span>'
        + '</div>';
    }).join("");
    resultsEl.classList.remove("hidden");

    resultsEl.querySelectorAll(".search-result-item").forEach(function (el) {
      el.addEventListener("click", function () {
        var sid = this.dataset.segid;
        if (global.OTISMap && global.OTISMap.selectSegmentById) global.OTISMap.selectSegmentById(sid);
        var inp = $("map-search-input");
        if (inp) inp.value = "";
        var clr = $("map-search-clear");
        if (clr) clr.classList.add("hidden");
        resultsEl.classList.add("hidden");
      });
    });
  }

  function _bindSearchEvents() {
    var input     = $("map-search-input");
    var clearBtn  = $("map-search-clear");
    var resultsEl = $("map-search-results");
    if (!input) return;

    var timer;
    input.addEventListener("input", function () {
      clearTimeout(timer);
      var val = this.value;
      if (clearBtn) clearBtn.classList.toggle("hidden", !val);
      timer = setTimeout(function () { _renderSearchResults(val.trim()); }, 140);
    });

    input.addEventListener("keydown", function (e) {
      if (e.key === "Escape") {
        this.value = "";
        if (clearBtn) clearBtn.classList.add("hidden");
        if (resultsEl) { resultsEl.classList.add("hidden"); resultsEl.innerHTML = ""; }
      }
    });

    if (clearBtn) {
      clearBtn.addEventListener("click", function () {
        input.value = "";
        this.classList.add("hidden");
        if (resultsEl) { resultsEl.classList.add("hidden"); resultsEl.innerHTML = ""; }
        input.focus();
      });
    }

    document.addEventListener("click", function (e) {
      var searchEl = $("overlay-search");
      if (searchEl && !searchEl.contains(e.target) && resultsEl) resultsEl.classList.add("hidden");
    });

    OTISData.onBasicReady(_buildSearchIndex);
  }

  /* ── event bindings ──────────────────────────────────────────────── */
  function bindEvents() {
    var closeBtn = $("rp-close");
    if (closeBtn) closeBtn.addEventListener("click", closePanel);
    _bindMobilePanelDrag();

    var bannerClose = $("banner-close");
    if (bannerClose) bannerClose.addEventListener("click", function () {
      var b = $("network-loading-banner");
      if (b) b.classList.add("hidden");
    });

    var periodCtrl = $("ctrl-period");
    if (periodCtrl) {
      periodCtrl.querySelectorAll(".period-tab").forEach(function (btn) {
        btn.addEventListener("click", function () {
          periodCtrl.querySelectorAll(".period-tab").forEach(function (b) { b.classList.remove("active"); });
          this.classList.add("active");
          _period = this.dataset.period;
          if (global.OTISMap) global.OTISMap.setPeriod(_period);
          onPeriodChange(_period);
        });
      });
    }

    var vizMode = $("ctrl-viz-mode");
    if (vizMode) {
      vizMode.addEventListener("change", function () {
        if (global.OTISMap) global.OTISMap.setVizMode(this.value);
        _updateMapLegend(this.value);
      });
    }

    _bindSearchEvents();
  }

  function _bindMobilePanelDrag() {
    var panel = $("right-panel");
    var header = panel ? panel.querySelector(".rp-header") : null;
    if (!panel || !header) return;

    var startY = null;
    var latestY = null;

    function start(y) {
      if (!_isMobilePanelMode() || !panel.classList.contains("open")) return;
      startY = y;
      latestY = y;
      panel.classList.add("dragging");
    }
    function move(y) {
      if (startY === null) return;
      latestY = y;
    }
    function end() {
      if (startY === null) return;
      var dy = latestY - startY;
      panel.classList.remove("dragging");
      if (dy < -28) _expandMobilePanel();
      else if (dy > 28) _collapseMobilePanel();
      startY = null;
      latestY = null;
    }

    header.addEventListener("touchstart", function (e) {
      if (e.touches && e.touches[0]) start(e.touches[0].clientY);
    }, { passive: true });
    header.addEventListener("touchmove", function (e) {
      if (e.touches && e.touches[0]) move(e.touches[0].clientY);
    }, { passive: true });
    header.addEventListener("touchend", end);

    header.addEventListener("mousedown", function (e) { start(e.clientY); });
    document.addEventListener("mousemove", function (e) { move(e.clientY); });
    document.addEventListener("mouseup", end);
  }

  /* ── map legend ──────────────────────────────────────────────────── */
  function _updateMapLegend(mode) {
    var el = $("map-legend");
    if (!el) return;
    var tpl = {
      prediction:
        '<div class="legend-title">Predicted Speeding %</div>'
        + '<div class="legend-items">'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#cc3000"></span>&ge;40%</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#f99300"></span>30–40%</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#f3c613"></span>20–30%</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#88c740"></span>10–20%</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#58c04d"></span>&lt;10%</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#c0c0c0"></span>No data</div>'
        + '</div>',
      road_class:
        '<div class="legend-title">Road Classification</div>'
        + '<div class="legend-items">'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#3d2280"></span>Major Arterial</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#6544c0"></span>Minor Arterial</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#c0aef5"></span>Collector</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#cfcfcf"></span>Local / Other</div>'
        + '</div>',
      speed_limit:
        '<div class="legend-title">Speed Limit</div>'
        + '<div class="legend-items">'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#cc3000"></span>40+ mph</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#f99300"></span>35 mph</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#f3c613"></span>30 mph</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#58c04d"></span>25 mph</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#a1a1a1"></span>Unknown</div>'
        + '</div>',
      lanes:
        '<div class="legend-title">Number of Lanes</div>'
        + '<div class="legend-items">'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#3d2280"></span>5+ lanes</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#6544c0"></span>4 lanes</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#9a82dd"></span>3 lanes</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#58c04d"></span>2 lanes</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#f3c613"></span>1 lane</div>'
        + '<div class="legend-item"><span class="legend-swatch" style="background:#a1a1a1"></span>Unknown</div>'
        + '</div>',
    };
    el.innerHTML = tpl[mode] || tpl.prediction;
  }

  /* ── public API ──────────────────────────────────────────────────── */
  global.OTISControls = {
    selectSegment,
    closePanel,
    onPeriodChange,
    drawCrossSection,
    getCurrentPeriod: function () { return _period; },
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", bindEvents);
  } else {
    bindEvents();
  }

}(window));
