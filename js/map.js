/**
 * map.js — dashboard map
 *
 * Popup shows all segment attributes (static) + period-sensitive data at bottom.
 * Popup content is rebuilt whenever the time period changes.
 */

/* ── state ──────────────────────────────────────────────────────── */
var otisMap            = null;
var otisNetworkLayer   = null;
var otisSelectedLayer  = null;
var otisVizMode        = "prediction";
var otisPeriod         = "night_offpeak";
var _currentPopupLayer = null;  /* track which layer has the open popup */

var OTIS_PERIOD_DISPLAY = {
  night_offpeak:  "Night Off-Peak",
  morning_peak:   "Morning Peak",
  midday_offpeak: "Midday Off-Peak",
  evening_peak:   "Evening Peak",
};

/* ── colour helpers ─────────────────────────────────────────────── */
function otisSpeedingColor(pct) {
  if (pct === null || pct === undefined) return "#c0c0c0";
  if (pct >= 0.40) return "#cc3000";
  if (pct >= 0.30) return "#f99300";
  if (pct >= 0.20) return "#f3c613";
  if (pct >= 0.10) return "#88c740";
  return "#58c04d";
}

function otisFeatureColor(props) {
  if (otisVizMode === "road_class")
    return OTISModel.roadClassColor(props.road_classification_city || "");
  if (otisVizMode === "speed_limit")
    return OTISModel.speedLimitColor(props.speed_limit);
  if (otisVizMode === "lanes")
    return OTISModel.lanesColor(props.lanes);
  var info = OTISData.getSegmentInfo(String(props.seg_id), otisPeriod);
  var pct  = info ? info.predicted_speeding_percent : null;
  return otisSpeedingColor(pct);
}

/* ── line weight ────────────────────────────────────────────────── */
function otisLineWeight(zoom) {
  if (zoom >= 17) return 5;
  if (zoom >= 15) return 3.5;
  if (zoom >= 13) return 2.5;
  return 1.8;
}

/* ── style callback ─────────────────────────────────────────────── */
function otisStyleFeature(feature) {
  var props      = feature.properties || {};
  var zoom       = otisMap ? otisMap.getZoom() : 13;
  var isSelected = otisSelectedLayer && otisSelectedLayer.feature === feature;
  var color      = otisFeatureColor(props);
  return {
    color:    isSelected ? "#3d2280" : color,
    weight:   isSelected ? otisLineWeight(zoom) + 2.5 : otisLineWeight(zoom),
    opacity:  isSelected ? 1.0 : 0.82,
    lineCap:  "round",
    lineJoin: "round",
  };
}

/* ── per-feature events ─────────────────────────────────────────── */
function otisOnEachFeature(feature, layer) {
  layer.on({
    mouseover: function (e) {
      if (e.target === otisSelectedLayer) return;
      e.target.setStyle({ weight: otisLineWeight(otisMap.getZoom()) + 1.5, opacity: 1.0 });
      e.target.bringToFront();
    },
    mouseout: function (e) {
      if (e.target === otisSelectedLayer) return;
      otisNetworkLayer.resetStyle(e.target);
    },
    click: otisOnSegmentClick,
  });

  var p    = feature.properties || {};
  var name = p.road_name || p.speed_measurement_road || "Road Segment";
  name = name.replace(/\b\w/g, function (c) { return c.toUpperCase(); });
  layer.bindTooltip(name, { className: "road-tooltip", sticky: true, offset: [0, -4] });
}

function otisApplySelectedStyle(layer) {
  var zoom = otisMap ? otisMap.getZoom() : 13;
  layer.setStyle({ color: "#3d2280", weight: otisLineWeight(zoom) + 2.5, opacity: 1.0 });
  layer.bringToFront();
}

/* ── popup builder ──────────────────────────────────────────────── */
function _buildPopupHtml(segId, name, period) {
  var info   = OTISData.getSegmentInfo(segId, period);
  /* Prefer existing_conditions row from v4 geojson for observed/predicted stats */
  var ecInfo = (OTISData.getExistingConditions && OTISData.getExistingConditions(segId, period)) || null;
  var stats  = ecInfo || info;   /* fallback to basic_info if scenario data not yet loaded */

  function escP(s) {
    return String(s || "").replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;");
  }
  function fmtPctP(f) { return (f !== null && f !== undefined) ? (f * 100).toFixed(1) + "%" : "—"; }
  function fmtN(v, dec, sfx) {
    if (v === null || v === undefined) return "—";
    var n = parseFloat(v); if (isNaN(n)) return "—";
    return n.toFixed(dec || 0) + (sfx || "");
  }

  /* Static fields (physical, don't change with period) */
  var roadClassCity = info ? (info.road_classification_city || "—") : "—";
  var roadClassFhwa = info ? (info.road_classification_fhwa || "—") : "—";
  var artType       = info ? (info.arterial_type_dvrpc      || "—") : "—";
  var speedLimit    = info ? (info.speed_limit ? info.speed_limit + " mph" : "—") : "—";
  var lanes         = info ? (info.lanes || "—") : "—";
  var trafficDir    = info ? (info.traffic_direction        || "—") : "—";
  var bikeLane      = info ? (info.bike_lane_status         || "—") : "—";
  var parking       = info ? (info.parking                  || "—") : "—";
  var sidewalk      = info ? (info.sidewalk_status          || "—") : "—";
  var c2c           = info ? fmtN(info.curb_to_curb_width,        0, " ft") : "—";
  var laneW         = info ? fmtN(info.width_per_traffic_lane,    1, " ft") : "—";
  var shoulder      = info ? fmtN(info.shoulder_width,            0, " ft") : "—";
  var segLength     = info ? fmtN(info.length,                    0, " ft") : "—";
  var calming       = info ? (info.traffic_calming ? "Yes" : "No") : "—";
  var transit       = info ? fmtN(info.count_transit,             0)        : "—";
  var intersect     = info ? fmtN(info.count_intersection_ctrl,   0)        : "—";
  var parcel        = info ? fmtN(info.parcel_density,            4)        : "—";
  /* Crashes & KSI from existing_conditions (or basic_info fallback) */
  var crashes = stats && stats.crashes !== null && stats.crashes !== undefined
                ? Math.round(stats.crashes) : "—";
  var ksi     = stats ? fmtPctP(stats.ksi_rate) : "—";

  /* Period-sensitive fields — speeding stats from existing_conditions; volume from basic_info */
  var vol     = info && info.volume_total !== null && info.volume_total !== undefined
                ? Math.round(info.volume_total).toLocaleString() : "—";
  var allSpd  = stats ? fmtPctP(stats.all_speeding_percent)      : "—";
  var hiSpd   = stats ? fmtPctP(stats.high_speeding_percent)     : "—";
  var predSpd = stats ? fmtPctP(stats.predicted_speeding_percent) : "—";

  var predColor   = stats && stats.predicted_speeding_percent !== null
                    ? OTISModel.riskColor(stats.predicted_speeding_percent) : "#555";
  var allSpdColor = stats && stats.all_speeding_percent !== null
                    ? OTISModel.riskColor(stats.all_speeding_percent)       : "#555";
  var hiSpdColor  = stats && stats.high_speeding_percent !== null
                    ? OTISModel.riskColor(stats.high_speeding_percent)      : "#555";

  var periodLabel = OTIS_PERIOD_DISPLAY[period] || period;

  return '<div class="popup-inner">'
    + '<div class="popup-road-name">' + escP(name) + '</div>'
    + '<div class="popup-grid">'
    + _pi("Seg ID",          '<span class="popup-mono">' + escP(segId) + '</span>')
    + _pi("Road Class",      escP(roadClassCity))
    + _pi("FHWA Class",      escP(roadClassFhwa))
    + _pi("DVRPC Type",      escP(artType))
    + _pi("Length",          escP(segLength))
    + _pi("Traffic Calming", escP(calming))
    + _pi("Transit Stops",   escP(transit))
    + _pi("Intersections",   escP(intersect))
    + _pi("Parcel Density",  escP(parcel))
    + '</div>'
    /* Period-sensitive section */
    + '<div class="popup-period-section">'
    + '<div class="popup-period-header">' + escP(periodLabel) + '</div>'
    + '<div class="popup-observed-label">Observed values</div>'
    + '<div class="popup-safety-grid">'
    + _pi('<span class="popup-src-label">Crashes <span class="popup-src-year">(2020–2024)</span></span>', escP(crashes))
    + _pi('<span class="popup-src-label">KSI Rate <span class="popup-src-year">(2020–2024)</span></span>', escP(ksi))
    + '</div>'
    + '<div class="popup-period-grid">'
    + _pi('<span class="popup-src-label">Avg Volume <span class="popup-src-year">(2022–2025)</span></span>', vol)
    + _pi2('<span class="popup-src-label">Avg speeding (1mph+ over) <span class="popup-src-year">(2022–2025)</span></span>', allSpd, allSpdColor)
    + _pi2('<span class="popup-src-label">Avg speeding (10mph+ over) <span class="popup-src-year">(2022–2025)</span></span>', hiSpd, hiSpdColor)
    + '</div>'
    + '<div class="popup-pred-row">'
    + '<span class="popup-pred-label">Predicted Speeding <span class="popup-pred-type">Model estimate</span></span>'
    + '<span class="popup-pred-val" style="color:' + predColor + '">' + predSpd + '</span>'
    + '</div>'
    + '</div>'
    + '</div>';
}

/* Helper: popup item */
function _pi(label, valueHtml) {
  return '<div><div class="popup-item-label">' + label + '</div>'
       + '<div class="popup-item-value">' + valueHtml + '</div></div>';
}
function _pi2(label, value, color) {
  return '<div><div class="popup-item-label">' + label + '</div>'
       + '<div class="popup-item-value popup-period-val" style="color:' + color + '">' + value + '</div></div>';
}

/* ── segment click ──────────────────────────────────────────────── */
function otisOnSegmentClick(e) {
  e.originalEvent._segClick = true;
  var layer = e.target;
  var feat  = layer.feature;

  if (otisSelectedLayer && otisSelectedLayer !== layer)
    otisNetworkLayer.resetStyle(otisSelectedLayer);
  otisSelectedLayer = layer;
  otisApplySelectedStyle(layer);

  var p     = feat.properties || {};
  var segId = String(p.seg_id || "—");
  var name  = (p.road_name || p.speed_measurement_road || "Road Segment")
              .replace(/\b\w/g, function (c) { return c.toUpperCase(); });

  layer.bindPopup(_buildPopupHtml(segId, name, otisPeriod), {
    maxWidth: 320, autoPan: true,
  }).openPopup();
  _currentPopupLayer = layer;  /* assign AFTER openPopup so bindPopup's popupclose doesn't clear it */

  if (typeof OTISControls !== "undefined") OTISControls.selectSegment(feat);
}

/* ── build Leaflet layer ────────────────────────────────────────── */
function otisBuildLayer(geojson) {
  if (otisNetworkLayer) otisMap.removeLayer(otisNetworkLayer);
  otisNetworkLayer = L.geoJSON(geojson, {
    style:         otisStyleFeature,
    onEachFeature: otisOnEachFeature,
  }).addTo(otisMap);

  try {
    var bounds = otisNetworkLayer.getBounds();
    if (bounds.isValid()) otisMap.fitBounds(bounds, { padding: [40, 40] });
  } catch (e) { /* ignore */ }
}

/* ── clear selection ────────────────────────────────────────────── */
function otisClearSelection() {
  if (otisSelectedLayer) {
    var prev = otisSelectedLayer;
    otisSelectedLayer = null;   /* null BEFORE resetStyle so style fn sees isSelected=false */
    if (prev.closePopup) prev.closePopup();
    otisNetworkLayer.resetStyle(prev);
  }
  _currentPopupLayer = null;
}

/* ── refresh colours ────────────────────────────────────────────── */
function otisRefreshColors() {
  if (!otisNetworkLayer) return;
  otisNetworkLayer.eachLayer(function (l) {
    if (l !== otisSelectedLayer) otisNetworkLayer.resetStyle(l);
  });
  if (otisSelectedLayer) otisApplySelectedStyle(otisSelectedLayer);
}

/* ── rebuild popup after period change ──────────────────────────── */
function _rebuildOpenPopup(period) {
  if (!_currentPopupLayer) return;
  if (!_currentPopupLayer.isPopupOpen || !_currentPopupLayer.isPopupOpen()) return;
  var p     = (_currentPopupLayer.feature && _currentPopupLayer.feature.properties) || {};
  var segId = String(p.seg_id || "—");
  var name  = (p.road_name || p.speed_measurement_road || "Road Segment")
              .replace(/\b\w/g, function (c) { return c.toUpperCase(); });
  _currentPopupLayer.setPopupContent(_buildPopupHtml(segId, name, period));
}

/* ── main init ──────────────────────────────────────────────────── */
function otisInitMap() {
  if (otisMap) return;
  if (typeof L === "undefined") {
    var el = document.getElementById("map");
    if (el) el.textContent = "Leaflet failed to load.";
    return;
  }

  var container = document.getElementById("map");
  if (!container) return;
  if (container.offsetHeight === 0)
    container.style.height = (window.innerHeight - 64) + "px";

  otisMap = L.map("map", {
    center: [39.9526, -75.1652], zoom: 13, minZoom: 10, maxZoom: 19,
  });
  otisMap.zoomControl.setPosition("topright");

  L.tileLayer(
    "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
    { attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> &copy; <a href="https://carto.com/attributions">CARTO</a>', subdomains: "abcd", maxZoom: 19 }
  ).addTo(otisMap);

  otisMap.on("zoomend", function () {
    if (otisNetworkLayer) otisNetworkLayer.eachLayer(function (l) { otisNetworkLayer.resetStyle(l); });
    if (otisSelectedLayer) otisApplySelectedStyle(otisSelectedLayer);
  });

  otisMap.on("click", function (e) {
    if (!e.originalEvent._segClick) {
      otisClearSelection();
      if (typeof OTISControls !== "undefined") OTISControls.closePanel();
    }
  });

  var banner = document.getElementById("network-loading-banner");
  var msg    = document.getElementById("banner-message");
  if (banner) banner.classList.remove("hidden");
  if (msg)    msg.textContent = "Loading road network…";

  OTISData.onBasicReady(function () {
    var geojson = OTISData.getNetworkGeoJSON();
    if (geojson && geojson.features && geojson.features.length) {
      otisBuildLayer(geojson);
      if (banner) banner.classList.add("hidden");
      /* Handle ?seg= URL parameter from Data Table "Map" button */
      try {
        var urlParams = new URLSearchParams(window.location.search);
        var segParam  = urlParams.get("seg");
        if (segParam) {
          setTimeout(function () {
            if (window.OTISMap) window.OTISMap.selectSegmentById(segParam);
          }, 200);
        }
      } catch (e) { /* ignore */ }
    } else {
      if (msg) msg.textContent = "No features found in basic_info.geojson.";
    }
  });

  setTimeout(function () { if (otisMap) otisMap.invalidateSize(); }, 150);
}

/* ── public API ─────────────────────────────────────────────────── */
window.OTISMap = {
  refreshColors:  otisRefreshColors,
  clearSelection: otisClearSelection,
  setVizMode: function (mode) { otisVizMode = mode; otisRefreshColors(); },
  setPeriod:  function (period) {
    otisPeriod = period;
    otisRefreshColors();
    _rebuildOpenPopup(period);
    if (typeof OTISControls !== "undefined") OTISControls.onPeriodChange(period);
  },
  getPeriod:  function () { return otisPeriod; },
  selectSegmentById: function (segId) {
    if (!otisNetworkLayer) return false;
    var target = null;
    otisNetworkLayer.eachLayer(function (l) {
      if (target) return;
      var p = l.feature && l.feature.properties;
      if (p && String(p.seg_id) === String(segId)) target = l;
    });
    if (!target) return false;
    try {
      var bounds = target.getBounds();
      if (bounds.isValid()) otisMap.fitBounds(bounds, { maxZoom: 16, padding: [100, 160] });
    } catch (e) {}
    if (otisSelectedLayer && otisSelectedLayer !== target)
      otisNetworkLayer.resetStyle(otisSelectedLayer);
    otisSelectedLayer = target;
    otisApplySelectedStyle(target);
    var p2   = target.feature.properties || {};
    var sId  = String(p2.seg_id || "—");
    var name = (p2.road_name || p2.speed_measurement_road || "Road Segment")
               .replace(/\b\w/g, function (c) { return c.toUpperCase(); });
    target.bindPopup(_buildPopupHtml(sId, name, otisPeriod), { maxWidth: 320, autoPan: true }).openPopup();
    _currentPopupLayer = target;
    if (typeof OTISControls !== "undefined") OTISControls.selectSegment(target.feature);
    return true;
  },
};

/* ── onboarding tour ─────────────────────────────────────────────── */
var _TOUR_CONTENT = {
  map: {
    title: "Exploring the Risk Map",
    body: "Each road segment is colored by predicted speeding risk. Use the time period tabs at the top to see how risk changes throughout the day. Switch the \"Color roads by\" dropdown to explore road classifications, lanes, or speed limits.",
  },
  simulator: {
    title: "Using the Intervention Simulator",
    body: "Click any road segment to open the detail panel on the right. Select an intervention type — lane diet, bike lanes, traffic calming, or parking changes — to see the model's predicted impact on off-peak speeding.",
  },
};

function _showTour(type) {
  var content = _TOUR_CONTENT[type];
  if (!content) return;
  var banner  = document.getElementById("map-tour-banner");
  var titleEl = document.getElementById("tour-banner-title");
  var bodyEl  = document.getElementById("tour-banner-body");
  if (!banner) return;
  if (titleEl) titleEl.textContent = content.title;
  if (bodyEl)  bodyEl.textContent  = content.body;
  banner.classList.remove("hidden");
  var closeBtn = document.getElementById("tour-banner-close");
  if (closeBtn) closeBtn.addEventListener("click", function () {
    banner.classList.add("hidden");
  });
}

window._showTour = _showTour;  /* expose for disclaimer callback */

/* ── bootstrap ──────────────────────────────────────────────────── */
function otisStart() {
  OTISData.load(null, null);
  otisInitMap();
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", otisStart);
} else {
  otisStart();
}
window.addEventListener("load", function () {
  if (otisMap) otisMap.invalidateSize();
});
