/**
 * table.js — Road segment data table
 * Loads real data from OTISData (basic_info.geojson).
 * Period selector drives which period's predicted_speeding and all_speeding are shown.
 * All other columns are static (physical attributes).
 */

(function () {
  "use strict";

  /* ── state ──────────────────────────────────────────────────────── */
  var allRows    = [];
  var sortCol    = "road_name";
  var sortDir    = "asc";
  var searchTerm = "";
  var _period    = "night_offpeak";

  /* ── build row objects from OTISData ────────────────────────────── */
  function buildRows() {
    var network = OTISData.getNetworkGeoJSON();
    if (!network || !network.features) return;

    allRows = network.features.map(function (f) {
      var p     = f.properties || {};
      var segId = String(p.seg_id || "");
      var info  = OTISData.getSegmentInfo(segId, _period);
      /* Prefer existing_conditions from v6 for speeding/crash stats */
      var ec    = OTISData.getExistingConditions ? OTISData.getExistingConditions(segId, _period) : null;
      var stats = ec || info;

      /* Static (physical) attrs — prefer info, fall back to geojson props */
      var roadClass  = (info && info.road_classification_city) || p.road_classification_city || "—";
      var speedLimit = (info && info.speed_limit  != null) ? info.speed_limit  : (p.speed_limit  != null ? p.speed_limit  : null);
      var lanes      = (info && info.lanes        != null) ? info.lanes        : (p.lanes        != null ? p.lanes        : null);
      var c2cWidth   = (info && info.curb_to_curb_width != null) ? info.curb_to_curb_width : null;
      var bikeLane   = (info && info.bike_lane_status)  || p.bike_lane_status  || "—";
      var calming    = (info != null) ? (info.traffic_calming ? "Yes" : "No") : "—";

      /* Period-sensitive — from existing_conditions (v6) with basic_info fallback */
      var predSpd = (stats && stats.predicted_speeding_percent != null) ? stats.predicted_speeding_percent : null;
      var allSpd  = (stats && stats.all_speeding_percent  != null) ? stats.all_speeding_percent  : null;
      var crashes = (stats && stats.crashes  != null) ? stats.crashes  : null;
      var ksiRate = (stats && stats.ksi_rate != null) ? stats.ksi_rate : null;

      var roadName = p.road_name || p.speed_measurement_road || "—";
      roadName = roadName.replace(/\b\w/g, function (c) { return c.toUpperCase(); });

      return {
        seg_id:             segId,
        road_name:          roadName,
        road_class:         roadClass,
        predicted_speeding: predSpd,
        all_speeding:       allSpd,
        speed_limit:        speedLimit !== null ? parseFloat(speedLimit) : null,
        lanes:              lanes      !== null ? parseFloat(lanes)      : null,
        c2c_width:          c2cWidth   !== null ? parseFloat(c2cWidth)   : null,
        crashes:            crashes    !== null ? parseFloat(crashes)    : null,
        ksi_rate:           ksiRate,
        bike_lane:          bikeLane,
        calming:            calming,
      };
    });

    render();
  }

  /* Re-fetch period-sensitive fields without rebuilding the full array */
  function refreshPeriodData() {
    allRows = allRows.map(function (row) {
      var info  = OTISData.getSegmentInfo(row.seg_id, _period);
      var ec    = OTISData.getExistingConditions ? OTISData.getExistingConditions(row.seg_id, _period) : null;
      var stats = ec || info;
      return Object.assign({}, row, {
        predicted_speeding: (stats && stats.predicted_speeding_percent != null) ? stats.predicted_speeding_percent : null,
        all_speeding:       (stats && stats.all_speeding_percent  != null) ? stats.all_speeding_percent  : null,
        crashes:            (stats && stats.crashes  != null) ? stats.crashes  : null,
        ksi_rate:           (stats && stats.ksi_rate != null) ? stats.ksi_rate : null,
        calming:            (info  != null) ? (info.traffic_calming ? "Yes" : "No") : row.calming,
      });
    });
    render();
  }

  /* ── filtering ──────────────────────────────────────────────────── */
  function filtered() {
    var term = searchTerm.toLowerCase();
    if (!term) return allRows;
    return allRows.filter(function (row) {
      return row.road_name.toLowerCase().indexOf(term) !== -1
          || row.seg_id.toLowerCase().indexOf(term) !== -1;
    });
  }

  /* ── sorting ────────────────────────────────────────────────────── */
  function sorted(rows) {
    var col = sortCol;
    var dir = sortDir === "asc" ? 1 : -1;
    return rows.slice().sort(function (a, b) {
      var va = a[col];
      var vb = b[col];
      /* numeric columns */
      var numCols = ["predicted_speeding","all_speeding","speed_limit","lanes","c2c_width","crashes","ksi_rate"];
      if (numCols.indexOf(col) !== -1) {
        var na = va !== null && va !== undefined ? parseFloat(va) : -Infinity;
        var nb = vb !== null && vb !== undefined ? parseFloat(vb) : -Infinity;
        return (na - nb) * dir;
      }
      /* seg_id numeric sort */
      if (col === "seg_id") {
        var sa = parseFloat(va); var sb = parseFloat(vb);
        if (!isNaN(sa) && !isNaN(sb)) return (sa - sb) * dir;
      }
      return String(va || "").localeCompare(String(vb || "")) * dir;
    });
  }

  /* ── render ─────────────────────────────────────────────────────── */
  function render() {
    var rows = sorted(filtered());
    renderSummary(rows);
    renderTable(rows);
    renderSortIndicators();
    var countEl = document.getElementById("table-count");
    if (countEl) countEl.textContent = rows.length + " segment" + (rows.length !== 1 ? "s" : "");
  }

  function renderSummary(rows) {
    var set = function (id, val) {
      var el = document.getElementById(id);
      if (el) el.textContent = val;
    };
    set("sum-total", rows.length || "—");

    var veryHigh = 0, high = 0, moderate = 0, low = 0, sumPct = 0, countPct = 0;
    rows.forEach(function (r) {
      if (r.predicted_speeding === null || r.predicted_speeding === undefined) return;
      var p = r.predicted_speeding;
      sumPct += p; countPct++;
      if (p >= 0.40)      veryHigh++;
      else if (p >= 0.30) high++;
      else if (p >= 0.20) moderate++;
      else                low++;
    });

    set("sum-very-high",   veryHigh);
    set("sum-high",        high);
    set("sum-moderate",    moderate);
    set("sum-low",         low);
    set("sum-avg-speeding", countPct > 0 ? (sumPct / countPct * 100).toFixed(1) + "%" : "—");
  }

  function fmtPct(v) {
    if (v === null || v === undefined) return "—";
    return (v * 100).toFixed(1) + "%";
  }
  function fmtN(v, dec) {
    if (v === null || v === undefined) return "—";
    var n = parseFloat(v); if (isNaN(n)) return "—";
    return n.toFixed(dec || 0);
  }
  function esc(s) {
    return String(s || "").replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;");
  }

  function renderTable(rows) {
    var tbody = document.getElementById("table-body");
    if (!tbody) return;
    if (!rows.length) {
      tbody.innerHTML = '<tr><td colspan="13" class="table-empty">No segments match the current search.</td></tr>';
      return;
    }

    var html = rows.map(function (row) {
      var predPct  = row.predicted_speeding;
      var predStr  = fmtPct(predPct);
      var predStyle = predPct !== null ? 'style="color:' + riskColor(predPct) + ';font-weight:700"' : "";

      var allStr   = fmtPct(row.all_speeding);
      var allStyle = row.all_speeding !== null ? 'style="color:' + riskColor(row.all_speeding) + '"' : "";

      return '<tr data-segid="' + esc(row.seg_id) + '" class="data-row">'
        + '<td class="td-mono">' + esc(row.seg_id) + '</td>'
        + '<td class="td-name" title="' + esc(row.road_name) + '">' + esc(row.road_name) + '</td>'
        + '<td>' + esc(row.road_class) + '</td>'
        + '<td class="td-num" ' + predStyle + '>' + predStr + '</td>'
        + '<td class="td-num" ' + allStyle  + '>' + allStr  + '</td>'
        + '<td class="td-num">' + (row.speed_limit !== null ? fmtN(row.speed_limit, 0) + " mph" : "—") + '</td>'
        + '<td class="td-num">' + (row.lanes !== null ? fmtN(row.lanes, 0) : "—") + '</td>'
        + '<td class="td-num">' + (row.c2c_width !== null ? fmtN(row.c2c_width, 0) + " ft" : "—") + '</td>'
        + '<td class="td-num">' + (row.crashes !== null ? fmtN(row.crashes, 0) : "—") + '</td>'
        + '<td class="td-num">' + fmtPct(row.ksi_rate) + '</td>'
        + '<td>' + esc(row.bike_lane) + '</td>'
        + '<td class="td-num">' + esc(row.calming) + '</td>'
        + '<td class="td-action">'
        + '<a href="index.html?seg=' + esc(row.seg_id) + '" class="btn-view-map" title="Open in map">'
        + '<svg width="11" height="11" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" aria-hidden="true"><path d="M21 10c0 7-9 13-9 13s-9-6-9-13a9 9 0 0 1 18 0z"/><circle cx="12" cy="10" r="3"/></svg>'
        + ' Map</a>'
        + '</td>'
        + '</tr>';
    }).join("");

    tbody.innerHTML = html;
  }

  function riskColor(f) {
    if (f >= 0.40) return "#cc3000";
    if (f >= 0.30) return "#f99300";
    if (f >= 0.20) return "#b08800";
    return "#2f8c2f";
  }

  /* ── sort indicators ─────────────────────────────────────────────── */
  function renderSortIndicators() {
    document.querySelectorAll(".th-sortable").forEach(function (th) {
      th.classList.remove("sort-asc", "sort-desc");
      if (th.dataset.col === sortCol)
        th.classList.add(sortDir === "asc" ? "sort-asc" : "sort-desc");
    });
  }

  /* ── CSV export ──────────────────────────────────────────────────── */
  function exportCSV() {
    var rows = sorted(filtered());
    var headers = ["Seg ID","Road Name","Road Class","Predicted Speeding %","Avg Speeding %",
                   "Speed Limit","Lanes","C2C Width (ft)","Crashes","KSI Rate","Bike Lane","Calming"];
    var lines = [headers.join(",")];
    rows.forEach(function (r) {
      lines.push([
        '"' + String(r.seg_id).replace(/"/g, '""')    + '"',
        '"' + String(r.road_name).replace(/"/g, '""') + '"',
        '"' + String(r.road_class).replace(/"/g, '""') + '"',
        r.predicted_speeding !== null ? (r.predicted_speeding * 100).toFixed(1) + "%" : "",
        r.all_speeding       !== null ? (r.all_speeding       * 100).toFixed(1) + "%" : "",
        r.speed_limit  !== null ? r.speed_limit  + " mph" : "",
        r.lanes        !== null ? r.lanes        : "",
        r.c2c_width    !== null ? r.c2c_width    + " ft"  : "",
        r.crashes      !== null ? r.crashes      : "",
        r.ksi_rate     !== null ? (r.ksi_rate * 100).toFixed(1) + "%" : "",
        '"' + String(r.bike_lane).replace(/"/g, '""') + '"',
        r.calming,
      ].join(","));
    });
    var blob = new Blob([lines.join("\n")], { type: "text/csv" });
    var a = document.createElement("a");
    a.href = URL.createObjectURL(blob);
    a.download = "otis_segments_" + _period + ".csv";
    a.click();
    URL.revokeObjectURL(a.href);
  }

  /* ── event bindings ──────────────────────────────────────────────── */
  function bindEvents() {
    /* Sortable headers */
    document.querySelectorAll(".th-sortable").forEach(function (th) {
      th.addEventListener("click", function () {
        var col = this.dataset.col;
        if (sortCol === col) { sortDir = sortDir === "asc" ? "desc" : "asc"; }
        else                 { sortCol = col; sortDir = col === "road_name" || col === "seg_id" ? "asc" : "desc"; }
        render();
      });
    });

    /* Period buttons */
    var periodGroup = document.getElementById("tbl-period-group");
    if (periodGroup) {
      periodGroup.querySelectorAll(".tbl-period-btn").forEach(function (btn) {
        btn.addEventListener("click", function () {
          periodGroup.querySelectorAll(".tbl-period-btn").forEach(function (b) { b.classList.remove("active"); });
          this.classList.add("active");
          _period = this.dataset.period;
          refreshPeriodData();
        });
      });
    }

    /* Search */
    var searchEl = document.getElementById("tbl-search");
    if (searchEl) {
      var timer;
      searchEl.addEventListener("input", function () {
        clearTimeout(timer);
        var val = this.value;
        timer = setTimeout(function () { searchTerm = val.trim(); render(); }, 180);
      });
    }

    /* Export */
    var exportBtn = document.getElementById("tbl-export");
    if (exportBtn) exportBtn.addEventListener("click", exportCSV);
  }

  /* ── bootstrap ───────────────────────────────────────────────────── */
  document.addEventListener("DOMContentLoaded", function () {
    bindEvents();
    /* Load both GeoJSON files; build rows once scenario data (v6) is ready */
    OTISData.load(null, null);
    OTISData.onAllReady(buildRows);   /* wait for v6 so existing_conditions stats are available */
  });

}());
