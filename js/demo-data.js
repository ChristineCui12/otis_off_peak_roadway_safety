/**
 * demo-data.js
 * Synthetic Philadelphia road segment dataset for browser demo
 * (used when assets/network.geojson is not yet generated from the .rds file).
 *
 * ~55 representative segments spanning all road classes and neighborhoods.
 * Geometry coordinates are [lng, lat] (GeoJSON convention).
 */
window.OTISDemoData = (function () {
  "use strict";

  // Helper: build a GeoJSON Feature for a straight-line road segment
  function seg(id, name, cls, lngs, lats, opts) {
    const props = Object.assign({
      seg_id:                   String(id),
      road_name:                name,
      road_classification_city: cls,
      lanes:                    2,
      width:                    36,
      speed_limit:              25,
      divided_roadway:          false,
      traffic_calming:          false,
      count_calming:            0,
      bike_lane_type_simple:    "None",
      parking:                  "Both sides",
      sidewalk_status:          "both_sides",
      count_transit:            2,
      count_intersection_ctrl:  3,
      parcel_density:           0.04,
      length:                   500,
      total_crashes:            5,
      ksi_rate:                 0.05,
    }, opts);

    // Build coordinate array from parallel lat/lng arrays
    const coords = lngs.map((lng, i) => [lng, lats[i]]);

    return {
      type: "Feature",
      properties: props,
      geometry: { type: "LineString", coordinates: coords }
    };
  }

  const features = [
    // ── MAJOR ARTERIALS ─────────────────────────────────────────────────────
    seg(1, "Broad St", "Major Arterial",
      [-75.1638, -75.1638], [39.9380, 39.9580],
      { lanes: 4, width: 80, speed_limit: 35, divided_roadway: true,
        bike_lane_type_simple: "None", parking: "None",
        count_transit: 8, count_intersection_ctrl: 6, total_crashes: 42, ksi_rate: 0.09 }),

    seg(2, "Broad St (N)", "Major Arterial",
      [-75.1638, -75.1638], [39.9580, 39.9800],
      { lanes: 4, width: 78, speed_limit: 35, divided_roadway: true,
        bike_lane_type_simple: "None", parking: "None",
        count_transit: 10, count_intersection_ctrl: 7, total_crashes: 38, ksi_rate: 0.08 }),

    seg(3, "Market St", "Major Arterial",
      [-75.1850, -75.1480], [39.9522, 39.9522],
      { lanes: 4, width: 72, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "Painted", parking: "None",
        count_transit: 12, count_intersection_ctrl: 8, total_crashes: 55, ksi_rate: 0.10 }),

    seg(4, "Market St (E)", "Major Arterial",
      [-75.1480, -75.1340], [39.9522, 39.9522],
      { lanes: 4, width: 68, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "Painted", parking: "None",
        count_transit: 9, count_intersection_ctrl: 7, total_crashes: 40, ksi_rate: 0.07 }),

    seg(5, "Roosevelt Blvd (NE)", "Major Arterial",
      [-75.0900, -75.0600], [40.0300, 40.0620],
      { lanes: 6, width: 120, speed_limit: 45, divided_roadway: true,
        bike_lane_type_simple: "None", parking: "None",
        count_transit: 4, count_intersection_ctrl: 4, total_crashes: 88, ksi_rate: 0.18 }),

    seg(6, "Roosevelt Blvd (central)", "Major Arterial",
      [-75.0600, -75.0350], [40.0620, 40.0880],
      { lanes: 6, width: 120, speed_limit: 45, divided_roadway: true,
        bike_lane_type_simple: "None", parking: "None",
        count_transit: 3, count_intersection_ctrl: 3, total_crashes: 76, ksi_rate: 0.16 }),

    seg(7, "Aramingo Ave", "Major Arterial",
      [-75.1050, -75.0750], [39.9700, 40.0100],
      { lanes: 4, width: 64, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "Sharrow", parking: "One side",
        count_transit: 5, count_intersection_ctrl: 5, total_crashes: 35, ksi_rate: 0.10 }),

    seg(8, "Frankford Ave", "Major Arterial",
      [-75.0950, -75.0620], [40.0000, 40.0450],
      { lanes: 3, width: 55, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "One side",
        count_transit: 6, count_intersection_ctrl: 6, total_crashes: 32, ksi_rate: 0.09 }),

    seg(9, "Germantown Ave (NW)", "Major Arterial",
      [-75.1700, -75.1950], [39.9920, 40.0300],
      { lanes: 2, width: 50, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 7, count_intersection_ctrl: 5, total_crashes: 28, ksi_rate: 0.08 }),

    seg(10, "Girard Ave", "Major Arterial",
      [-75.1900, -75.1350], [39.9700, 39.9700],
      { lanes: 4, width: 70, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "Separated", parking: "None",
        count_transit: 8, count_intersection_ctrl: 6, total_crashes: 45, ksi_rate: 0.11 }),

    seg(11, "Passyunk Ave", "Major Arterial",
      [-75.1780, -75.1480], [39.9430, 39.9600],
      { lanes: 2, width: 45, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "Painted", parking: "Both sides",
        count_transit: 5, count_intersection_ctrl: 6, total_crashes: 22, ksi_rate: 0.07 }),

    seg(12, "Baltimore Ave", "Major Arterial",
      [-75.2000, -75.1650], [39.9500, 39.9600],
      { lanes: 2, width: 52, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "Painted", parking: "One side",
        count_transit: 6, count_intersection_ctrl: 5, total_crashes: 24, ksi_rate: 0.07 }),

    seg(13, "Woodland Ave", "Major Arterial",
      [-75.2000, -75.1700], [39.9370, 39.9500],
      { lanes: 2, width: 48, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 5, count_intersection_ctrl: 4, total_crashes: 20, ksi_rate: 0.06 }),

    seg(14, "Oregon Ave", "Major Arterial",
      [-75.1850, -75.1450], [39.9230, 39.9230],
      { lanes: 4, width: 72, speed_limit: 35, divided_roadway: true,
        bike_lane_type_simple: "None", parking: "None",
        count_transit: 4, count_intersection_ctrl: 5, total_crashes: 30, ksi_rate: 0.09 }),

    seg(15, "Ridge Ave", "Major Arterial",
      [-75.1900, -75.1550], [39.9750, 39.9950],
      { lanes: 3, width: 56, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "Sharrow", parking: "One side",
        count_transit: 6, count_intersection_ctrl: 5, total_crashes: 29, ksi_rate: 0.08 }),

    // ── MINOR ARTERIALS ──────────────────────────────────────────────────────
    seg(16, "South St", "Minor Arterial",
      [-75.1800, -75.1450], [39.9428, 39.9428],
      { lanes: 2, width: 42, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "Painted", parking: "Both sides",
        count_transit: 4, count_intersection_ctrl: 5, total_crashes: 15, ksi_rate: 0.05 }),

    seg(17, "Spring Garden St", "Minor Arterial",
      [-75.1850, -75.1450], [39.9610, 39.9610],
      { lanes: 2, width: 44, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "Painted", parking: "Both sides",
        count_transit: 5, count_intersection_ctrl: 4, total_crashes: 18, ksi_rate: 0.06 }),

    seg(18, "Fairmount Ave", "Minor Arterial",
      [-75.1850, -75.1500], [39.9635, 39.9635],
      { lanes: 2, width: 40, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 4, count_intersection_ctrl: 4, total_crashes: 14, ksi_rate: 0.05 }),

    seg(19, "Washington Ave", "Minor Arterial",
      [-75.1800, -75.1450], [39.9385, 39.9385],
      { lanes: 3, width: 55, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "Separated", parking: "One side",
        count_transit: 5, count_intersection_ctrl: 4, total_crashes: 20, ksi_rate: 0.07 }),

    seg(20, "Chestnut St", "Minor Arterial",
      [-75.1750, -75.1430], [39.9497, 39.9497],
      { lanes: 2, width: 40, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "Painted", parking: "One side",
        count_transit: 7, count_intersection_ctrl: 6, total_crashes: 16, ksi_rate: 0.05 }),

    seg(21, "Walnut St", "Minor Arterial",
      [-75.1750, -75.1430], [39.9476, 39.9476],
      { lanes: 2, width: 38, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "One side",
        count_transit: 6, count_intersection_ctrl: 6, total_crashes: 12, ksi_rate: 0.04 }),

    seg(22, "Cecil B Moore Ave", "Minor Arterial",
      [-75.2000, -75.1550], [39.9795, 39.9795],
      { lanes: 2, width: 44, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "Painted", parking: "Both sides",
        count_transit: 6, count_intersection_ctrl: 4, total_crashes: 17, ksi_rate: 0.06 }),

    seg(23, "Lehigh Ave", "Minor Arterial",
      [-75.2000, -75.1450], [39.9935, 39.9935],
      { lanes: 3, width: 52, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "One side",
        count_transit: 4, count_intersection_ctrl: 3, total_crashes: 22, ksi_rate: 0.08 }),

    seg(24, "Allegheny Ave", "Minor Arterial",
      [-75.1950, -75.1450], [40.0040, 40.0040],
      { lanes: 3, width: 54, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "One side",
        count_transit: 4, count_intersection_ctrl: 3, total_crashes: 25, ksi_rate: 0.09 }),

    seg(25, "Hunting Park Ave", "Minor Arterial",
      [-75.1850, -75.1400], [40.0070, 40.0070],
      { lanes: 2, width: 46, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "Sharrow", parking: "Both sides",
        count_transit: 5, count_intersection_ctrl: 4, total_crashes: 18, ksi_rate: 0.07 }),

    seg(26, "Cheltenham Ave", "Minor Arterial",
      [-75.1250, -75.0750], [40.0780, 40.0780],
      { lanes: 4, width: 65, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "None",
        count_transit: 5, count_intersection_ctrl: 5, total_crashes: 28, ksi_rate: 0.08 }),

    // ── COLLECTOR RESIDENTIAL ────────────────────────────────────────────────
    seg(27, "22nd St", "Collector Residential",
      [-75.1790, -75.1790], [39.9430, 39.9700],
      { lanes: 2, width: 32, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 2, count_intersection_ctrl: 4, total_crashes: 7, ksi_rate: 0.03 }),

    seg(28, "5th St", "Collector Residential",
      [-75.1485, -75.1485], [39.9420, 39.9700],
      { lanes: 1, width: 28, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "Sharrow", parking: "Both sides",
        count_transit: 3, count_intersection_ctrl: 5, total_crashes: 5, ksi_rate: 0.02 }),

    seg(29, "2nd St", "Collector Residential",
      [-75.1398, -75.1398], [39.9420, 39.9720],
      { lanes: 1, width: 26, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "Sharrow", parking: "Both sides",
        count_transit: 2, count_intersection_ctrl: 4, total_crashes: 6, ksi_rate: 0.02 }),

    seg(30, "Front St", "Collector Residential",
      [-75.1365, -75.1365], [39.9380, 39.9700],
      { lanes: 2, width: 34, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "One side",
        count_transit: 3, count_intersection_ctrl: 4, total_crashes: 8, ksi_rate: 0.03 }),

    seg(31, "Kensington Ave", "Collector Residential",
      [-75.1330, -75.1330], [39.9780, 40.0100],
      { lanes: 2, width: 36, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 4, count_intersection_ctrl: 3, total_crashes: 12, ksi_rate: 0.05 }),

    seg(32, "Torresdale Ave", "Collector Residential",
      [-75.0450, -75.0250], [40.0500, 40.0800],
      { lanes: 2, width: 38, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 2, count_intersection_ctrl: 3, total_crashes: 9, ksi_rate: 0.03 }),

    seg(33, "Ogontz Ave", "Collector Residential",
      [-75.1600, -75.1600], [40.0200, 40.0650],
      { lanes: 2, width: 36, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "Sharrow", parking: "Both sides",
        count_transit: 3, count_intersection_ctrl: 3, total_crashes: 8, ksi_rate: 0.04 }),

    seg(34, "Lancaster Ave", "Collector Residential",
      [-75.2100, -75.1750], [39.9690, 39.9820],
      { lanes: 2, width: 42, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "Painted", parking: "Both sides",
        count_transit: 5, count_intersection_ctrl: 4, total_crashes: 11, ksi_rate: 0.04 }),

    seg(35, "42nd St", "Collector Residential",
      [-75.2090, -75.2090], [39.9450, 39.9750],
      { lanes: 2, width: 34, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 2, count_intersection_ctrl: 3, total_crashes: 6, ksi_rate: 0.02 }),

    seg(36, "58th St", "Collector Residential",
      [-75.2280, -75.2280], [39.9400, 39.9700],
      { lanes: 2, width: 33, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 2, count_intersection_ctrl: 3, total_crashes: 5, ksi_rate: 0.02 }),

    seg(37, "Pattison Ave", "Collector Residential",
      [-75.1850, -75.1480], [39.9240, 39.9240],
      { lanes: 2, width: 44, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "Painted", parking: "One side",
        count_transit: 3, count_intersection_ctrl: 4, total_crashes: 7, ksi_rate: 0.02 }),

    // ── LOCAL RESIDENTIAL ────────────────────────────────────────────────────
    seg(38, "Pine St", "Local Residential",
      [-75.1750, -75.1440], [39.9480, 39.9480],
      { lanes: 1, width: 24, speed_limit: 25, divided_roadway: false,
        traffic_calming: true, count_calming: 3,
        bike_lane_type_simple: "Separated", parking: "One side",
        count_transit: 2, count_intersection_ctrl: 4, total_crashes: 3, ksi_rate: 0.01 }),

    seg(39, "Spruce St", "Local Residential",
      [-75.1750, -75.1440], [39.9490, 39.9490],
      { lanes: 1, width: 24, speed_limit: 25, divided_roadway: false,
        traffic_calming: true, count_calming: 2,
        bike_lane_type_simple: "Separated", parking: "One side",
        count_transit: 2, count_intersection_ctrl: 3, total_crashes: 4, ksi_rate: 0.01 }),

    seg(40, "Arch St", "Local Residential",
      [-75.1720, -75.1450], [39.9550, 39.9550],
      { lanes: 1, width: 26, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 3, count_intersection_ctrl: 3, total_crashes: 4, ksi_rate: 0.02 }),

    seg(41, "Lombard St", "Local Residential",
      [-75.1740, -75.1450], [39.9465, 39.9465],
      { lanes: 1, width: 22, speed_limit: 25, divided_roadway: false,
        traffic_calming: true, count_calming: 2,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 1, count_intersection_ctrl: 3, total_crashes: 3, ksi_rate: 0.01 }),

    seg(42, "Christian St", "Local Residential",
      [-75.1720, -75.1500], [39.9435, 39.9435],
      { lanes: 1, width: 22, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 2, count_intersection_ctrl: 3, total_crashes: 4, ksi_rate: 0.02 }),

    seg(43, "Ellsworth St", "Local Residential",
      [-75.1720, -75.1520], [39.9408, 39.9408],
      { lanes: 1, width: 22, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 1, count_intersection_ctrl: 3, total_crashes: 3, ksi_rate: 0.01 }),

    seg(44, "Tasker St", "Local Residential",
      [-75.1720, -75.1520], [39.9393, 39.9393],
      { lanes: 1, width: 22, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 1, count_intersection_ctrl: 2, total_crashes: 3, ksi_rate: 0.01 }),

    seg(45, "Morris St", "Local Residential",
      [-75.1700, -75.1520], [39.9375, 39.9375],
      { lanes: 1, width: 22, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 1, count_intersection_ctrl: 2, total_crashes: 2, ksi_rate: 0.01 }),

    seg(46, "Mifflin St", "Local Residential",
      [-75.1700, -75.1500], [39.9348, 39.9348],
      { lanes: 1, width: 22, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 1, count_intersection_ctrl: 2, total_crashes: 2, ksi_rate: 0.01 }),

    seg(47, "20th St (S)", "Local Residential",
      [-75.1760, -75.1760], [39.9430, 39.9650],
      { lanes: 1, width: 24, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 2, count_intersection_ctrl: 4, total_crashes: 5, ksi_rate: 0.02 }),

    seg(48, "10th St", "Local Residential",
      [-75.1598, -75.1598], [39.9420, 39.9700],
      { lanes: 1, width: 26, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 2, count_intersection_ctrl: 4, total_crashes: 4, ksi_rate: 0.02 }),

    seg(49, "Wharton St", "Local Residential",
      [-75.1700, -75.1520], [39.9360, 39.9360],
      { lanes: 1, width: 22, speed_limit: 25, divided_roadway: false,
        traffic_calming: true, count_calming: 1,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 1, count_intersection_ctrl: 2, total_crashes: 2, ksi_rate: 0.01 }),

    seg(50, "Snyder Ave", "Local Residential",
      [-75.1750, -75.1480], [39.9335, 39.9335],
      { lanes: 2, width: 34, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "Sharrow", parking: "Both sides",
        count_transit: 3, count_intersection_ctrl: 3, total_crashes: 6, ksi_rate: 0.02 }),

    seg(51, "Academy Rd", "Collector Residential",
      [-75.0500, -75.0200], [40.0700, 40.0700],
      { lanes: 2, width: 38, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 2, count_intersection_ctrl: 3, total_crashes: 7, ksi_rate: 0.03 }),

    seg(52, "City Ave", "Major Arterial",
      [-75.2400, -75.2100], [40.0000, 40.0000],
      { lanes: 4, width: 75, speed_limit: 45, divided_roadway: true,
        bike_lane_type_simple: "None", parking: "None",
        count_transit: 5, count_intersection_ctrl: 4, total_crashes: 55, ksi_rate: 0.13 }),

    seg(53, "Castor Ave", "Minor Arterial",
      [-75.0850, -75.0600], [40.0380, 40.0680],
      { lanes: 2, width: 44, speed_limit: 35, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 3, count_intersection_ctrl: 3, total_crashes: 14, ksi_rate: 0.05 }),

    seg(54, "Moyamensing Ave", "Minor Arterial",
      [-75.1650, -75.1500], [39.9250, 39.9380],
      { lanes: 2, width: 40, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 3, count_intersection_ctrl: 3, total_crashes: 10, ksi_rate: 0.04 }),

    seg(55, "Harbison Ave", "Collector Residential",
      [-75.0680, -75.0500], [40.0550, 40.0550],
      { lanes: 2, width: 35, speed_limit: 25, divided_roadway: false,
        bike_lane_type_simple: "None", parking: "Both sides",
        count_transit: 2, count_intersection_ctrl: 2, total_crashes: 5, ksi_rate: 0.02 }),
  ];

  return {
    type: "FeatureCollection",
    features: features,
  };
}());
