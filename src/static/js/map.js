'use strict';

//
// map - some legacy code, used before migrating to elm ; a better separation would be nice
//

const rbush = require('../../../node_modules/rbush/rbush.js');
const graphlib = require('../../../node_modules/graphlib/dist/graphlib.js');
const geofunctions = require('./geofunctions.js');

const roadsGeojsonFile = 'static/data/roads_filtered.geojson';


module.exports.createMap = function (config) {
  var mymap = L.map('mapid', {
    rotate: true
  }).setView([config.playerStartLocation.lat, config.playerStartLocation.lng], config.defaultZoomLevel);

  L.control.attribution({
    position: 'bottomleft'
  }).addTo(mymap);

  // serving tiles
  if (config.useLocalTileServer) {
    var tileLayer = L.tileLayer('http://' + config.localTileServerUrl + '/styles/smart-city/rendered/{z}/{x}/{y}.png', {
      maxZoom: 17
    });
    tileLayer.addTo(mymap);
  } else {
    var tileLayer = L.tileLayer('https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_all/{z}/{x}/{y}.png', {
      maxZoom: config.maxZoomLevel,
      minZoom: config.minZoomLevel,
      attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>, &copy; <a href="https://carto.com/attribution">CARTO</a>'
    });

    tileLayer.addTo(mymap);
  }

  var roads = addRoads(mymap, config);

  return { map: mymap, roads: roads };
}


function coordinatesKey(lat, lng) {
  return lat.toString() + '#' + lng.toString();
}


function keyToCoordinates(key) {
  return key.split("#").map(function(e) {return Number(e);});
}

module.exports.keyToCoordinates = keyToCoordinates;

function loadRoads(jsonFile) {
  return new L.GeoJSON.AJAX(roadsGeojsonFile, {
    style: function (feature) {
      var color = 'white';
      return {
        color: color,
        weight: 15,
        opacity: 0.3
      };
    }
  });
}


function addRoads(mymap, config) {
  var roads = loadRoads(roadsGeojsonFile);

  var graph = new graphlib.Graph({directed: true});
  var searchtree = rbush();

  roads.refilter(function (feature) {

    var isGreenWave = feature.properties.name !== undefined && config.greenWaveStreets.indexOf(feature.properties.name) != -1;

    feature.geometry.coordinates.reduce(function (last, current) {
      if (last !== null) {
        var k1 = coordinatesKey(last[1], last[0]);
        var k2 = coordinatesKey(current[1], current[0]);
        var label = isGreenWave ? 1 : geofunctions.distanceTo(last[1], last[0], current[1], current[0]);

        let rectLB = config.areaRectangleBottomLeft;
        let rectTR = config.areaRectangeTopRight;

        // only if in the smaller area around the green wave
        if (geofunctions.inRectangle([last[1],last[0]], rectLB, rectTR) && geofunctions.inRectangle([current[1],current[0]], rectLB, rectTR)) {
          graph.setEdge(k1, k2, ""+label);
          graph.setEdge(k2, k1, ""+label);
        }

        searchtree.insert({
          minX: last[0],
          minY: last[1],
          maxX: current[0],
          maxY: current[1],
          coordinates: feature.geometry.coordinates
        });
      }
      return current;
    }, null);

    return true;
  });

  roads.getDistance = function (lat, lng) {
    const diff = 0.0015; // max area to search for
    var results = searchtree.search({
      minX: lng - diff,
      minY: lat - diff,
      maxX: lng + diff,
      maxY: lat + diff
    });

    var distances = results.map(function (r) {
      return distancePointToLineSeg(lng, lat, r.minX, r.minY, r.maxX, r.maxY);
    });

    var distance = distances.length > 0 ? Math.min.apply(null, distances) : diff;
    return distance;
  };

  roads.createRoute = function() {
    return createRoute(graph);
  }

  roads.addTo(mymap);

  return roads;
}

function distancePointToLineSeg(x, y, x1, y1, x2, y2) {
  var A = x - x1;
  var B = y - y1;
  var C = x2 - x1;
  var D = y2 - y1;

  var dot = A * C + B * D;
  var len_sq = C * C + D * D;
  var param = -1;
  if (len_sq !== 0)
    param = dot / len_sq;

  var xx, yy;

  if (param < 0) {
    xx = x1;
    yy = y1;
  } else if (param > 1) {
    xx = x2;
    yy = y2;
  } else {
    xx = x1 + param * C;
    yy = y1 + param * D;
  }

  var dx = x - xx;
  var dy = y - yy;
  return Math.sqrt(dx * dx + dy * dy);
}

function createRoute(graph) {
  let from = graph.nodes()[Math.floor(Math.random() * graph.nodeCount())];

  let dijkstra = graphlib.alg.dijkstra(graph, from, function (e) {
    let ee = graph.edge(e.v, e.w);
    return Number(ee);
  });
  // remove Infinite nodes
  Object.keys(dijkstra).forEach(function (to) {
    if (dijkstra[to].distance === Infinity) {
      delete dijkstra[to];
    }
  });

  var destinations = Object.keys(dijkstra).filter(function (dest){ return dest!=from;});
  let to = destinations[Math.floor(Math.random() * destinations.length)];

  if (to !== undefined) {
    return getRoutePoints(dijkstra, from, to);
  } else {
    return undefined;
  }
}

function getRoutePoints(dijkstra, from, to) {
  let points = [];
  let tmpTo = to;
  do {
    points.push(keyToCoordinates(tmpTo));
    tmpTo = dijkstra[tmpTo].predecessor;
  } while (tmpTo !== undefined);

  points.reverse();

  if (points.length == 1) console.error(JSON.stringify(dijkstra));
  return points;
}

