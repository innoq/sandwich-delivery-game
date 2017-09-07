'use strict';

//
// draws elements on map
//

const geofunctions = require('./geofunctions.js');
const polygonBoolean = require('2d-polygon-boolean');

let styles =
  { trafficLight:
      { redStreet: 'red'
      , greenStreet: 'green'
      , yellowStreet: 'orange'
      , redLight: 'red'
      , greenLight: 'green'
      , yellowLight: 'orange'
      }

  , player:
      { default: '#00c8e6'
      , offRoad: 'red'
      , groupOrderDirection: 'black'
      , groupOrderDirectionSelected: 'green'
      }

  , groupOrder:
    { color: 'lightblue'
    , opacity: 0.3
    , fillColor: 'lightblue'
    , fillOpacity: 0.5

    , activeColor: '#3090C7'
    , activeOpacity: 0.3
    , activeFillColor: '#3090C7'
    , activeFillOpacity: 0.5

    , selectedColor: '#3090C7'
    , selectedOpacity: 0.8
    , selectedFillColor: 'lightblue'
    , selectedFillOpacity: 0.5

    , highlightedColor: '#3090C7'
    , highlightedOpacity: 0.8
    , highlightedFillColor: 'lightblue'
    , highlightedFillOpacity: 0.5
    }

  , biker:
      { radius: 6
      , style:
        { color: 'black'
        , opacity: 0.6
        , fillColor: '#00c8e6'
        , fillOpacity: 0.5
        }
      }
  }


var bikerViews = {};

function draw(config, map, type, data) {
  let views = [];
  if (type === 'trafficLight') {
    views = views.concat(drawTrafficLight(data));

  } else if (type === 'groupOrder') {
    views = views.concat(drawGroupOrder(data));

  } else if (type === 'player') {
    views = views.concat(drawPlayer(data));

    let bearing = -data.bearing;

    let [lat, lng] = geofunctions.computeNewPoint(data.location.lat, data.location.lng, config.playerViewOffset, -bearing);

    map.setView([lat, lng], map.getZoom(), { animate: false });

    if (Math.abs(map.getBearing() - bearing) >= 1) {
      map.setBearing(bearing);
    }

  } else if (type === 'playerGroupOrderDirections') {
    views = views.concat(drawPlayerGroupOrderDirections(data[0], data[1]));
  }

  for (let i in views) {
    let v = views[i];
    v.addTo(map);
  }

  return views;
}

function drawTrafficLight(trafficLight) {
  let views = [];

  let intersectionEdges = trafficLight.intersection.edges.map(function (e) {
    return [e.lat, e.lng];
  });

  for (let i in trafficLight.trafficFlows) {
    let f = trafficLight.trafficFlows[i];
    views = views.concat(drawTrafficFlow(f, intersectionEdges));
  }

  return views;
}

function drawTrafficFlow(trafficFlow, intersectionEdges) {
  let views = [];

  for (let i in trafficFlow.streets) {
    let s = trafficFlow.streets[i];
    views = views.concat(drawStreet(s, trafficFlow.lightStates, intersectionEdges));
  }
  for (let i in trafficFlow.lights) {
    let l = trafficFlow.lights[i];
    views = views.concat(drawLight(l, trafficFlow.lightStates));
  }

  return views;
}

function drawStreet(street, lightStates, intersectionEdges) {
  let views = [];

  let latLngs = street.waitingArea.edges.map(function (e) {
    return [e.lat, e.lng];
  });

  var intersectionArea = polygonBoolean(latLngs, intersectionEdges, 'or');

  let color = undefined;
  if (lightStates.red) {
    color = styles.trafficLight.redStreet;
  } else if (lightStates.green) {
    color = styles.trafficLight.greenStreet;
  } else if (lightStates.yellow) {
    color = styles.trafficLight.yellowStreet;
  }

  if (color !== undefined) {
    if (intersectionArea.length > 0) {
      let view = L.polygon(intersectionArea[0], {
        color: color,
        opacity: 1,
        fillColor: color,
        fillOpacity: 0.3,
        weight: 2
      });

      views.push(view);
    } else {
      console.error("No intersection area found for street and intersection");
    }
  }

  return views;
}


function drawLight(light, lights) {
  let views = [];

  let color = undefined;
  if (lights.red) {
    color = styles.trafficLight.redLight;
  } else if (lights.green) {
    color = styles.trafficLight.greenLight;
  } else if (lights.yellow) {
    color = styles.trafficLight.yellowLight;
  }

  if (color !== undefined) {
    var circle = L.circle([light.lat, light.lng], 12, {
      color: 'black',
      weight: 3,
      fillColor: color,
      fillOpacity: 0.5
    });

    views.push(circle);
  }

  return views;
}

function drawGroupOrder(groupOrder) {
  let views = [];

  let lat = groupOrder.location.lat;
  let lng = groupOrder.location.lng;
  let radius = groupOrder.radius;
  let count = groupOrder.count;
  let isActive = groupOrder.activeSince !== null;
  let isSelected = groupOrder.selected;
  let isHighlighted = groupOrder.highlighted;

  let color = styles.groupOrder.color;
  let opacity = styles.groupOrder.opacity;
  let fillColor = styles.groupOrder.fillColor;
  let fillOpacity = styles.groupOrder.fillOpacity;
  if (isActive) {
    color = styles.groupOrder.activeColor;
    opacity = styles.groupOrder.activeOpacity;
    fillColor = styles.groupOrder.activeFillColor;
    fillOpacity = styles.groupOrder.activeFillOpacity;
  } else if (isSelected) {
    color = styles.groupOrder.selectedColor;
    opacity = styles.groupOrder.selectedOpacity;
    fillColor = styles.groupOrder.selectedFillColor;
    fillOpacity = styles.groupOrder.selectedFillOpacity;
  } else if (isHighlighted) {
    color = styles.groupOrder.highlightedColor;
    opacity = styles.groupOrder.highlightedOpacity;
    fillColor = styles.groupOrder.highlightedFillColor;
    fillOpacity = styles.groupOrder.highlightedFillOpacity;
  }

  var circle = L.circle([lat, lng], radius, {
    color: color,
    opacity: opacity,
    fillColor: fillColor,
    fillOpacity: fillOpacity
  });

  let text = L.divIcon({className: 'group-order-text', html: count.toString()});
  let marker = L.marker([lat, lng], {icon: text});

  views.push(circle);
  views.push(marker);

  return views;
}

function drawPlayer(player) {
  let views = [];

  const arrowStretch = 20;
  const arrowSize = 0.03; // in km

  let points = [
    geofunctions.computeNewPoint(player.location.lat, player.location.lng, arrowSize, player.bearing - 180 - arrowStretch),
    [player.location.lat, player.location.lng],
    geofunctions.computeNewPoint(player.location.lat, player.location.lng, arrowSize, player.bearing - 180 + arrowStretch)
  ];

  let arrows = L.polyline(points,
    { color: player.outOfStreet ? styles.player.offRoad : styles.player.default
    , weight: 6
    }
  );

  views.push(arrows);

  return views;
}

function drawPlayerGroupOrderDirections(player, groupOrders) {
  let views = [];

  for (let i in groupOrders) {
    var o = groupOrders[i];

    if (o.selected) {
      var lat1 = player.location.lat;
      var lng1 = player.location.lng;
      var lat2 = o.location.lat;
      var lng2 = o.location.lng;

      var d = geofunctions.distanceTo(lat1, lng1, lat2, lng2);
      const lineSize = 10;

      [lat1, lng1] = geofunctions.computePointOnLine(lat1, lng1, lat2, lng2, player.radius - 1);
      [lat2, lng2] = geofunctions.computePointOnLine(lat1, lng1, lat2, lng2, lineSize);

      var reach = 1000;

      var colorIntensity = o.selected ? 100 : 100 - Math.floor(d * 100 / reach);
      if (colorIntensity < 0) colorIntensity = 0;

      var color = o.selected ? player.groupOrderDirectionSelected : player.groupOrderDirection;

      var opacity = 1;
      if (opacity > 1) opacity = 1;

      var latLngs = [[lat1, lng1], [lat2, lng2]];
      var polyline = L.polyline(latLngs,
        { color: color
        , weight: 5
        , opacity: opacity
        }
      );

      polyline.bringToFront();
      views.push(polyline);
    }

  }

  return views;
}


var viewables = {};

function update(config, map, id, type, data) {
  let key = type+'#'+id;
  let viewable = viewables[key];

  if (!viewable) {
    if (data !== null) {
      let views = draw(config, map, type, data);
      viewables[key] =
        { type: type
        , views: views
        };
    }
  } else {
    for (var i in viewable.views) {
      viewable.views[i].remove();
    }

    if (data !== null) {
      let views = draw(config, map, type, data);
      viewables[key].views = views;
    } else {
      delete viewables[key];
    }
  }
}

module.exports.update = update;

module.exports.updateBikers = function(bikers) {
  var viewIds = [];
  for (let biker of bikers) {
    viewIds.push(biker.id);
    let latlng = [biker.location.lat, biker.location.lng];
    if (bikerViews[biker.id] === undefined) {
      bikerViews[biker.id] = L.circle(latlng, styles.biker.radius,
        styles.biker.style
      ).addTo(game.map.map);
    } else {
      bikerViews[biker.id].setLatLng(latlng);
    }
  }

  Object.keys(bikerViews)
    .filter(function (k){
      return viewIds.indexOf(Number(k)) == -1;
    })
    .forEach(function (k){
      bikerViews[k].remove();
      delete bikerViews[k];
    });
};
