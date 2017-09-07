'use strict';

//
// utility functions for geo - most of the code is based on or taken from the authors on stackoverflow etc.
//

Math.radians = function(degrees) {
  return degrees * Math.PI / 180;
};

Math.degrees = function(radians) {
  return radians * 180 / Math.PI;
};

const R = 6378.137;

module.exports.computeNewPoint = function (lat, lng, distanceKm, brng) {
  // note: this is spherical computation

  brng = Math.radians(brng);
  const d = distanceKm;

  let lat1 = Math.radians(lat);
  let lon1 = Math.radians(lng);

  let lat2 = Math.asin(Math.sin(lat1)*Math.cos(d/R) +
      Math.cos(lat1)*Math.sin(d/R)*Math.cos(brng));

  let lon2 = lon1 + Math.atan2(Math.sin(brng)*Math.sin(d/R)*Math.cos(lat1),
              Math.cos(d/R)-Math.sin(lat1)*Math.sin(lat2));

  lat2 = Math.degrees(lat2);
  lon2 = Math.degrees(lon2);

  return [lat2, lon2];
}

module.exports.computePointOnLine = function (lat1, lng1, lat2, lng2, distance) {
  // intercept theorem
  var d = distanceTo(lat1, lng1, lat2, lng2);

  var d1 = distance * 1.5;
  var a1 = d1 * (lat2 - lat1) / d;
  var b1 = d1 * (lng2 - lng1) / d;

  var lat = lat1 + a1;
  var lng = lng1 + b1;

  return [lat, lng];
}

function distanceTo(lat1, lon1, lat2, lon2) {
  // http://stackoverflow.com/questions/639695/how-to-convert-latitude-or-longitude-to-meters
  var dLat = lat2 * Math.PI / 180 - lat1 * Math.PI / 180;
  var dLon = lon2 * Math.PI / 180 - lon1 * Math.PI / 180;
  var a = Math.sin(dLat/2) * Math.sin(dLat/2) +
  Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) *
  Math.sin(dLon/2) * Math.sin(dLon/2);
  var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
  var d = R * c;
  return d * 1000;
}

module.exports.distanceTo = distanceTo;

module.exports.isInArea = function (lat, lng, areaLat, areaLng, areaRadius) {
  var dist = distanceTo(lat, lng, areaLat, areaLng);
  return dist <= areaRadius;
}

module.exports.inRectangle = function(point, rectLeftBottom, rectTopRight) {
  return point[0] >= rectLeftBottom[0] && point[0] <= rectTopRight[0] &&
         point[1] >= rectLeftBottom[1] && point[1] <= rectTopRight[1];
};
