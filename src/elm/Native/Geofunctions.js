
// https://hackernoon.com/creating-an-elm-native-module-for-currency-formatting-c9800e57a908

// required global vars:
// geofunctions

var _innoq$sandwich_game$Native_Geofunctions = (function () {
    var distanceTo = function(lat1, lng1, lat2, lng2) {
        try {
            var result = geofunctions.distanceTo(lat1, lng1, lat2, lng2);
            return result;
        } catch (e) {
            console.error(e);
            return NaN;
        }
    };
    var computeNewPoint = function(lat, lng, distanceKm, brng) {
        try {
            var result = geofunctions.computeNewPoint(lat, lng, distanceKm, brng);
            return { lat: result[0], lng: result[1] };
        } catch (e) {
            console.error(e);
            return NaN;
        }
    };
    var computePointOnLine = function(lat1, lng1, lat2, lng2, distance) {
        try {
            var result = geofunctions.computePointOnLine(lat1, lng1, lat2, lng2, distance);
            return { lat: result[0], lng: result[1] };
        } catch (e) {
            console.error(e);
            return NaN;
        }
    };
    // if less than 2 params: just use fkt
    return { distanceTo: F4(distanceTo)
           , computeNewPoint: F4(computeNewPoint)
           , computePointOnLine: F5(computePointOnLine)
           };
})();
