//
// dependencies
//

require( './styles/leaflet.css' );
require( './styles/main.scss' );

require('./libs/leaflet-rotate/leaflet.js');
require('./libs/leaflet.ajax.min.js');

require('./js/map.js');

// required for native elm modules
geofunctions = require( './js/geofunctions.js' );


//
// configuration
//

function autoDetectControls() {
  if (navigator.userAgent.match(/(iPhone|iPod|iPad|Android|BlackBerry|IEMobile)/)) {
    return 'orientation';
  } else {
    return 'keyboard';
  }
}

config =
  { control: autoDetectControls()
  , interval: 100
  , minZoomLevel: 14
  , maxZoomLevel: 17
  , defaultZoomLevel: 15
  , rideZoomLevel: 17
  , focusedSidebarZoomLevel: 15
  , playerViewOffset: 0.10
  , playerRadius: 50
  , playerStartLocation:
    { lat: 47.56238668343279
    , lng: 7.6006488100539595
    }
  , playerStartBearing: -122
  , keyboardSpeedDiff: 10
  , keyboardStearingAngleDiff: 10
  , maxGroupOrders: 5
  , minGroupOrderCount: 1
  , maxGroupOrderCount: 9
  , roadsGeoJson: "static/data/roads_filtered.geojson"
  , greenWaveStreets: [
    , "Wettsteinplatz"
    , "Wettsteinbr\u00fccke"
    , "St. Alban-Graben"
    , "Freie Strasse"
    , "Marktplatz"
    , "Eisengasse"
    , "Mittlere Br\u00fccke"
    , "Greifengasse"
    , "Claraplatz"
    , "Claragraben"
    ]
  , areaRectangleBottomLeft:
    [ 47.552288202750624
    , 7.585394382476807
    ]
  , areaRectangeTopRight:
    [ 47.56301837683779
    , 7.600951194763183
    ]
  , trafficLightsGeoJson: "static/data/traffic_lights.geojson"
  , groupOrderLocationsGeoJson: "static/data/group_order_locations.geojson"
  , bikersCount: 50
  , scorePointsPerOrder: 10
  , scorePointsPerRedLight: -60
  , scorePointsPerOffRoad: -30
  , canRotateWithoutSpeed: false
  , outOfStreetWarningDistance: 0.10
  , trafficLightConfig:
    { maxFlowTime: 8000
    , minFlowTime: 2000
    , yellowRedChangeTime: 2000
    , defaultGreenPhaseTime: 6000
    }
  , timerStart: 60 * 2
  , timerNotifications:
    [ 60 * 4, 60 * 3, 60 * 2, 60, 30, 20, 10, 5, 4, 3, 2, 1]
  , tellWarnings: true
  , showControls: false
  , showBuildings: false
  , useLocalTileServer: false
  , localTileServerUrl: location.hostname + ':8180'
  , scoreServerUrl: location.hostname + ':8280'
  };


//
// connect Elm
//

var elm = require( '../elm/Main' );
app = elm.Main.embed(document.getElementById('main'), config);

var draw = require( './js/draw.js' );

let mapInitialized = false;
game = undefined;

app.ports.initializeMap.subscribe(function(a) {
  if (!mapInitialized) {
    g = require( './js/game.js' );
    game = g.createGame(config);
    mapInitialized = true;
    setTimeout(function() {
      // a stupid workaround to wait till the road graph is ready
      app.ports.onMapInitialized.send(null);
    }, 200);
  }
});

app.ports.initializeControls.subscribe(function(a) {
  if (game) {
    game.resetControls();
  }
});

app.ports.restart.subscribe(function(data) {
  location.reload();
});

app.ports.tell.subscribe(function(text) {
});

app.ports.updateZoomLevel.subscribe(function(zoomLevel) {
  game.map.map.setZoom(zoomLevel);
});

app.ports.updateTrafficLightView.subscribe(function(data) {
  draw.update(config, game.map.map, data[0], 'trafficLight', data[1]);
});

app.ports.updateGroupOrder.subscribe(function(data) {
  draw.update(config, game.map.map, data[0], 'groupOrder', data[1]);
});

app.ports.updatePlayer.subscribe(function(data) {
  draw.update(config, game.map.map, '1', 'player', data);

  // each player computes a new distance to roads
  let distance = game.map.roads.getDistance(data.location.lat, data.location.lng);
  distance = distance * 1000;
  app.ports.playerDistanceToRoads.send(distance);
});

app.ports.updatePlayerGroupOrderDirections.subscribe(function(data) {
  draw.update(config, game.map.map, '1', 'playerGroupOrderDirections', data);
});

app.ports.createBikerRoute.subscribe(function() {
  let route = game.map.roads.createRoute();
  let f = function(route) {
    if (route) {
      let r = route.map(function (latlng){
        return {lat: latlng[0], lng: latlng[1]};
      });
      let speed = 0.5 + Math.random();
      app.ports.bikerRouteCreated.send([r, speed]);
    } else {
      setTimeout(function() {
        // a stupid workaround to wait till the road graph is ready
        let route = game.map.roads.createRoute();
        f(route);
      }, 200);
    }
  }
  f(route);
});


app.ports.updateBikersView.subscribe(draw.updateBikers);
