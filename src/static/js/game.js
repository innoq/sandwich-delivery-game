'use strict';

//
// game - legacy code, used before migrating to elm, never cleaned up properly
//

const control = require('./control.js');
const map = require('./map.js');

module.exports.createGame = function (config) {
  var game = {};

  // create map
  game.map = map.createMap(config);

  // init controls + add reset function
  control.initControls(config.control, config);
  game.resetControls = function() {
    control.resetOrientationControls();
  };

  return game;
}
