'use strict';

//
// handles the control input: keyboard or orientation API for mobile phones
//


// current player control info
let player = {
  speed: 0,
  stearingAngle: 0
};

// orientation api variables
var startSpeed = undefined;
var startAngle = undefined;
var touchStart = undefined;


module.exports.initControls = function (type, config) {
  if (type == 'keyboard') {
    initKeyboardControls(config.keyboardSpeedDiff, config.keyboardStearingAngleDiff);
  } else if (type === 'orientation') {
    initOrientationControls();
  } else {
    console.error(`Unknown control type ${type}`);
  }
};

module.exports.resetOrientationControls = function() {
  startSpeed = undefined;
  startAngle = undefined;
  touchStart = undefined;
};


function initKeyboardControls(speedDiff, stearingAngleDiff) {

  console.log("Configuring keyboard controls");

  function onKeypress(key) {
    var speed = player.speed;
    var stearingAngle = player.stearingAngle;

    if (key == 'w') {
      speed = speedDiff + speed * 1.5;
    }
    else if (key == 's') {
      speed = speed / 2 - speedDiff;
      if (speed < 0) {
        speed = 0;
      }
    }
    else if (key == 'a') {
      if (stearingAngle > 0) {
        stearingAngle = 0;
      } else {
        stearingAngle = stearingAngle - stearingAngleDiff;
      }
    }
    else if (key == 'd') {
      if (stearingAngle < 0) {
        stearingAngle = 0;
      } else {
        stearingAngle = stearingAngle + stearingAngleDiff;
      }
    } else if (key == 'h') {
      app.ports.controllerInput.send('left');
    } else if (key == 'l') {
      app.ports.controllerInput.send('right');
    } else if (key == 'k') {
      app.ports.controllerInput.send('up');
    } else if (key == 'j') {
      app.ports.controllerInput.send('down');
    } else if (key == ' ') {
      app.ports.controllerInput.send('select');
    }

    player.speed = speed;
    player.stearingAngle = stearingAngle;

    app.ports.playerMovements.send({
      speed: player.speed, stearingAngle: player.stearingAngle
    });
  }

  document.onkeypress = function(e) {
    onKeypress(String.fromCharCode(e.charCode));
  };
}


function initOrientationControls() {

  console.log("Configuring orientation API controls");

  window.addEventListener('touchstart', function (event) {
    console.log("touch started");
    touchStart = event.changedTouches[0];
  });

  window.addEventListener('touchend', function (event) {
    let touchEnd = event.changedTouches[0];
    let yDiff = Math.abs(touchStart.screenY - touchEnd.screenY);

    if (yDiff < 20) {
      let xDiff = touchStart.screenX - touchEnd.screenX;

      if (xDiff > 50) {
        app.ports.controllerInput.send('left');
      } else if (xDiff < -50) {
        app.ports.controllerInput.send('right');
      } else {
        app.ports.controllerInput.send('select');
      }
    }
  });

  window.addEventListener('deviceorientation', function (event) {
    let orientationAngle = event.alpha;
    let speedAngle = event.beta;

    if (startSpeed === undefined) {
      startSpeed = speedAngle;
    }
    if (startAngle === undefined) {
      startAngle = orientationAngle;
    }

    player.speed = (startSpeed - speedAngle) * 10;

    let angle = startAngle - orientationAngle;
    if (angle > 180) {
      angle = -360 + angle % 360;
    } else if (angle < -180) {
      angle = 360 + angle;
    }

    player.stearingAngle = angle;

    app.ports.playerMovements.send({
      speed: player.speed, stearingAngle: player.stearingAngle
    });
  });
}
