'use strict';

require('./index.html');

var Elm = require('./Main.elm').Elm;

var app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: {
    currentTime: Date.now()
  }
});

(function() {
  let uniqueId = 1;

  app.ports.pushChoreAttempt.subscribe(function(data) {
    const dataWithId = Object.assign({}, data, { id: String(uniqueId) });

    uniqueId = uniqueId + 1;

    app.ports.choreAttemptAdded.send(dataWithId);
  });
})();

