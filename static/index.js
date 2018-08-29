"use strict";

var json = JSON.parse(
  document.getElementById('config').textContent
);

Elm.Main.init(
  { node: document.getElementById('main')
  , flags: { config: json }
  }
);
