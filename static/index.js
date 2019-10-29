"use strict";

var json = JSON.parse(
  document.getElementById('config').textContent
);

Elm.Pages.Board.init(
  { node: document.getElementById('main')
  , flags: { config: json }
  }
);
