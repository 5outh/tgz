import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import {overlayPlayerMonuments, overlayPlayerCraftsmen, renderMapLayout, initializeGameCanvas, overlayUsedMarkers } from './game';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

registerServiceWorker();

app.ports.renderMapLayout.subscribe(function(mapLayout) {
  app.ports.mapLayoutRendered.send(renderMapLayout(mapLayout));
});

app.ports.overlayPlayerMonuments.subscribe(function(playerMonuments) {
  overlayPlayerMonuments(playerMonuments);
});

app.ports.overlayPlayerCraftsmen.subscribe(function(playerCraftsmen) {
  overlayPlayerCraftsmen(playerCraftsmen);
});

app.ports.overlayUsedMarkers.subscribe(function(usedMarkers) {
  overlayUsedMarkers(usedMarkers);
});
