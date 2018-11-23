import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import { renderMapLayout, initializeGameCanvas } from './game';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

registerServiceWorker();

app.ports.renderMapLayout.subscribe(function(mapLayout) {
  renderMapLayout(mapLayout);
});
