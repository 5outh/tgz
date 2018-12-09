import {Rect} from './canvas';

var canvas;
var ctx;

export function getGameCanvasContext() {
  canvas = document.getElementById('game-canvas');
  ctx = canvas.getContext('2d');
  ctx.textAlign="center"

  return ctx;
}

function locationToCoordinates(size, location) {
  const alphabet= "abcdefghijklmnopqrstuvwxyz";
  const {x, y} = location;
  const yIndex = alphabet.indexOf(y);

  return [(x-1) * size, yIndex * size];
}

function locationText(location) {
  const {x, y} = location;
  return y + String(x);
}

export function renderMapLayout(mapLayout) {
  var ctx = getGameCanvasContext()

  // either 12 or 18 squares
  let numSquares = 18
  if (mapLayout.length == 144) {
    numSquares = 12;
  }
  const size = 720 / numSquares
  const textSize = size / 2.5
  const smallTextSize = size / 3.5

  if (ctx) {
    for (let i = 0; i < mapLayout.length; i++) {
      const [location, square] = mapLayout[i];
      const [x, y] = locationToCoordinates(size, location);
      const padding = (size - textSize) / 2
      const smallPadding = (size - smallTextSize) / 2
      const [textX, textY] = [x + (2*padding), y + 2*padding]
      const [locTextX, locTextY] = [x + (1.5*smallPadding), y + 2.5*smallPadding]

      ctx.fillStyle = 'Linen';
      if (square.Land) {
        if (square.Land.StartingArea) {
          ctx.strokeStyle="Tan";
          ctx.fillRect(x, y, size, size)
          ctx.font = textSize + 'px monospace';
          ctx.fillText('\uD83C\uDFDA\uFE0F', textX, textY)
        }

        if (square.Land.BlankLand) {
          ctx.strokeStyle="Tan";
          ctx.font = textSize + 'px monospace';
          ctx.fillRect(x, y, size, size)
        }

        if (square.Land.Resource) {
          ctx.strokeStyle="Tan";
          switch(square.Land.Resource) {
            case "Clay":
              ctx.fillRect(x, y, size, size)
              ctx.font = textSize + 'px monospace';
              ctx.fillText('\u26F0\uFE0F', textX, textY)
              break;
            case "Diamonds":
              ctx.fillRect(x, y, size, size)
              ctx.font = textSize + 'px monospace';
              ctx.fillText('\uD83D\uDC8E', textX, textY)
              break;
            case "Ivory":
              ctx.fillRect(x, y, size, size)
              ctx.font = textSize + 'px monospace';
              ctx.fillText('\uD83D\uDC18', textX, textY)
              break;
            case "Wood":
              ctx.fillRect(x, y, size, size)
              ctx.font = textSize + 'px monospace';
              ctx.fillText('\uD83C\uDF33', textX, textY)
              break;
          }
        }

        ctx.fillStyle = 'Black';
        ctx.font = smallTextSize + 'px monospace';
        ctx.fillText(locationText(location), locTextX, locTextY)
      }

      if (square.Water) {
        ctx.strokeStyle="MidnightBlue";
        ctx.fillStyle = 'MidnightBlue';
        ctx.fillRect(x, y, size, size);
      }
    }

    for (let i = 0; i < mapLayout.length; i++) {
      const [location, square] = mapLayout[i];
      const [x, y] = locationToCoordinates(size, location);

      if (!square.Water) {
        ctx.strokeStyle="Tan";
        ctx.lineWidth=4;
        ctx.strokeRect(x, y, size, size)
      }
    }
  }

  // Return an empty string for Elm runtime.
  return "";
}

// overlay player monuments on the grid
export function overlayPlayerMonuments(monumentsAndEmpire) {
  const monuments = monumentsAndEmpire.monuments
  const empire = monumentsAndEmpire.empire

  var ctx = getGameCanvasContext()

  // either 12 or 18 squares
  let numSquares = 18
  if (monuments.length == 144) {
    numSquares = 12;
  }

  const size = 720 / numSquares
  const textSize = size / 2.5
  const smallTextSize = size / 3.5

  if (ctx) {
    for (let i = 0; i < monuments.length; i++) {
      const smallPadding = (size - smallTextSize) / 2
      const [locTextX, locTextY] = [x + smallPadding, y + 2.5*smallPadding]

      const [location, monumentCount] = monuments[i];
      const [x, y] = locationToCoordinates(size, location);
      const centerX = x + size / 2
      const centerY = y + size / 2
      const radius = size / 2.25
      const [background, textColor] = empireColors(empire);

      // TODO: This is pretty silly, we should abstract it
      ctx.beginPath();
      ctx.arc(centerX, centerY, radius, 0, 2 * Math.PI, false);
      ctx.fillStyle = background;
      ctx.fill();
      ctx.lineWidth = 3;
      ctx.strokeStyle = '#003300';
      ctx.stroke();

      ctx.fillStyle = textColor;
      ctx.font = 'bold ' + textSize + 'px monospace';
      ctx.textAlign="center"
      // todo not sure why we need y offset
      ctx.fillText(String(monumentCount), centerX, centerY);

      ctx.fillStyle = textColor;
      ctx.font = 'bold ' + (textSize/1.5) + 'px monospace';
      ctx.textAlign="center"
      ctx.fillText(locationText(location), centerX, centerY + size/4);
    }
  }

  // Return an empty string for Elm runtime.
  return "";
}

// TODO: need size
export function overlayPlayerCraftsmen(playerCraftsmen) {
  console.log({playerCraftsmen});

  const craftsmen = playerCraftsmen.craftsmen;
  const empire = playerCraftsmen.empire;

  // either 12 or 18 squares
  // TODO: need size from somewhere
  let numSquares = 18
  //if (craftsmen.length == 144) {
    //numSquares = 12;
  //}

  const size = 720 / numSquares

  for (var i = 0; i < craftsmen.length; i++) {
    overlayPlayerCraftsman(size, empire, craftsmen[i]);
  }
}

export function overlayPlayerCraftsman(size, empire, playerCraftsman) {
  var ctx = getGameCanvasContext()
  // TODO: Fix this

  const textSize = size / 2.5
  const smallTextSize = size / 3.5

  const craftsman = playerCraftsman.craftsman;
  const [x, y] = locationToCoordinates(size, playerCraftsman.location);
  const {width, height} = playerCraftsman.dimensions;
  const [primary, secondary] = craftsmanColors(craftsman);
  const rect = new Rect(x,y,width*size,height*size);

  rect.drawRounded(ctx,size/6)
  ctx.fillStyle=secondary;
  ctx.fill();

  rect.drawRounded(ctx,size/6)
  ctx.strokeStyle=primary;
  ctx.lineWidth=4;
  ctx.stroke();
}

export function overlayUsedMarkers(usedMarkers) {
  var ctx = getGameCanvasContext()

  // either 12 or 18 squares
  // TODO: need size from somewhere
  let numSquares = 18
  //if (craftsmen.length == 144) {
    //numSquares = 12;
  //}

  const size = 720 / numSquares

  for (let i = 0; i < usedMarkers.length; i++) {
      let times = usedMarkers[i].times
      let [x,y] = locationToCoordinates(size, usedMarkers[i].location)
      let rect = new Rect(x,y,size,size)
      rect.drawX(ctx);
      ctx.strokeStyle = 'black';
      ctx.stroke();
  }
}

function empireColors(empire) {
  switch(empire){
    case 'Kilwa':
      return ['red', 'white'];
    case 'Mutapa':
      return ['yellow', 'black'];
    case 'Zulu':
      return ['green', 'white'];
    case 'Mapungubwe':
      return ['white', 'black'];
    case 'Lozi':
      return ['black', 'white'];
    default:
      return ['black', 'white'];
  }
}

function craftsmanColors(craftsman) {
  // TODO: Fix this
  switch(craftsman) {
    case "IvoryCarver": return ["black", "ivory"]
    case "Potter": return ["black", "Orange"]
    case "WoodCarver": return ["black", "Purple"]
    case "DiamondCutter": return ["black", "LightBlue"]

    case "VesselMaker": return ["gold", "Orange"]
    case "ThroneMaker": return ["gold", "Purple"]
    case "Sculptor": return ["gold", "ivory"]
    default: return ["white, black"]
  }
}

