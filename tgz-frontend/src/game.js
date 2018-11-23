var canvas;
var ctx;

export function getGameCanvasContext() {
  if (!canvas) {
    canvas = document.getElementById('game-canvas');
    if (canvas) {
      ctx = canvas.getContext('2d');
    }
  }

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
  console.log(y+String(x));
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
      const [textX, textY] = [x + padding, y + 2*padding]
      const [locTextX, locTextY] = [x + smallPadding, y + 2.5*smallPadding]

      if (square.Land) {
        ctx.fillStyle = 'Linen';
        if (square.Land.StartingArea) {
          // set this based off of square type
          ctx.strokeStyle="Tan";
          //ctx.fillStyle = 'SlateGray';
          ctx.fillRect(x, y, size, size)
          ctx.font = textSize + 'px monospace';
          ctx.fillText('\uD83C\uDFDA\uFE0F', textX, textY)
        }

        if (square.Land.BlankLand) {
          // set this based off of square type
          ctx.strokeStyle="Tan";
          //ctx.fillStyle = 'Linen';
          ctx.font = textSize + 'px monospace';
          ctx.fillRect(x, y, size, size)
        }

        if (square.Land.Resource) {
          ctx.strokeStyle="Tan";
          switch(square.Land.Resource) {
            case "Clay":
              ctx.fillStyle = 'Tomato';
              //ctx.fillRect(x, y, size, size)
              ctx.font = textSize + 'px monospace';
              ctx.fillText('\u26F0\uFE0F', textX, textY)
              break;
            case "Diamonds":
              ctx.fillStyle = 'SkyBlue';
              //ctx.fillRect(x, y, size, size)
              ctx.font = textSize + 'px monospace';
              ctx.fillText('\uD83D\uDC8E', textX, textY)
              break;
            case "Ivory":
              ctx.fillStyle = 'GreenYellow';
              //ctx.fillRect(x, y, size, size)
              ctx.font = textSize + 'px monospace';
              ctx.fillText('\uD83D\uDC18', textX, textY)
              break;
            case "Wood":
              ctx.fillStyle = 'MediumOrchid';
              //ctx.fillRect(x, y, size, size)
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
        // set this based off of square type
        ctx.strokeStyle="MidnightBlue";
        ctx.fillStyle = 'MidnightBlue';
        ctx.fillRect(x, y, size, size);
        //ctx.strokeRect(x, y, size, size);
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
}
