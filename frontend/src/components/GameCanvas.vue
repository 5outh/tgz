<template>
  <div>
    <h1>{{ game.name }}</h1>
    <canvas id="game-canvas" />
    <ul>
      <li v-for="error in errors" :key="error">{{ error }}</li>
    </ul>
  </div>
</template>

<script lang="ts">
import { getAuthToken } from "@/auth";
import { Component, Vue, Prop } from "vue-property-decorator";
import { authorizedFetch } from "@/api";
import type { GameView } from "@/types"
import { GameViewV } from "@/types";
import { pipe } from 'fp-ts/lib/pipeable'
import { fold } from 'fp-ts/lib/Either'
import { PathReporter } from 'io-ts/lib/PathReporter'
import * as t from 'io-ts'

@Component
export default class GameCanvas extends Vue {
  @Prop({required: true}) readonly game!: GameView
  private token: string | null = null;
  private usernameText: string = "";
  private errors: Array<string> = [];

  mounted() {
    const token = getAuthToken();
    if (token === null || token === undefined) {
      return;
    }
    this.token = token;
    renderMapLayout(this.getGameCanvasContext(), this.game.state.map_layout)
  }

  private getGameCanvasContext(): CanvasRenderingContext2D {
    const canvas : any = document.getElementById("game-canvas");
    if (!canvas) {
      this.errors.push("Canvas is missing")
      throw new Error("Canvas is missing");
    }
    let ctx = canvas.getContext("2d");

    if (!ctx) {
      this.errors.push("Canvas context is missing");
      throw new Error("Canvas context is missing");
    }

    ctx.textAlign = "center";
    return ctx;
  }
}

function locationText(location:any) {
  const { x, y } = location;
  return y + String(x);
}

function locationToCoordinates(size:any, location:any) {
  const alphabet = "abcdefghijklmnopqrstuvwxyz";
  const { x, y } = location;
  const yIndex = alphabet.indexOf(y);

  return [(x - 1) * size + 1, yIndex * size + 1];
}

class Rect {
  private x : number;
  private y : number;
  private w : number;
  private h : number;

  constructor(x: number, y: number, w: number, h: number) {
    this.x = x
    this.y = y
    this.w = w
    this.h = h
  }

  public fill(ctx: CanvasRenderingContext2D): void {
    ctx.fillRect(this.x, this.y, this.w, this.h);
  }

  public stroke(ctx: CanvasRenderingContext2D): void {
    ctx.strokeRect(this.x, this.y, this.w, this.h);
  }

  public padded(padding: number) : Rect {
    return new Rect(this.x+padding, this.y+padding, this.w-2*padding, this.h-2*padding);
  }
}

export function renderMapLayout(ctx: any, mapLayout: any) {
  // either 12 or 18 squares
  let numSquares = 18;
  if (mapLayout.length == 144) {
    numSquares = 12;
  }
  const size = 720 / numSquares;
  const textSize = size / 2.5;
  const smallTextSize = size / 3.5;

  ctx.textAlign = "center"
  ctx.canvas.width = size * numSquares + 2;
  ctx.canvas.height = size * numSquares + 2;

  if (ctx) {
    for (let i = 0; i < mapLayout.length; i++) {
      const [location, square] = mapLayout[i];
      const [x, y] = locationToCoordinates(size, location);
      const rect = new Rect(x,y,size,size);

      ctx.fillStyle = "Linen";
      if (square.Land) {
        if (square.Land.StartingArea) {
          ctx.fillRect(x, y, size, size);
          rect.fill(ctx);
          ctx.fillStyle = "Brown"
          rect.padded(size/10).fill(ctx)
        }

        if (square.Land.BlankLand) {
          rect.fill(ctx)
        }

        if (square.Land.Resource) {
          switch (square.Land.Resource) {
            case "Clay":
              ctx.fillStyle = "Orange"
              rect.fill(ctx)
              break;
            case "Diamonds":
              ctx.fillStyle = "LightBlue"
              rect.fill(ctx)
              break;
            case "Ivory":
              ctx.fillStyle = "White"
              rect.fill(ctx)
              break;
            case "Wood":
              ctx.fillStyle = "Purple"
              rect.fill(ctx)
              break;
          }
        }
      }

      if (square.Water) {
        ctx.strokeStyle = "MidnightBlue";
        ctx.fillStyle = "MidnightBlue";
        rect.fill(ctx)
      }
    }

    // Stroke all squares except water
    for (let i = 0; i < mapLayout.length; i++) {
      const [location, square] = mapLayout[i];
      const [x, y] = locationToCoordinates(size, location);
      const rect = new Rect(x,y,size,size)

      if (!square.Water) {
        ctx.strokeStyle = "Black";
        ctx.lineWidth = 1;
        rect.stroke(ctx)
      }
    }
  }

  return "";
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
h3 {
  margin: 40px 0 0;
}
a {
  color: #42b983;
}
li {
  font-size: 14px;
}
</style>
