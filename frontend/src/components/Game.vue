<template>
  <div>
    <h1>Game</h1>
    <game-canvas v-if="game" v-bind:game="game" />
    <ul>
      <li v-for="error in errors" :key="error">{{ error }}</li>
    </ul>
  </div>
</template>

<script lang="ts">
import { isAuthenticated } from "@/auth";
import { Component, Vue } from "vue-property-decorator";
import { authorizedFetch, fetchMe, fetchGame } from "@/api";
import type { GameView } from "@/types"
import { GameViewV } from "@/types";
import { pipe } from 'fp-ts/lib/pipeable'
import { fold, bimap } from 'fp-ts/lib/Either'
import { PathReporter } from 'io-ts/lib/PathReporter'
import GameCanvas from "@/components/GameCanvas.vue"
import * as t from 'io-ts'

@Component({
  name: 'Game',
  components: {
    'game-canvas': GameCanvas
  }
})
export default class Home extends Vue {
  private game: GameView | null = null;
  private errors: Array<string> = [];

  mounted() {
    this.setup()
  }

  private reportErrors(a: any) {
    this.errors = this.errors.concat(PathReporter.report(a))
  }

  private async setup() {
    if (isAuthenticated()) {
      const game = await fetchGame(parseInt(this.$route.params.id));
      bimap(() => this.reportErrors(game), (a: GameView) => { this.game = a })(game)
    }
  }

  private fetchGame(gameId: number): Promise<GameView | void> {
    const onLeft = (errors : t.Errors) => {}
    const onRight = (game : GameView) => game
    const self = this

    return authorizedFetch(`http://localhost:8000/games/${gameId}`).then(
      (response: Response) => {
        if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        return response.json()
      }).then((data: object) => {
        const result = GameViewV.decode(data)

        self.errors = PathReporter.report(result);
        return pipe(result, fold(onLeft, onRight));
      });
  }
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
