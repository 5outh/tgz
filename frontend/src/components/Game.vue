<template>
  <div>
    <h1>Game</h1>
    {{ game }}
  </div>
</template>

<script lang="ts">
import { getAuthToken } from "@/auth";
import { Component, Vue } from "vue-property-decorator";
import { authorizedFetch } from "@/api";
import type { GameView } from "@/types"
import { GameViewV } from "@/types";
import { pipe } from 'fp-ts/lib/pipeable'
import { fold } from 'fp-ts/lib/Either'
import { PathReporter } from 'io-ts/lib/PathReporter'
import * as t from 'io-ts'

@Component
export default class Home extends Vue {
  private token: string | null = null;
  private usernameText: string = "";
  private game: GameView | null = null;

  mounted() {
    const self = this;
    const token = getAuthToken();

    if (token === null || token === undefined) {
      return;
    }
    this.token = token;

    this.fetchGame(parseInt(this.$route.params.id), token)
      .then((game: GameView | void) => {
        if (game) {
          self.game = game;
        } else {
          throw new Error("Game was not parseable")
        }
      })
      .catch(console.error);
  }

  private createNewGame() {
    const users = this.usernameText.split("\n");
    if (this.token) {
      this.newGame(users, this.token).then(console.log);
    }
  }

  private updateUsernames() {
    alert("usernames");
  }

  private newGame(usernames: Array<string>, token: string): Promise<GameView> {
    return authorizedFetch(`http://localhost:8000/games`, token, {
      method: "POST",
      body: JSON.stringify({ usernames, name: "TODO" })
    }).then((response: Response) => {
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      return response.json();
    });
  }

  private fetchGame(gameId: number, token: string): Promise<GameView | void> {
    const onLeft = (errors : t.Errors) => {}
    const onRight = (game : GameView) => game

    return authorizedFetch(`http://localhost:8000/games/${gameId}`, token).then(
      (response: Response) => {
        if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        return response.json()
      }).then((data: object) => {
        console.log({data})
        const result = GameViewV.decode(data)

        console.log(PathReporter.report(result))

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
ul {
  list-style-type: none;
  padding: 0;
}
li {
  display: inline-block;
  margin: 0 10px;
}
a {
  color: #42b983;
}
</style>
