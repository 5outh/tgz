<template>
  <div>
    <h1>Home</h1>

    <ul v-if="this.errors.length > 0">
      <h3>Errors!</h3>
      <li v-for="(error, i) in this.errors" :key="i">{{ error }}</li>
    </ul>

    <p v-if="user">Logged in as {{ user.username }}</p>

    <h3>Games</h3>
    <ul>
      <li v-for="(game, i) in this.games" :key="i">
        <router-link :to="{ name: 'Game', params: { id: game.id } }">{{
          game.name
        }}</router-link>
      </li>
    </ul>

    <h3>New Game</h3>
    <span> Game Name (Required): <input type="text" v-model="gameName"/></span>
    <p>Players (One per line)</p>
    <textarea v-model="usernameText" />
    <button @click="this.createNewGame">New Game</button>
  </div>
</template>

<script lang="ts">
import { getAuthToken } from "@/auth";
import { Component, Prop, Vue } from "vue-property-decorator";
import { authorizedFetch, fetchMe, fetchGames } from "@/api";
import type { User, GameView } from "@/types"
import {UserV} from "@/types"
import { pipe } from 'fp-ts/lib/pipeable'
import { fold, bimap } from 'fp-ts/lib/Either'
import { PathReporter } from 'io-ts/lib/PathReporter'
import * as t from 'io-ts'

@Component
export default class Home extends Vue {
  @Prop({ required: true }) readonly userId!: number;
  private token: string | null = null;
  private games: Array<GameView> = [];
  private errors: Array<string> = [];
  private usernameText: string = "";
  private gameName: string = "";
  private user: User | null = null;

  mounted() {
    this.setup();
  }

  private reportErrors(a: any) {
    this.errors = this.errors.concat(PathReporter.report(a))
  }

  private async setup() {
    const token = getAuthToken();

    if (token === null || token === undefined) {
      return;
    }
    this.token = token;

    const games = await fetchGames(this.userId);
    bimap(() => this.reportErrors(games), (a: Array<GameView>) => { this.games = a })(games)

    const me = await fetchMe();
    bimap(() => this.reportErrors(me), (a: User) => { this.user = a })(me)
  }

  private async createNewGame() {
    const users = this.usernameText.split("\n");

    if (this.token && this.gameName) {
      const game = await this.newGame(users, this.gameName)
      this.$router.push(`/games/${game.id}`)
    }
  }

  private newGame(usernames: Array<string>, gameName: string): Promise<GameView> {
    return authorizedFetch(`http://localhost:8000/games`, {
      method: "POST",
      body: JSON.stringify({ usernames, name: gameName })
    }).then((response: Response) => {
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      return response.json();
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
</style>
