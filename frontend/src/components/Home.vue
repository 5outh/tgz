<template>
  <div>
    <h1>Home</h1>
    {{ token }}
    {{ usernameText }}
    <textarea v-model="usernameText" />
    <button @click="this.createNewGame">New Game</button>
  </div>
</template>

<script lang="ts">
import { getAuthToken } from "@/auth";
import { Component, Prop, Vue } from "vue-property-decorator";
import { authorizedFetch } from "@/api";

interface Game {
  waterTiles: number;
}

@Component
export default class Home extends Vue {
  @Prop({ required: true }) readonly userId!: number;
  private token: string | null = null;
  private games: Array<Game> = [];
  private usernameText: string = "";

  mounted() {
    const self = this;
    const token = getAuthToken();

    if (token === null || token === undefined) {
      return;
    }
    this.token = token;

    this.fetchGames(this.userId, token)
      .then((games: Array<Game>) => {
        self.games = games;
      })
      .catch(console.error);
  }

  private createNewGame() {
    const users = this.usernameText.split("\n");
    if (this.token) {
      console.log(users);
      this.newGame(users, this.token).then(console.log);
    }
  }

  private updateUsernames() {
    alert("usernames");
  }

  private newGame(usernames: Array<string>, token: string): Promise<Game> {
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

  private fetchGames(userId: number, token: string): Promise<Array<Game>> {
    return authorizedFetch(
      `http://localhost:8000/users/${this.userId}/games`,
      token
    ).then((response: Response) => {
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
