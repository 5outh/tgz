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
import type { Game } from "@/types";

@Component
export default class Home extends Vue {
  private token: string | null = null;
  private usernameText: string = "";
  private game: Game | null = null;

  mounted() {
    const self = this;
    const token = getAuthToken();

    if (token === null || token === undefined) {
      return;
    }
    this.token = token;

    this.fetchGame(parseInt(this.$route.params.id), token)
      .then((game: Game) => {
        self.game = game;
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

  private fetchGame(gameId: number, token: string): Promise<Game> {
    return authorizedFetch(`http://localhost:8000/games/${gameId}`, token).then(
      (response: Response) => {
        if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        return response.json();
      }
    );
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
