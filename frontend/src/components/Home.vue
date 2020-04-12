<template>
  <div>
    <h1>Home</h1>
    {{ token }}
  </div>
</template>

<script lang="ts">
import { getAuthToken } from "@/auth";
import { Component, Prop, Vue } from "vue-property-decorator";

interface Game {
  waterTiles: number;
}

@Component
export default class Home extends Vue {
  @Prop({ required: true }) readonly userId!: number;
  private token: string | null = null;
  private games: Array<Game> = [];

  mounted() {
    const self = this;
    this.token = getAuthToken();
    // todo;

    this.fetchGames(this.userId)
      .then((games: Array<Game>) => {
        self.games = games;
      })
      .catch(console.error);
  }

  private fetchGames(userId: number): Promise<Array<Game>> {
    return fetch(`http://localhost:8000/users/${userId}/games`, {
      headers: {
        Authorization: `Bearer ${this.token}`
      }
    }).then(response => {
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
