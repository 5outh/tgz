<template>
  <div>
    <h1>Home</h1>
    {{ token }}
  </div>
</template>

<script lang="ts">
import { getAuthToken } from "@/auth";
import { Component, Vue } from "vue-property-decorator";

@Component
export default class Home extends Vue {
  private token: string | null = null;

  mounted() {
    this.token = getAuthToken();
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
