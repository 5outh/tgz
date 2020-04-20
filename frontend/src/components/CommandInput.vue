<template>
  <div>
    <div><textarea v-model="command" rows="6" columns="100" /></div>
    <button @click="this.preview">Preview</button>
    <button @click="this.save">Save</button>
  </div>
</template>

<script lang="ts">
import { getAuthToken } from "@/auth";
import { Component, Vue, Prop } from "vue-property-decorator";

@Component
export default class CommandInput extends Vue {
  private command: string = "";

  preview() {
    console.log({ preview: this.command });
  }
  save() {
    console.log({ save: this.command });
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
