<template>
  <div>
    <h1>Login</h1>
    <form @submit.prevent="onSubmit">
      <div><input v-model="username" placeholder="username" /></div>
      <div>
        <input type="password" v-model="password" placeholder="password" />
      </div>
      <div><button type="submit">Submit</button></div>
    </form>
  </div>
</template>

<script>
import { setAuthToken } from "@/auth";
export default {
  name: "SignupForm",
  data: function() {
    return {
      errors: [],
      username: "",
      password: ""
    };
  },
  methods: {
    onSubmit: function(e) {
      e.preventDefault();
      const body = {
        username: this.username,
        password: this.password
      };
      fetch("http://localhost:8000/login", {
        method: "POST",
        body: JSON.stringify(body)
      })
        .then(response => {
          if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
          }
          return response.json();
        })
        .then(data => {
          setAuthToken(data.token);
          this.$router.push("/");
        });
    }
  }
};
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
