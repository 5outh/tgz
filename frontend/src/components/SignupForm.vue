<template>
  <div>
    <h1>Signup</h1>
    <form @submit.prevent="onSubmit">
      <div><input v-model="username" placeholder="username" /></div>
      <div>
        <input type="password" v-model="password" placeholder="password" />
      </div>
      <div>
        <input type="email" v-model="email" placeholder="email address" />
      </div>
      <div><button type="submit">Submit</button></div>
    </form>
  </div>
</template>

<script>
import { setAuthToken } from "@/auth";
import { authorizedFetch } from "@/api";
export default {
  name: "SignupForm",
  data: function() {
    return {
      errors: [],
      email: "",
      username: "",
      password: "",
      localStorage: null
    };
  },
  methods: {
    onSubmit: function(e) {
      e.preventDefault();
      const body = {
        email: this.email,
        username: this.username,
        password: this.password
      };
      fetch("http://localhost:8000/signup", {
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
        })
        .then(() => {
          return authorizedFetch("http://localhost:8000/users/me");
        })
        .then(response => {
          if (!response.ok) {
            console.log({ response });
            throw new Error(`HTTP error! status: ${response.status}`);
          }
          return response.json();
        })
        .then(data => {
          this.$router.push(`/home/${data.id}`);
        })
        .catch(alert);
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
