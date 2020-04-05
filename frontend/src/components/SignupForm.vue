<template>
  <form @submit.prevent="onSubmit">
    <input v-model="username" placeholder="username">
    <input type="password" v-model="password" placeholder="password">
    <input type="email" v-model="email" placeholder="email address">
    <button type="submit" >Submit</button>
    {{ {username,password,email} }}
  </form>
</template>

<script>
export default {
  name: "SignupForm",
  data: function() {
    return {
      errors: [],
      email: "",
      username: "",
      password: ""
    }
  },
  methods: {
    onSubmit(e) {
      e.preventDefault()
      const body = {email: this.email, username:this.username, password:this.password}
      fetch('http://localhost:8000/signup', { method:'POST', body: JSON.stringify(body) })
        .then((response) => {
          return response.json();
        })
        .then((data) => {
          console.log(data);
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
