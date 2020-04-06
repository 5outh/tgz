import Vue from "vue";
import Router from "vue-router";
import SignupForm from "@/components/SignupForm.vue";
import LoginForm from "@/components/LoginForm.vue";

Vue.use(Router);

export default new Router({
  routes: [
    {
      path: "/signup",
      name: "Signup Form",
      component: SignupForm
    },
    {
      path: "/login",
      name: "Login Form",
      component: LoginForm
    }
  ]
});
