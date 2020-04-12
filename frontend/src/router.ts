import Vue from "vue";
import Router from "vue-router";
import SignupForm from "@/components/SignupForm.vue";
import LoginForm from "@/components/LoginForm.vue";
import Game from "@/components/Game.vue";
import Home from "@/components/Home.vue";

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
    },
    {
      path: "/",
      name: "Home",
      component: Home,
      props: { userId: 18 } // TODO: Fix
    },
    {
      path: "/games/:id",
      name: "Game",
      component: Game
    }
  ]
});
