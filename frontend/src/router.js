import Vue from "vue";
import Router from "vue-router";
import SignupForm from "@/components/SignupForm";

Vue.use(Router);

export default new Router({
  routes: [
    {
      path: "/signup",
      name: "Signup Form",
      component: SignupForm
    }
  ]
});
