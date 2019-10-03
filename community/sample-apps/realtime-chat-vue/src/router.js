import Vue from "vue";
import Router from "vue-router";
import Home from "./views/Home.vue";
import Login from "./views/Login.vue";
import { TokenService } from "./services/storage";

Vue.use(Router);

const router = new Router({
  mode: "history",
  base: process.env.BASE_URL,
  routes: [
    {
      path: "/",
      name: "home",
      component: Home
    },
    {
      path: "/chat/:userId",
      component: Home,
      props: true
    },
    {
      path: "/login",
      name: "login",
      component: Login,
      meta: {
        public: true
      }
    }
  ]
});

router.beforeEach((to, from, next) => {
  const isPublic = to.matched.some(record => record.meta.public);
  const onlyWhenLoggedOut = to.matched.some(record => record.meta.onlyWhenLoggedOut)
  const loggedIn = !!TokenService.getToken();
  if (!isPublic && !loggedIn) {
    return next({
      path: "/login",
      query: { redirect: to.fullPath }
    });
  }
  if (loggedIn && onlyWhenLoggedOut) {
    return next('/')
  }
  next();
});

export default router;
