import Vue from "vue";
import App from "./App.vue";
import router from "./router";
import AuthPlugin from "./plugins/auth";

Vue.use(AuthPlugin);

Vue.config.productionTip = false;

new Vue({
  router,
  render: h => h(App)
}).$mount("#app");
