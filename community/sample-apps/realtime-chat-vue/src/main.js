import Vue from "vue";
import App from "./App.vue";
import router from "./router";
import store from "./store";
import style from "./App.css";
import { createProvider } from "./vue-apollo";
Vue.config.productionTip = false;

new Vue({
  router,
  style,
  store,
  apolloProvider: createProvider(),
  render: h => h(App)
}).$mount("#app");
