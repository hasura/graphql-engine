import Vue from "vue";
import App from "./App.vue";
import router from "./router";
import AuthPlugin from "./plugins/auth";

import createProvider from './apollo'
import VueApollo from 'vue-apollo'

Vue.use(AuthPlugin);
Vue.use(VueApollo);

Vue.config.productionTip = false;

const apolloProvider = new VueApollo({
  defaultClient: createProvider(),
})

new Vue({
  router,
  apolloProvider,
  render: h => h(App)
}).$mount("#app");
