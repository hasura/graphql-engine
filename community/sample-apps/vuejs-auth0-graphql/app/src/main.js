import Vue from "vue";
import App from "./App.vue";
import router from "./router";
import AuthPlugin from "./plugins/auth";
import HighlightJs from "./directives/highlight";

import { library } from "@fortawesome/fontawesome-svg-core";
import { faLink } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/vue-fontawesome";
import { createProvider } from './vue-apollo'

Vue.use(AuthPlugin);
Vue.directive("highlightjs", HighlightJs);

Vue.config.productionTip = false;

library.add(faLink);

Vue.component("font-awesome-icon", FontAwesomeIcon);

new Vue({
  router,
  apolloProvider: createProvider(),
  render: h => h(App)
}).$mount("#app");
