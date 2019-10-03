import authService from "../auth/authService";

export default {
  install(Vue) {
    Vue.prototype.$auth = authService;

    Vue.mixin({
      created() {
        if (this.handleLoginEvent) {
          authService.addListener("loginEvent", this.handleLoginEvent);
        }
      },

      destroyed() {
        if (this.handleLoginEvent) {
          authService.removeListener("loginEvent", this.handleLoginEvent);
        }
      }
    });
  }
};
