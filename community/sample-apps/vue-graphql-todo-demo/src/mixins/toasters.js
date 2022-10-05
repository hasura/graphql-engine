export default {
     data() {
          return {
               options: {
                    position: "bottom-right",
                    timeout: 2500,
                    closeOnClick: true,
                    pauseOnFocusLoss: false,
                    pauseOnHover: true,
                    draggable: true,
                    draggablePercent: 0.6,
                    showCloseButtonOnHover: true,
                    hideProgressBar: false,
                    closeButton: "button",
                    icon: true,
                    rtl: false
               }
          }
     },
     methods: {
          success(message) {
               this.$toast.success(message, this.options);
          },
          warning(message) {
               this.$toast.warning(message, this.options);
          },
          error(message) {
               this.$toast.error(message, this.options);
          },
          info(message) {
               this.$toast.info(message, this.options);
          }
     },
}