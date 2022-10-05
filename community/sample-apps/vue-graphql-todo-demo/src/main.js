import Vue from 'vue'
import App from './App.vue'
import apolloProvider from './apollo'
import Toast from "vue-toastification"
import "vue-toastification/dist/index.css"
import VueRouter from 'vue-router'
import routes from './routes'

Vue.config.productionTip = false
Vue.use(VueRouter)

Vue.use(Toast, {
  transition: "Vue-Toastification__fade",
  maxToasts: 8
})

const scrollBehaviour = () => {
  return {
    x: 0,
    y: 0
  }
}

const router = new VueRouter({
  scrollBehaviour,
  routes,
  mode: 'history'
});

router.beforeEach((to, from, next) => {
  if (to.matched.some(record => record.meta.requiresAuth)) {
    next(); // go to wherever I'm going
  } else {
    next(); // does not require auth, make sure to always call next()!
  }
});


new Vue({
  router,
  apolloProvider,
  render: h => h(App)
}).$mount('#app')
