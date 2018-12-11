import Vue from 'vue'
import Router from 'vue-router'
import AddUser from '../components/AddUser'
import Dashboard from '../components/Dashboard'

Vue.use(Router)

export default new Router({
  mode: 'history',
  routes: [
    {
      path: '/',
      name: 'Home',
      component: AddUser
    },
    {
      path: '/user/:id/todos',
      name: 'Dashboard',
      component: Dashboard,
      props: true
    }
  ]
})
