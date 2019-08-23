<template>
  <q-layout view="lHh Lpr lFf">
    <q-layout-header>
      <q-toolbar
        color="primary"
        :glossy="$q.theme === 'mat'"
        :inverted="$q.theme === 'ios'"
      >
        <q-btn
          flat
          dense
          round
          @click="leftDrawerOpen = !leftDrawerOpen"
          aria-label="Menu"
        >
          <q-icon name="menu" />
        </q-btn>

        <q-toolbar-title>
          Quasar App with Hasura GraphQL Engine
          <div slot="subtitle">Running on Quasar v{{ $q.version }}</div>
        </q-toolbar-title>
      </q-toolbar>
    </q-layout-header>

    <q-layout-drawer
      v-model="leftDrawerOpen"
      :content-class="$q.theme === 'mat' ? 'bg-grey-2' : null"
    >
      <q-list
        no-border
        link
        inset-delimiter
      >
        <q-list-header>Author List</q-list-header>
        <q-item v-for="item in author" :key="item.id">
          <q-item-side icon="arrow_forward" />
          <q-item-main :label="item.name" @click.native="fetchArticles(item)" />
        </q-item>
      </q-list>
    </q-layout-drawer>

    <q-page-container>
      <router-view />
    </q-page-container>
  </q-layout>
</template>

<script>
import { openURL } from 'quasar'
import gql from 'graphql-tag'

const authorQuery = gql`
  query {
    author {
      id
      name
    }
  }`
export default {
  name: 'MyLayout',
  data () {
    return {
      leftDrawerOpen: this.$q.platform.is.desktop
    }
  },
  methods: {
    openURL,
    fetchArticles (item) {
      this.$router.push('/author/' + item.id)
    }
  },
  apollo: {
    // Simple query that will update the 'author' vue property
    author: authorQuery
  }

}
</script>

<style>
</style>
