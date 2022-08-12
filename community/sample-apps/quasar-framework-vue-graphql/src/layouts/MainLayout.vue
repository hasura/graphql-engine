<template>
  <q-layout view="lHh Lpr lFf">
    <q-header elevated>
      <q-toolbar>
        <q-btn
          flat
          dense
          round
          icon="menu"
          aria-label="Menu"
          @click="toggleLeftDrawer"
        />

        <q-toolbar-title
          class="my-box cursor-pointer q-hoverable"
          @click="this.$router.push(`/`)"
        >
          Quasar App with Hasura GraphQL Engine
        </q-toolbar-title>

        <div>Running on Quasar v{{ $q.version }}</div>
      </q-toolbar>
    </q-header>

    <q-drawer v-model="leftDrawerOpen" show-if-above bordered>
      <q-list>
        <q-item-label header> Author List </q-item-label>

        <q-item v-for="author in authors" :key="author.id">
          <q-item-section class="my-box cursor-pointer q-hoverable">
            <q-item-label @click="fetchArticle(author)">
              {{ author.name }}
            </q-item-label>
            <q-item-label caption>ID: {{ author.id }}</q-item-label>
          </q-item-section>
        </q-item>
      </q-list>
    </q-drawer>

    <q-page-container>
      <router-view />
    </q-page-container>
  </q-layout>
</template>

<script>
import { defineComponent, ref } from "vue";
import { useRouter } from "vue-router";
import { useQuery, useResult } from "@vue/apollo-composable";
import gql from "graphql-tag";

export default defineComponent({
  name: "MainLayout",

  setup() {
    const leftDrawerOpen = ref(false);
    const router = useRouter();

    const { result, loading, error } = useQuery(gql`
      query {
        author {
          id
          name
        }
      }
    `);

    const authors = useResult(result, null, (data) => data.author);

    const fetchArticle = (author) => {
      return router.push(`/author/${author.id}`);
    };

    return {
      authors,
      leftDrawerOpen,
      fetchArticle,
      toggleLeftDrawer() {
        leftDrawerOpen.value = !leftDrawerOpen.value;
      },
    };
  },
});
</script>
