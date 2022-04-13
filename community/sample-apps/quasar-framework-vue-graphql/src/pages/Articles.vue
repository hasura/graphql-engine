<template>
  <q-page padding>
    <q-item v-for="article in articles" :key="article.id">
      <q-item-section class="my-box cursor-pointer q-hoverable">
        <q-item-label>
          {{ article.title }}
        </q-item-label>
        <q-item-label caption>ID: {{ article.id }}</q-item-label>
        <q-item-label>
          {{ article.content }}
        </q-item-label>
      </q-item-section>
    </q-item>
  </q-page>
</template>

<script>
import { watch } from "vue";
import { useQuery, useResult } from "@vue/apollo-composable";
import { useRoute } from "vue-router";
import gql from "graphql-tag";

export default {
  name: "PageName",

  setup() {
    const route = useRoute();

    const { result, loading, error, refetch } = useQuery(
      gql`
        query articleQuery($authorId: Int!) {
          article(where: { author_id: { _eq: $authorId } }) {
            id
            title
            content
          }
        }
      `,
      {
        authorId: route.params.authorId,
      }
    );

    const articles = useResult(result, null, (data) => data.article);

    watch(
      () => route.params.authorId,
      async (newId) => {
        refetch({ authorId: newId });
      }
    );

    return {
      articles,
    };
  },
};
</script>
