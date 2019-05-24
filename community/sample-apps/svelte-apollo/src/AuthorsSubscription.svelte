<script context="module">
  import gql from 'graphql-tag';
  import { client } from './apollo';

  import { subscribe } from 'svelte-apollo';

  const AUTHOR_LIST = gql`
    subscription {
      author(order_by: [{name: asc}]) {
        name
      }
    }
  `;
  const authorsList = subscribe(client, { query: AUTHOR_LIST });
</script>

<ul>
  {#await $authorsList}
    <li>Loading...</li>
  {:then result}
    {#each result.data.author as author (author.id)}
      <li>{author.name}</li>
    {:else}
      <li>No authors found</li>
    {/each}
  {:catch error}
    <li>Error loading authors: {error}</li>
  {/await}
</ul>

