<template>
  <div class="apollo-example">
    <!-- Cute tiny form -->
    <div class="form">
      <label for="field-name" class="label">Name</label>
      <input
        v-model="name"
        placeholder="Type a name"
        class="input"
        id="field-name"
      />
    </div>

    <!-- Apollo watched Graphql query -->
    <ApolloQuery
      :query="require('../graphql/HelloWorld.gql')"
      :variables="{ name }"
    >
      <template slot-scope="{ result: { loading, error, data } }">
        <!-- Loading -->
        <div v-if="loading" class="loading apollo">Loading...</div>

        <!-- Error -->
        <div v-else-if="error" class="error apollo">An error occured</div>

        <!-- Result -->
        <div v-else-if="data" class="result apollo">{{ data.hello }}</div>

        <!-- No result -->
        <div v-else class="no-result apollo">No result :(</div>
      </template>
    </ApolloQuery>

    <!-- Tchat example -->
    <ApolloQuery :query="require('../graphql/Messages.gql')">
      <ApolloSubscribeToMore
        :document="require('../graphql/MessageAdded.gql')"
        :update-query="onMessageAdded"
      />

      <div slot-scope="{ result: { data } }">
        <template v-if="data">
          <div
            v-for="message of data.messages"
            :key="message.id"
            class="message"
          >
            {{ message.text }}
          </div>
        </template>
      </div>
    </ApolloQuery>

    <ApolloMutation
      :mutation="require('../graphql/AddMessage.gql')"
      :variables="{
        input: {
          text: newMessage
        }
      }"
      class="form"
      @done="newMessage = ''"
    >
      <template slot-scope="{ mutate }">
        <form v-on:submit.prevent="formValid && mutate()">
          <label for="field-message">Message</label>
          <input
            id="field-message"
            v-model="newMessage"
            placeholder="Type a message"
            class="input"
          />
        </form>
      </template>
    </ApolloMutation>

    <div class="images">
      <div v-for="file of files" :key="file.id" class="image-item">
        <img :src="`${$filesRoot}/${file.path}`" class="image" />
      </div>
    </div>

    <div class="image-input">
      <label for="field-image">Image</label>
      <input
        id="field-image"
        type="file"
        accept="image/*"
        required
        @change="onUploadImage"
      />
    </div>
  </div>
</template>

<script>
import FILES from "../graphql/Files.gql";
import UPLOAD_FILE from "../graphql/UploadFile.gql";

export default {
  data() {
    return {
      name: "Anne",
      newMessage: ""
    };
  },

  apollo: {
    files: FILES
  },

  computed: {
    formValid() {
      return this.newMessage;
    }
  },

  methods: {
    onMessageAdded(previousResult, { subscriptionData }) {
      return {
        messages: [
          ...previousResult.messages,
          subscriptionData.data.messageAdded
        ]
      };
    },

    async onUploadImage({ target }) {
      if (!target.validity.valid) return;
      await this.$apollo.mutate({
        mutation: UPLOAD_FILE,
        variables: {
          file: target.files[0]
        },
        update: (store, { data: { singleUpload } }) => {
          const data = store.readQuery({ query: FILES });
          data.files.push(singleUpload);
          store.writeQuery({ query: FILES, data });
        }
      });
    }
  }
};
</script>

<style scoped>
.form,
.input,
.apollo,
.message {
  padding: 12px;
}

label {
  display: block;
  margin-bottom: 6px;
}

.input {
  font-family: inherit;
  font-size: inherit;
  border: solid 2px #ccc;
  border-radius: 3px;
}

.error {
  color: red;
}

.images {
  display: grid;
  grid-template-columns: repeat(auto-fill, 300px);
  grid-auto-rows: 300px;
  grid-gap: 10px;
}

.image-item {
  display: flex;
  align-items: center;
  justify-content: center;
  background: #ccc;
  border-radius: 8px;
}

.image {
  max-width: 100%;
  max-height: 100%;
}

.image-input {
  margin: 20px;
}
</style>
