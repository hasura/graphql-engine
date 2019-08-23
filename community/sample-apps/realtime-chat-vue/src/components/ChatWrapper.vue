<template>
  <div class="chatWrapper">
    <div class="wd25 hidden-xs">
        <OnlineUsers
          :userId="userId"
          :username="username"
        />
    </div>
    <div class="mobileview visible-xs">
      <OnlineUsers
        :userId="userId"
        :username="username"
        :isMobileView="true"
      />
    </div>
    <div class="wd75">
      <RenderMessages
        :username="username"
        :userId="userId"
      />
      <Textbox
        :username="username"
        :userId="userId"
      />
    </div>
  </div>
</template>

<script>
  import OnlineUsers from "@/components/OnlineUsers.vue";
  import RenderMessages from "@/components/RenderMessages.vue";
  import Textbox from "@/components/TextBox.vue";

  export default {
    name: "ChatWrapper",
    components: {
      OnlineUsers,
      RenderMessages,
      Textbox
    },
    props: {
      userId: {
        type: String,
        required: true
      },
      username: {
        type: String
      }
    }
  }
</script>

<style scoped>

</style>