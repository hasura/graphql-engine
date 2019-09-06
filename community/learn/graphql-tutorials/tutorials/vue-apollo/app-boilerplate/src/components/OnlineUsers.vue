<template>
  <div class="sliderMenu grayBgColor">
    <div class="sliderHeader">
      Online users - {{ online_list.length }}
    </div>
    <div class="userInfo" v-for="user in online_list" v-bind:key="user.user.name">
      <div class="userImg">
        <i class="far fa-user" />
      </div>
      <div class="userName">
        {{ user.user.name }}
      </div>
    </div>
  </div>
</template>

<script>
  export default {
    data() {
      return {
        online_list: [
          { user: { name: "someUser1" }},
          { user: { name: "someUser2" }}
        ]
      };
    },
  }

</script>
