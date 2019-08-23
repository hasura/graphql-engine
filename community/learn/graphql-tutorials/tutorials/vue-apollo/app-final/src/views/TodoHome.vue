<template>
  <div>
    <div class="col-xs-12 col-md-12 col-lg-9 col-sm-12 noPadd">
      <div>
        <div class="col-md-6 col-sm-12">
          <div class="wd95 addPaddTopBottom">
            <div class="sectionHeader">Personal todos</div>
            <TodoPrivateWrapper />
          </div>
        </div>
        <div class="col-xs-12 col-md-6 col-sm-12 grayBgColor todoMainWrapper commonBorRight">
          <div class="wd95 addPaddTopBottom">
            <div class="sectionHeader">Public feed (realtime)</div>
            <TodoPublicWrapper />
          </div>
        </div>
      </div>
    </div>
    <div class="col-xs-12 col-lg-3 col-md-12 col-sm-12 noPadd">
      <OnlineUsers />
    </div>
  </div>
</template>

<script>
import TodoPrivateWrapper from "../components/TodoPrivateWrapper";
import TodoPublicWrapper from "../components/TodoPublicWrapper";
import OnlineUsers from "../components/OnlineUsers";

export default {
  components: {
    TodoPrivateWrapper, TodoPublicWrapper, OnlineUsers
  }
};
</script>
