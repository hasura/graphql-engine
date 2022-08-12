<template>
  <div className="banner" @click="bannerClick">
    You have {{numOfNewMessages}} new message(s)
  </div>
</template>

<script>
  export default {
    name: "Banner",
    props: ["numOfNewMessages"],
    methods: {
      bannerClick(){
        this.$emit("bannerClicked")
      }
    }
  }
</script>

<style scoped>

</style>