class FeedItem {
  String id = "";
  String username = "";
  String feed = "";

  FeedItem.fromElements(String id, String username, String feed) {
    this.id = id;
    this.username = username;
    this.feed = feed;
  }
}
