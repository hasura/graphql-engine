class FeedItem {
  String id = "";
  String username = "";
  String feed = "";

  FeedItem.fromElements(String id, String username, String feed) {
    this.username = username;
    this.feed = feed;
    this.id = id;
  }
}
