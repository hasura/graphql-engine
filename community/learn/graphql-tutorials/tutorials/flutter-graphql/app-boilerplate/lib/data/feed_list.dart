import 'package:app_boilerplate/model/feed_item.dart';

class FeedList {
  List<FeedItem> list = [
    FeedItem.fromElements("", "user1", "I'm user1"),
    FeedItem.fromElements("", "user2", "I'm user2"),
    FeedItem.fromElements("", "user3", "I'm user3"),
    FeedItem.fromElements("", "user4", "I'm user4"),
    FeedItem.fromElements("", "user5", "I'm user5"),
  ];

  addFeed(String id, String username, String feed) {
    list.add(
      FeedItem.fromElements(id, username, feed),
    );
  }
}

FeedList feedList = new FeedList();
