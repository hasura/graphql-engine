import 'package:app_final/model/feed_item.dart';

class FeedList {
  List<FeedItem> list = [];

  addFeed(String id, String username, String feed) {
    list.add(
      FeedItem.fromElements(id, username, feed),
    );
  }

  addfirstFeed(String id, String username, String feed) {
    list.insert(
      0,
      FeedItem.fromElements(id, username, feed),
    );
  }
}

FeedList feedList = new FeedList();
