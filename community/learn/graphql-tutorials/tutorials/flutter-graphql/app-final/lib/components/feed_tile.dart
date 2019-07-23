import 'package:flutter/material.dart';

class FeedTile extends StatelessWidget {
  final String username;
  final String feed;

  FeedTile({
    Key key,
    @required this.username,
    @required this.feed,
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Card(
      child: ListTile(
        title: Text(username),
        subtitle: Text(feed),
      ),
    );
  }
}
