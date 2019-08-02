import 'package:app_final/components/custom_button.dart';
import 'package:app_final/components/feed_tile.dart';
import 'package:app_final/data/feed_fetch.dart';
import 'package:app_final/data/feed_list.dart';
import 'package:flutter/material.dart';
import 'package:graphql_flutter/graphql_flutter.dart';

class Feeds extends StatefulWidget {
  const Feeds({Key key}) : super(key: key);

  @override
  _FeedsState createState() => _FeedsState();
}

class _FeedsState extends State<Feeds> {
  static int _lastLatestFeedId;
  static int _oldestFeedId;
  static int _newTodoCount = 0;
  static int _previousId = 0;
  static int _newId = 0;
  GraphQLClient _client;
  TextEditingController _controller = TextEditingController();

  @override
  void initState() {
    WidgetsBinding.instance.addPostFrameCallback((_) {
      _client = GraphQLProvider.of(context).value;
    });
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Column(
      children: <Widget>[
        Mutation(
          options: MutationOptions(document: FeedFetch.addPublicTodo),
          builder: (
            RunMutation runMutation,
            QueryResult result,
          ) {
            return Padding(
              padding: const EdgeInsets.all(18.0),
              child: Row(
                mainAxisSize: MainAxisSize.min,
                children: <Widget>[
                  Expanded(
                    child: TextFormField(
                      controller: _controller,
                      decoration: InputDecoration(
                        labelText: "Say something ...",
                        border: OutlineInputBorder(),
                      ),
                    ),
                  ),
                  Padding(
                    padding: const EdgeInsets.all(8.0),
                    child: CustomButton(
                      width: 90,
                      height: 50,
                      onTap: () {
                        runMutation({"title": _controller.text});
                        //feedList.addFeed("You", _controller.text);
                        _controller.clear();
                        FocusScope.of(context).requestFocus(new FocusNode());
                      },
                      text: "Post",
                    ),
                  )
                ],
              ),
            );
          },
        ),
        Subscription(
          "fetchNewNotification",
          FeedFetch.fetchNewNotification,
          builder: ({
            bool loading,
            dynamic payload,
            dynamic error,
          }) {
            if (payload != null) {
              _newId = payload['todos'][0]['id'];

              if (_previousId != 0) {
                _newTodoCount = _newTodoCount + (_newId - _previousId);
              } else {
                _lastLatestFeedId = _newId;
                _client
                    .query(
                  QueryOptions(
                    document: FeedFetch.loadMoreTodos,
                    variables: {"oldestTodoId": _newId + 1},
                  ),
                )
                    .then((onValue) {
                  for (var todo in onValue.data.data['todos']) {
                    feedList.addFeed(todo['id'].toString(), todo['title'],
                        todo['user']['name']);
                  }
                  setState(() {});
                  // feedList.addFeed(username, feed)
                }).catchError((onError) {
                  print(onError);
                });
              }
              _previousId = payload['todos'][0]['id'];

              if (_newTodoCount != 0) {
                return CustomButton(
                  onTap: () {
                    _client
                        .query(
                      QueryOptions(
                        document: FeedFetch.newTodos,
                        variables: {"latestVisibleId": _lastLatestFeedId},
                      ),
                    )
                        .then((onValue) {
                      for (var todo in onValue.data.data['todos'].reversed) {
                        feedList.addfirstFeed(todo['id'].toString(),
                            todo['title'], todo['user']['name']);
                      }
                      _lastLatestFeedId = int.parse(feedList.list.first.id);
                      _newTodoCount = 0;
                      setState(() {});
                    }).catchError((onError) {
                      print(onError);
                    });
                  },
                  height: 50,
                  text: " $_newTodoCount New Notification",
                  width: MediaQuery.of(context).size.width / 2,
                );
              } else
                return SizedBox();
            } else {
              return SizedBox();
            }
          },
        ),
        Expanded(
          child: ListView.builder(
            itemCount: feedList.list.length,
            itemBuilder: (context, index) {
              return FeedTile(
                  username: feedList.list[index].username,
                  feed: feedList.list[index].feed);
            },
          ),
        ),
        CustomButton(
          onTap: () {
            _oldestFeedId = int.parse(feedList.list.last.id);
            _client
                .query(
              QueryOptions(
                document: FeedFetch.loadMoreTodos,
                variables: {"oldestTodoId": _oldestFeedId},
              ),
            )
                .then((onValue) {
              for (var todo in onValue.data.data['todos']) {
                feedList.addFeed(
                    todo['id'].toString(), todo['title'], todo['user']['name']);
              }
              setState(() {});
            }).catchError((onError) {
              print(onError);
            });
          },
          height: 50,
          text: "Load More",
          width: MediaQuery.of(context).size.width / 3,
        )
      ],
    );
  }
}
