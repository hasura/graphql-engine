---
title: "Set up a GraphQL client with  Flutter GraphQL Package"
metaTitle: "Flutter GraphQL Package Setup | GraphQL Flutter Tutorial"
metaDescription: "You will learn how to configure GraphQL Package in Flutter by importing graphql_flutter dependency"
---

graphql_flutter plugin gives a neat abstraction layer and an interface to your GraphQL server. You don't need to worry about constructing your queries with request body, headers and options, that you might have done with `http` or `dio` say. You can directly write queries and mutations in GraphQL and they will automatically be sent to your server via your apollo client instance.

### Flutter GraphQL Installation
Let's get started by adding `graphql_flutter` dependency in pubspec.yaml :

```yaml
dependencies:
  graphql_flutter: ^1.0.1
```

### Create GraphQLClient Instance
Create a folder called `config` inside `lib` and inside it create a file called `client.dart` in that file create a class named Config and initalize `httpLink` , `authLink` , `websocketLink` and concat all of them to `link`. Now create a function to initailize `ValueNotifier` of `GraphQLClient` type and pass that link in GraphQLClient constructor.

 <!-- FIXME: Add the github link for file here -->

```dart
import 'package:app_final/services/shared_preferences_service.dart';
import 'package:flutter/material.dart';
import 'package:graphql_flutter/graphql_flutter.dart';

class Config {
  static final HttpLink httpLink = HttpLink(
    uri: 'https://learn.hasura.io/graphql',
  );

  static final AuthLink authLink =
      AuthLink(getToken: () async => await sharedPreferenceService.token);
  static final WebSocketLink websocketLink = WebSocketLink(
    url: 'wss://learn.hasura.io/graphql',
    config: SocketClientConfig(
      autoReconnect: true,
      inactivityTimeout: Duration(seconds: 30),
    ),
  );

  static final Link link =
      authLink.concat(httpLink as Link).concat(websocketLink);

  static ValueNotifier<GraphQLClient> initailizeClient() {
    ValueNotifier<GraphQLClient> client = ValueNotifier(
      GraphQLClient(
        cache: OptimisticCache(dataIdFromObject: typenameDataIdFromObject),
        link: link,
      ),
    );
    return client;
  }
}
```

Let's try to understand what is happening here. 

### Link and OptimisticCache
We are creating an `HttpLink` , `AuthLink` , `WebSocketLink` and concatenating them to connect GraphQLClient with the GraphQL server. As you know already, our GraphQL server is running at [https://learn.hasura.io/graphql](https://learn.hasura.io/graphql)

At the end, we instantiate GraphQLClient by passing in our HttpLink and a new instance of `OptimisticCache` . We are wrapping all of this in a function which will return the client.

We are going to make use of this function inside dashboard screen as it is very first page which require graphQL (`lib/screens/dashboard.dart`).

Go to `lib/screens/dashboard.dart`, Wrap whole Widget(DefaultTabController in this case) in `CacheProvider` and then again in `GraphQLProvider` . GraphQLProvider `client` as an argument, for that create  instance of ` ValueNotifier<GraphQLClient>` using function `initailizeClient` created in client.dart and pass it to `client`. GraphQLProvider provides this client's to the context of the all the child widgets so that it can be used wherever required.

```dart
import 'package:app_final/config/client.dart';
import 'package:app_final/screens/tabs/dashboard/feeds.dart';
import 'package:app_final/screens/tabs/dashboard/online.dart';
import 'package:app_final/screens/tabs/dashboard/todos.dart';
import 'package:app_final/services/shared_preferences_service.dart';
import 'package:flutter/material.dart';
import 'package:graphql_flutter/graphql_flutter.dart';

class Dashboard extends StatelessWidget {
  Dashboard({Key key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return GraphQLProvider(
      client: Config.initailizeClient(),
      child: CacheProvider(
        child: DefaultTabController(
          length: 3,
          child: Scaffold(
            appBar: AppBar(
              automaticallyImplyLeading: false,
              centerTitle: true,
              title: Text(
                "ToDo App",
              ),
              actions: <Widget>[
                IconButton(
                  icon: Icon(Icons.exit_to_app),
                  onPressed: () async {
                    sharedPreferenceService.clearToken();
                    Navigator.pushReplacementNamed(context, "/login");
                  },
                ),
              ],
            ),
            bottomNavigationBar: new TabBar(
              tabs: [
                Tab(
                  text: "Todos",
                  icon: new Icon(Icons.edit),
                ),
                Tab(
                  text: "Feeds",
                  icon: new Icon(Icons.message),
                ),
                Tab(
                  text: "Online",
                  icon: new Icon(Icons.people),
                ),
              ],
              labelColor: Colors.black,
              unselectedLabelColor: Colors.grey,
              indicatorSize: TabBarIndicatorSize.label,
              indicatorPadding: EdgeInsets.all(5.0),
              indicatorColor: Colors.blue,
            ),
            body: TabBarView(
              physics: NeverScrollableScrollPhysics(),
              children: [
                Todos(),
                Feeds(),
                Online(),
              ],
            ),
          ),
        ),
      ),
    );
  }
}
```
Once this has been completed, we can now use the features of GraphQL Client anywhere in the child widgets.
