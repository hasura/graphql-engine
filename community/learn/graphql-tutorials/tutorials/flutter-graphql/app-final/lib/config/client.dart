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
