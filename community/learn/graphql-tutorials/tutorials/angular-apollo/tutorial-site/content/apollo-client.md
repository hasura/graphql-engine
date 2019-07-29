---
title: "Set up a GraphQL client with Apollo"
metaTitle: "Apollo Client GraphQL Setup | GraphQL Angular Apollo Tutorial"
metaDescription: "You will learn how to configure Apollo Client in Angular by installing dependencies like apollo-angular, apollo-client, apollo-link-http, apollo-cache-inmemory"
---

import GithubLink from "../src/GithubLink.js";

Apollo gives a neat abstraction layer and an interface to your GraphQL server. You don't need to worry about constructing your queries with request body, headers and options, that you might have done with `axios` or `fetch` say. You can directly write queries and mutations in GraphQL and they will automatically be sent to your server via your apollo client instance.

### Angular Apollo Installation
Let's get started by installing apollo client & peer graphql dependencies:

```bash
$ npm install --save apollo-client apollo-angular apollo-cache-inmemory apollo-link apollo-angular-link-http graphql graphql-tag
```

### Create Apollo Client Instance
Open `src/app/app.module.ts` and add the following imports at the top:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/angular-apollo/app-final/src/app/app.module.ts" text="src/app/app.module.ts" />

```typescript
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';
import { NgModule } from '@angular/core'

+ import { ApolloModule, APOLLO_OPTIONS } from "apollo-angular";
+ import { HttpLinkModule, HttpLink } from "apollo-angular-link-http";
+ import { InMemoryCache } from "apollo-cache-inmemory";
+ import { HttpClientModule  } from "@angular/common/http";
+ import { ApolloClient } from 'apollo-client';

```

These are the required apollo dependencies to get started. Now let's define a provider which will return apollo client with httplink and cache.

```typescript

  imports: [
    BrowserModule,
    AppRoutingModule,
    FormsModule,
+    HttpClientModule,
+    ApolloModule,
+    HttpLinkModule
  ],
  providers: [{
+    provide: APOLLO_OPTIONS,
+    useFactory: (httpLink) => {
+      return new ApolloClient({
+        cache: new InMemoryCache(),
+        link:  httpLink.create({
+          uri: 'https://learn.hasura.io/graphql',
+            headers: {
+              Authorization: `Bearer ${localStorage.getItem('token')}`
+            }
+        })
+      })
+    },
+    deps: [HttpLink]
  }]
```  

Let's try to understand what is happening here. 

### HttpLink and InMemoryCache
We are creating an `HttpLink` to connect ApolloClient with the GraphQL server. As you know already, our GraphQL server is running at [https://learn.hasura.io/graphql](https://learn.hasura.io/graphql)

At the end, we instantiate ApolloClient by passing in our HttpLink and a new instance of `InMemoryCache` (recommended caching solution). We are wrapping all of this in a function which will return the client.

