import { NgModule } from "@angular/core";
import { HttpClientModule, HttpHeaders } from "@angular/common/http";
import { Apollo, ApolloModule } from "apollo-angular";
import { HttpLink, HttpLinkModule } from "apollo-angular-link-http";
import { InMemoryCache } from "apollo-cache-inmemory";
import { environment } from "src/environments/environment";

@NgModule({
  exports: [HttpClientModule, ApolloModule, HttpLinkModule]
})
export class GraphQLModule {
  constructor(apollo: Apollo, httpLink: HttpLink) {
    const uri = environment.graphqlEndpoint;

    /** Following values need to be added to the header before making any
     *  query.
     *  1. X-Hasura-Access-Key: This Access Key is what will let your app access your endpoint.
     *  2. Content-Type: To tell the type of content.
     *  3. Authorization: This is the token that tells that a user is logged in.
     *  4. X-Hasura-Role: This will be 'user' to let Hasura know that a user is accessing the endpoint.
     *  5. X-Hasura-User-Id: This the user id of the user.
     */
    const authHeader = new HttpHeaders()
      .set("X-Hasura-Access-Key", environment.hasuraAccessKey)
      .set("Content-Type", "application/json")
      .set("Authorization", `Bearer <token-goes-here>`)
      .set("X-Hasura-Role", "user")
      .set("X-Hasura-User-Id", "<user_id>");

    // Create a HTTP Link with the URI and the header.
    const http = httpLink.create({ uri, headers: authHeader });

    // Create an Apollo client with HTTP Link and cache as InMemoryCache.
    apollo.create({
      link: http,
      cache: new InMemoryCache()
    });
  }
}
