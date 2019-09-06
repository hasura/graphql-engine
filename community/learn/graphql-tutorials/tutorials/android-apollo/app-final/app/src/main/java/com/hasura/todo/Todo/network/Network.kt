package com.hasura.todo.Todo.network

import android.content.Context
import com.apollographql.apollo.ApolloClient
import com.apollographql.apollo.api.Operation
import com.apollographql.apollo.api.ResponseField
import com.apollographql.apollo.cache.normalized.CacheKey
import com.apollographql.apollo.cache.normalized.CacheKeyResolver
import com.apollographql.apollo.cache.normalized.lru.EvictionPolicy
import com.apollographql.apollo.cache.normalized.lru.LruNormalizedCacheFactory
import com.apollographql.apollo.cache.normalized.sql.ApolloSqlHelper
import com.apollographql.apollo.cache.normalized.sql.SqlNormalizedCacheFactory
import com.apollographql.apollo.response.CustomTypeAdapter
import com.apollographql.apollo.response.CustomTypeValue
import com.apollographql.apollo.subscription.WebSocketSubscriptionTransport
import com.hasura.todo.type.CustomType
import okhttp3.OkHttpClient
import okhttp3.logging.HttpLoggingInterceptor
import java.text.ParseException
import java.text.SimpleDateFormat


private val GRAPHQL_ENDPOINT: String = "https://learn.hasura.io/graphql"
private val GRAPHQL_WEBSOCKET_ENDPOINT: String = "wss://learn.hasura.io/graphql"

private val SQL_CACHE_NAME = "mktodo"


class Network {
    companion object{
        @JvmStatic
        lateinit var apolloClient: ApolloClient
    }

    fun setApolloClient(accessTokenId: String, context: Context){
        val log: HttpLoggingInterceptor = HttpLoggingInterceptor().setLevel(HttpLoggingInterceptor.Level.BODY)
        val authHeader = "Bearer $accessTokenId"
        val okHttpClient = OkHttpClient.Builder()
            .addInterceptor(log)
            .addInterceptor { chain ->
                val original = chain.request()
                val builder = original.newBuilder().method(original.method(), original.body())
                builder.header("Authorization", authHeader)
                chain.proceed(builder.build())
            }
            .build()

        val dateCustomTypeAdapter = object : CustomTypeAdapter<String> {
            var ISO8601 = SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSSSSZ")

            override fun decode(value: CustomTypeValue<*>): String {
                try {
                    return ISO8601.parse(value.value.toString()).toString()
                } catch (e: ParseException) {
                    throw RuntimeException(e)
                }

            }

            override fun encode(value: String): CustomTypeValue<*> {
                return CustomTypeValue.GraphQLString(value)
            }
        }

        val apolloSqlHelper = ApolloSqlHelper(context, SQL_CACHE_NAME)
        val normalizedCacheFactory = LruNormalizedCacheFactory(EvictionPolicy.NO_EVICTION)
            .chain(SqlNormalizedCacheFactory(apolloSqlHelper))

        val cacheKeyResolver: CacheKeyResolver = object : CacheKeyResolver() {
            override fun fromFieldRecordSet(field: ResponseField, recordSet: Map<String, Any>): CacheKey {
                if (recordSet.containsKey("todos")) {
                    val id = recordSet["todos"] as String
                    return CacheKey.from(id)
                }
                return CacheKey.NO_KEY
            }

            override fun fromFieldArguments(field: ResponseField, variables: Operation.Variables): CacheKey {
                return CacheKey.NO_KEY
            }
        }

        apolloClient = ApolloClient.builder()
            .serverUrl(GRAPHQL_ENDPOINT)
            .okHttpClient(okHttpClient)
            .normalizedCache(normalizedCacheFactory, cacheKeyResolver)
            .subscriptionTransportFactory(WebSocketSubscriptionTransport.Factory(GRAPHQL_WEBSOCKET_ENDPOINT, okHttpClient))
            .addCustomTypeAdapter(CustomType.TIMESTAMPTZ, dateCustomTypeAdapter)
            .build()
    }
}