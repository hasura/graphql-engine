import { DataProxy } from 'apollo-cache'
import { OperationVariables, FetchPolicy } from 'apollo-client'
import { DocumentNode, GraphQLError } from 'graphql'
import { useEffect, useMemo, useRef, useState } from 'react'
import { useApolloClient } from 'react-apollo-hooks'
import * as isEqual from 'react-fast-compare'

export type SubscriptionOptions<T, TVariables> = {
  variables?: TVariables
  fetchPolicy?: FetchPolicy
  onSubscriptionData?: (options?: { client?: DataProxy; subscriptionData?: T }) => any
}

export const useSubscription = <T, TVariables = OperationVariables>(
  query: DocumentNode,
  options: SubscriptionOptions<T, TVariables> = {},
): {
  data: T | { [key: string]: void }
  error?: GraphQLError
  loading: boolean
} => {
  const onSubscriptionData = options.onSubscriptionData
  const prevOptions = useRef<typeof options | null>(null)
  const client = useApolloClient()
  const [data, setData] = useState<T | {}>({})
  const [error, setError] = useState<GraphQLError | null>(null)
  const [loading, setLoading] = useState<boolean>(true)

  const subscriptionOptions = {
    query,
    variables: options.variables,
    fetchPolicy: options.fetchPolicy,
  }

  useEffect(
    () => {
      prevOptions.current = subscriptionOptions
      const subscription = client
        .subscribe<{ data: T }, TVariables>(subscriptionOptions)
        .subscribe({
          next: ({ data }) => {
            setData(data)

            if (onSubscriptionData) {
              onSubscriptionData({ client, subscriptionData: data })
            }
          },
          error: err => {
            setError(err)
            setLoading(false)
          },
          complete: () => {
            setLoading(false)
          },
        })

      return () => {
        subscription.unsubscribe()
      }
    },
    [isEqual(prevOptions.current, subscriptionOptions) ? prevOptions.current : subscriptionOptions],
  )

  return useMemo(
    () => ({
      data,
      error,
      loading,
    }),
    [data, error, loading],
  )
}
