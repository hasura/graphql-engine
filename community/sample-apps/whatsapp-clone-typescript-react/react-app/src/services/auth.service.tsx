import * as React from 'react'
import { useContext } from 'react'
import { useQuery } from 'react-apollo-hooks'
import { Redirect } from 'react-router-dom'
import store from '../apollo-client'
import * as queries from '../graphql/queries'
import { Me } from '../graphql/types'
import { useSubscriptions } from './cache.service'

const MyContext = React.createContext(null)

export const useMe = () => {
  return useContext(MyContext)
}

export const withAuth = (Component: React.ComponentType) => {
  return props => {
    if (!getAuthHeader()) return <Redirect to="/sign-in" />

    // Validating against server
    const fetchUser = useQuery<Me.Query, Me.Variables>(queries.me, { suspend: true, context: { headers: {'x-hasura-role': 'mine'}} })
    const myResult = fetchUser.data.users ? fetchUser.data.users[0] : {};

    useSubscriptions(myResult)

    return (
      <MyContext.Provider value={myResult}>
        <Component {...props} />
      </MyContext.Provider>
    )
  }
}

export const storeAuthHeader = (auth: string) => {
  localStorage.setItem('Authorization', 'Bearer '+auth)
}

export const getAuthHeader = (): string | null => {
  return localStorage.getItem('Authorization') || null
}

export const signIn = ({ username, password }) => {

  return fetch(`${process.env.REACT_APP_AUTH_URL}/login`, {
    method: 'POST',
    body: JSON.stringify({ username, password }),
    headers: {
      'Content-Type': 'application/json'
    },
  })
  .then(res => {
    if (res.status < 400) {
      return res.json().then((data) => {
        const token = data.token;
        storeAuthHeader(token);
      });
    } else {
      return Promise.reject(res.statusText)
    }
  })
}

export const signUp = ({ username, password, name }) => {
  return fetch(`${process.env.REACT_APP_AUTH_URL}/signup`, {
    method: 'POST',
    body: JSON.stringify({ name, username, password, confirmPassword: password }),
    headers: {
      Accept: 'application/json',
      'Content-Type': 'application/json'
    },
  })
}

export const signOut = () => {
  localStorage.removeItem('Authorization')
  // window.location.href = '/sign-in'

  return store.clearStore()
}

export default {
  useMe,
  withAuth,
  storeAuthHeader,
  getAuthHeader,
  signIn,
  signUp,
  signOut,
}
