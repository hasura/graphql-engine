import Router from 'next/router'
import fetch from 'isomorphic-unfetch'
import nextCookie from 'next-cookies'
import Layout from '../components/layout'
import ArticleList from '../components/ArticleList'
import { withAuthSync } from '../utils/auth'

const Articles = props => {
  return (
    <Layout>
      <ArticleList />
    </Layout>
  )
}

export default withAuthSync(Articles)
