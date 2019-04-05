import { Component } from 'react'
import fetch from 'isomorphic-unfetch'
import Layout from '../components/layout'
import { signup } from '../utils/auth'

class Signup extends Component {
  static getInitialProps ({ req }) {
    const protocol = process.env.NODE_ENV === 'production' ? 'https' : 'http'

    const apiUrl = process.browser
      ? `${protocol}://${window.location.host}/signup`
      : `${protocol}://${req.headers.host}/signup`

    return { apiUrl }
  }

  constructor (props) {
    super(props)

    this.state = { username: '', password: '', confirmPassword: '', error: '' }
    this.handleChangeUsername = this.handleChangeUsername.bind(this)
    this.handleChangePassword = this.handleChangePassword.bind(this)
    this.handleChangeConfirmPassword = this.handleChangeConfirmPassword.bind(this)
    this.handleSubmit = this.handleSubmit.bind(this)
  }

  handleChangeUsername (event) {
    this.setState({ username: event.target.value })
  }

  handleChangePassword (event) {
    this.setState({ password: event.target.value })
  }

  handleChangeConfirmPassword (event) {
    this.setState({ confirmPassword: event.target.value })
  }

  async handleSubmit (event) {
    event.preventDefault()
    this.setState({ error: '' })
    const username = this.state.username
    const password = this.state.password
    const confirmPassword = this.state.confirmPassword
    const url = this.props.apiUrl

    try {
      const response = await fetch(url, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ username, password, confirmPassword })
      })
      if (response.ok) {
        const { token } = await response.json()
        signup({ token })
      } else {
        console.log('Login failed.')
        // https://github.com/developit/unfetch#caveats
        let error = new Error(response.statusText)
        error.response = response
        throw error
      }
    } catch (error) {
      console.error(
        'You have an error in your code or there are Network issues.',
        error
      )
      this.setState({ error: error.message })
    }
  }

  render () {
    return (
      <Layout>
        <div className='login'>
          <form onSubmit={this.handleSubmit}>
            <label htmlFor='username'>Enter username</label>

            <input
              type='text'
              id='username'
              name='username'
              value={this.state.username}
              onChange={this.handleChangeUsername}
            />
            <label htmlFor='username'>Enter Password</label>
            <input
              type='text'
              id='password'
              name='password'
              value={this.state.password}
              onChange={this.handleChangePassword}
            />
            <label htmlFor='username'>Confirm Password</label>
            <input
              type='text'
              id='confirmPassword'
              name='confirmPassword'
              value={this.state.confirmPassword}
              onChange={this.handleChangeConfirmPassword}
            />

            <button type='submit'>Signup</button>

            <p className={`error ${this.state.error && 'show'}`}>
              {this.state.error && `Error: ${this.state.error}`}
            </p>
          </form>
        </div>
        <style jsx>{`
          .login {
            max-width: 340px;
            margin: 0 auto;
            padding: 1rem;
            border: 1px solid #ccc;
            border-radius: 4px;
          }

          form {
            display: flex;
            flex-flow: column;
          }

          label {
            font-weight: 600;
          }

          input {
            padding: 8px;
            margin: 0.3rem 0 1rem;
            border: 1px solid #ccc;
            border-radius: 4px;
          }

          .error {
            margin: 0.5rem 0 0;
            display: none;
            color: brown;
          }

          .error.show {
            display: block;
          }
        `}</style>
      </Layout>
    )
  }
}

export default Signup
