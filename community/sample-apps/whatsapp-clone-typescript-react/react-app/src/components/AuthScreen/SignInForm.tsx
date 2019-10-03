import Button from '@material-ui/core/Button'
import TextField from '@material-ui/core/TextField'
import { History } from 'history'
import * as React from 'react'
import { useState } from 'react'
import { signIn } from '../../services/auth.service'

interface SignInFormProps {
  history: History
}

export default ({ history }: SignInFormProps) => {
  const [username, setUsername] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState('')

  const onUsernameChange = ({ target }) => {
    setError('')
    setUsername(target.value)
  }

  const onPasswordChange = ({ target }) => {
    setError('')
    setPassword(target.value)
  }

  const maySignIn = () => {
    return !!(username && password)
  }

  const handleSignIn = () => {
    signIn({ username, password })
      .then(() => {
        history.push('/chats')
      })
      .catch(error => {
        setError(error.message || error)
      })
  }

  const handleSignUp = () => {
    history.push('/sign-up')
  }

  return (
    <div className="SignInForm Screen">
      <form>
        <legend>Sign in</legend>
        <div style={{ width: '100%' }}>
          <TextField
            className="AuthScreen-text-field"
            label="Username"
            value={username}
            onChange={onUsernameChange}
            margin="normal"
            placeholder="Enter your username"
          />
          <TextField
            className="AuthScreen-text-field"
            label="Password"
            type="password"
            value={password}
            onChange={onPasswordChange}
            margin="normal"
            placeholder="Enter your password"
          />
        </div>
        <Button
          type="button"
          color="secondary"
          variant="contained"
          disabled={!maySignIn()}
          onClick={handleSignIn}
        >
          Sign in
        </Button>
        <div className="AuthScreen-error">{error}</div>
        <span className="AuthScreen-alternative">
          Don't have an account yet? <a onClick={handleSignUp}>Sign up!</a>
        </span>
      </form>
    </div>
  )
}
