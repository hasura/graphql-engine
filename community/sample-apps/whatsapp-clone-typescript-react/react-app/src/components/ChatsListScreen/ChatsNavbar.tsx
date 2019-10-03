import Button from '@material-ui/core/Button'
import List from '@material-ui/core/List'
import ListItem from '@material-ui/core/ListItem'
import Popover from '@material-ui/core/Popover'
import MoreIcon from '@material-ui/icons/MoreVert'
import SignOutIcon from '@material-ui/icons/PowerSettingsNew'
import SettingsIcon from '@material-ui/icons/Settings'
import { History } from 'history'
import * as React from 'react'
import { useState } from 'react'
import styled from 'styled-components'
import { signOut } from '../../services/auth.service'

const Style = styled.div`
  padding: 0;
  display: flex;
  flex-direction: row;

  .ChatsNavbar-title {
    line-height: 56px;
  }

  .ChatsNavbar-options-btn {
    float: right;
    height: 100%;
    font-size: 1.2em;
    margin-right: -15px;
    color: var(--primary-text);
  }

  .ChatsNavbar-rest {
    flex: 1;
    justify-content: flex-end;
  }

  .ChatsNavbar-options-item svg {
    margin-right: 10px;
  }
`

interface ChatsNavbarProps {
  history: History
}

export default ({ history }: ChatsNavbarProps) => {
  const [popped, setPopped] = useState(false)

  const navToSettings = () => {
    setPopped(false)
    history.push('/settings')
  }

  const handleSignOut = () => {
    setPopped(false)
    signOut()

    history.push('/sign-in')
  }

  return (
    <Style className="ChatsNavbar">
      <span className="ChatsNavbar-title">WhatsApp Clone</span>
      <div className="ChatsNavbar-rest">
        <Button className="ChatsNavbar-options-btn" onClick={setPopped.bind(null, true)}>
          <MoreIcon />
        </Button>
      </div>
      <Popover
        open={popped}
        onClose={setPopped.bind(null, false)}
        anchorOrigin={{
          vertical: 'top',
          horizontal: 'right',
        }}
        transformOrigin={{
          vertical: 'top',
          horizontal: 'right',
        }}
      >
        <Style>
          <List>
            <ListItem className="ChatsNavbar-options-item" button onClick={navToSettings}>
              <SettingsIcon />
              Settings
            </ListItem>
            <ListItem className="ChatsNavbar-options-item" button onClick={handleSignOut}>
              <SignOutIcon />
              Sign Out
            </ListItem>
          </List>
        </Style>
      </Popover>
    </Style>
  )
}
