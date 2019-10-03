import Button from '@material-ui/core/Button'
import AddIcon from '@material-ui/icons/Add'
import { History } from 'history'
import * as React from 'react'
import styled from 'styled-components'
import { User } from '../../graphql/types'

const Style = styled.div`
  position: fixed;
  right: 10px;
  bottom: 10px;

  button {
    min-width: 50px;
    width: 50px;
    height: 50px;
    border-radius: 999px;
    background-color: var(--secondary-bg);
    color: white;
  }
`

interface CreateGroupButtonProps {
  history: History
  users: User.Fragment[]
  // users: any[]
}

export default ({ history, users }: CreateGroupButtonProps) => {
  const onClick = () => {
    history.push('/new-chat/group/details', {
      users,
    })
  }

  return (
    <Style className="CreateGroupButton">
      <Button variant="contained" color="secondary" onClick={onClick}>
        <AddIcon />
      </Button>
    </Style>
  )
}
