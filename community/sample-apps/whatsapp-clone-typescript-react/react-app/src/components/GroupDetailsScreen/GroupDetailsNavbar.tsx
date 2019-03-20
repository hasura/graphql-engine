import Button from '@material-ui/core/Button'
import ArrowBackIcon from '@material-ui/icons/ArrowBack'
import { History } from 'history'
import * as React from 'react'
import styled from 'styled-components'

const Style = styled.div`
  padding: 0;
  display: flex;
  flex-direction: row;
  margin-left: -20px;

  .GroupDetailsNavbar-title {
    line-height: 56px;
  }

  .GroupDetailsNavbar-back-button {
    color: var(--primary-text);
  }
`

interface GroupDetailsNavbarProps {
  history: History
  chatId?: string
}

export default ({ history, chatId }: GroupDetailsNavbarProps) => {
  const navToNewGroup = () => {
    if (chatId) {
      history.push(`/chats/${chatId}`)
    } else {
      history.push('/new-chat/group')
    }
  }

  return (
    <Style className="GroupDetailsNavbar">
      <Button className="GroupDetailsNavbar-back-button" onClick={navToNewGroup}>
        <ArrowBackIcon />
      </Button>
      <div className="GroupDetailsNavbar-title">Group Details</div>
    </Style>
  )
}
