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

  .NewGroupNavbar-title {
    line-height: 56px;
  }

  .NewGroupNavbar-back-button {
    color: var(--primary-text);
  }
`

interface NewGroupNavbarProps {
  history: History
}

export default ({ history }: NewGroupNavbarProps) => {
  const navToChats = () => {
    history.push('/new-chat')
  }

  return (
    <Style className="NewGroupNavbar">
      <Button className="NewGroupNavbar-back-button" onClick={navToChats}>
        <ArrowBackIcon />
      </Button>
      <div className="NewGroupNavbar-title">New Chat Group</div>
    </Style>
  )
}
