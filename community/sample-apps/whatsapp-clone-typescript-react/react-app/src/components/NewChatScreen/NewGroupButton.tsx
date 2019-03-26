import Button from '@material-ui/core/Button'
import GroupAddIcon from '@material-ui/icons/GroupAdd'
import { History } from 'history'
import * as React from 'react'
import styled from 'styled-components'

const Style = styled.div`
  display: flex;

  button {
    border-radius: 0;
    text-transform: none;
    font-size: inherit;
    width: 100%;
    justify-content: flex-start;
    padding-left: 15px;
    padding-right: 15px;

    svg {
      font-size: 30px;
      margin-top: 10px;
    }
  }

  .NewGroupButton-icon {
    height: 50px;
    width: 50px;
    object-fit: cover;
    border-radius: 50%;
    color: white;
    background-color: var(--secondary-bg);
  }

  .NewGroupButton-title {
    padding-left: 15px;
    font-weight: bold;
  }
`

interface NewGroupButtonProps {
  history: History
}

export default ({ history }: NewGroupButtonProps) => {
  const navToGroup = () => {
    history.push('/new-chat/group')
  }

  return (
    <Style>
      <Button onClick={navToGroup}>
        <div className="NewGroupButton-icon">
          <GroupAddIcon />
        </div>
        <div className="NewGroupButton-title">New Group</div>
      </Button>
    </Style>
  )
}
