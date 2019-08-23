import * as React from 'react'
import { useState, Suspense } from 'react'
import { RouteComponentProps } from 'react-router-dom'
import styled from 'styled-components'
import Navbar from '../Navbar'
import UsersList from '../UsersList'
import CreateGroupButton from './CreateGroupButton'
import NewGroupNavbar from './NewGroupNavbar'

const Style = styled.div`
  .UsersList {
    height: calc(100% - 56px);
    overflow-y: overlay;
  }
`

export default ({ history }: RouteComponentProps) => {
  const [selectedUsers, setSelectedUsers] = useState([])

  return (
    <Style className="NewGroupScreen Screen">
      <Navbar>
        <NewGroupNavbar history={history} />
      </Navbar>
      <Suspense fallback={null}>
        <UsersList selectable onSelectionChange={setSelectedUsers} />
      </Suspense>

      {!!selectedUsers.length && <CreateGroupButton history={history} users={selectedUsers} />}
    </Style>
  )
}
