import List from '@material-ui/core/List'
import ListItem from '@material-ui/core/ListItem'
import CheckCircle from '@material-ui/icons/CheckCircle'
import gql from 'graphql-tag'
import * as React from 'react'
import { useState } from 'react'
import { useQuery } from 'react-apollo-hooks'
import { useSubscription } from '../polyfills/react-apollo-hooks'
import styled from 'styled-components'
import * as fragments from '../graphql/fragments'
import { ExistingChatUsers, RemainingUsers, RemainingUsersSub } from '../graphql/types'
import { useMe } from '../services/auth.service'

const Style = styled.div`
  .UsersList-users-list {
    padding: 0;
  }

  .UsersList-user-item {
    position: relative;
    padding: 7.5px 15px;
    display: flex;
    ${props => props.selectable && 'cursor: pointer;'}
  }

  .UsersList-profile-pic {
    height: 50px;
    width: 50px;
    object-fit: cover;
    border-radius: 50%;
  }

  .UsersList-name {
    padding-left: 15px;
    font-weight: bold;
  }

  .UsersList-checkmark {
    position: absolute;
    left: 50px;
    top: 35px;
    color: var(--secondary-bg);
    background-color: white;
    border-radius: 50%;
  }
`

const existingUsersQuery = gql`
  query ExistingChatUsers($userId: Int){
    chat(where:{users:{user_id:{_eq:$userId}}, owner_id:{_is_null:true}}){
      id
      name
      owner_id
      users(order_by:[{user_id:desc}],where:{user_id:{_neq:$userId}}) {
        user_id
        user {
          ...user
        }
      }
    }
  }
  ${fragments.user}
`;

const remainingUsersQuery = gql`
  query RemainingUsers($existingUsersId: [Int!]) {
    users(order_by:[{id:desc}],where:{id:{_nin:$existingUsersId}}){
      ...user
    }
  }
  ${fragments.user}
`;

const remainingUsersSubscription = gql`
  subscription RemainingUsersSub($existingUsersId: [Int!]) {
    users(order_by:[{id:desc}],where:{id:{_nin:$existingUsersId}}){
      ...user
    }
  }
  ${fragments.user}
`;

interface UsersListProps {
  selectable?: boolean
  onSelectionChange?: (users: any[]) => void
  // onUserPick?: (user: User.Fragment) => void
  onUserPick?: (user: any[]) => void
}

export default (props: UsersListProps) => {
  const { selectable, onSelectionChange, onUserPick } = {
    selectable: false,
    onSelectionChange: () => {},
    onUserPick: () => {},
    ...props,
  }

  const [selectedUsers, setSelectedUsers] = useState([])
  const me = useMe()
  const currentUserId = me.id;

  const { data: { chat } } = useQuery<ExistingChatUsers.Query, ExistingChatUsers.Variables>(
    existingUsersQuery,
    {variables: {userId: me.id}, suspend: true}
  );
  let existingChatUsers
  let existingUserIds
  if(chat) {
    existingChatUsers = chat.map((chat) => {
      return({...chat.users[0].user, isExisting: true, chat_id: chat.id})
    })
    existingUserIds = chat.map((chat) => {
      return(chat.users[0].user_id)
    })
  }
  if(existingUserIds) {
    existingUserIds.push(currentUserId);
  }
  useSubscription<RemainingUsersSub.Subscription, RemainingUsersSub.Variables>(
    remainingUsersSubscription, { variables: {existingUsersId: existingUserIds},
    // onSubscriptionData: ({ client, subscriptionData: { userUpdated } }) => {
    onSubscriptionData: ({ client, subscriptionData: { users } }) => {
      let queryUsers
      try {
        queryUsers = client.readQuery<RemainingUsers.Query, RemainingUsers.Variables>({
          query: remainingUsersQuery,
          variables: {existingUsersId: existingUserIds}
        }).users
      } catch (e) {
        console.error(e);
      }
      if(queryUsers && users && users.length && !queryUsers.some(_user => _user.id === users[0].id)) {
        const newUserList = queryUsers.unshift(users[0])
        client.writeQuery<RemainingUsers.Query, RemainingUsers.Variables>({
          query: remainingUsersQuery,
          variables: {existingUsersId: existingUserIds},
          data: { users: queryUsers },
        })
      }
    }
  })

  const { data: { users } } = useQuery<RemainingUsers.Query, RemainingUsers.Variables>(
    remainingUsersQuery,
    {variables: {existingUsersId: existingUserIds}, suspend: true}
  );

  let remainingUsers;
  if(users) {
    remainingUsers = users.map((user) => {
      return({...user, isExisting: false })
    });
  }
  let finalUsers
  if(existingChatUsers && remainingUsers) {
    finalUsers = existingChatUsers.concat(remainingUsers)
  }

  const onListItemClick = user => {
    if (!selectable) {
      return onUserPick(user)
    }
    if (selectedUsers.includes(user)) {
      const index = selectedUsers.indexOf(user)
      selectedUsers.splice(index, 1)
    } else {
      selectedUsers.push(user)
    }

    setSelectedUsers(selectedUsers)
    onSelectionChange(selectedUsers)
  }

  return (
    <Style className="UsersList" selectable={selectable}>
      <List className="UsersList-users-list">
        {finalUsers.map(user => {
          const isSelectedUser = selectedUsers.some((el) => { 
            return el.id === user.id
          });
          return(
          <ListItem
            className="UsersList-user-item"
            key={user.id}
            button
            onClick={onListItemClick.bind(null, user)}
          >
            <img
              className="UsersList-profile-pic"
              src={user.picture || '/assets/default-profile-pic.jpg'}
            />
            <div className="UsersList-name">{user.name}</div>

            {isSelectedUser && <CheckCircle className="UsersList-checkmark" />}
          </ListItem>
        )})}
      </List>
    </Style>
  )
}
