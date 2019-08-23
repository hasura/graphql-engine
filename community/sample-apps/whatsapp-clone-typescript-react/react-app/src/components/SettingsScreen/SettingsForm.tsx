import TextField from '@material-ui/core/TextField'
import EditIcon from '@material-ui/icons/Edit'
import { defaultDataIdFromObject } from 'apollo-cache-inmemory'
import gql from 'graphql-tag'
import * as React from 'react'
import { useEffect, useState } from 'react'
import { useMutation } from 'react-apollo-hooks'
import { RouteComponentProps } from 'react-router-dom'
import styled from 'styled-components'
import * as fragments from '../../graphql/fragments'
import { SettingsFormMutation, User } from '../../graphql/types'
import { useMe } from '../../services/auth.service'
import { pickPicture, uploadProfilePicture } from '../../services/picture.service'
import Navbar from '../Navbar'
import SettingsNavbar from './SettingsNavbar'

const Style = styled.div`
  .SettingsForm-picture {
    max-width: 300px;
    display: block;
    margin: auto;
    margin-top: 50px;

    img {
      object-fit: cover;
      border-radius: 50%;
      margin-bottom: -34px;
      width: 300px;
      height: 300px;
    }

    svg {
      float: right;
      font-size: 30px;
      opacity: 0.5;
      border-left: black solid 1px;
      padding-left: 5px;
      cursor: pointer;
    }
  }

  .SettingsForm-name-input {
    display: block;
    margin: auto;
    width: calc(100% - 50px);
    margin-top: 50px;

    > div {
      width: 100%;
    }
  }
`

const mutation = gql`
  mutation SettingsFormMutation($name: String, $picture: String, $userId: Int) {
    update_users(_set: {name: $name, picture: $picture}, where: {id: {_eq: $userId}}) {
      affected_rows
      returning {
        id
        name
        picture
        username
      }
    }
  }
`

export default ({ history }: RouteComponentProps) => {
  const me = useMe()
  const [myName, setMyName] = useState(me.name)
  const [myPicture, setMyPicture] = useState(me.picture)

  const updateUser = useMutation<SettingsFormMutation.Mutation, SettingsFormMutation.Variables>(
    mutation,
    {
      variables: { name: myName, picture: myPicture, userId: me.id },
      update: (client, { data: { update_users } }) => {
        me.picture = myPicture
        me.name = myPicture

        client.writeFragment<User.Fragment>({
          id: defaultDataIdFromObject(me),
          fragment: fragments.user,
          data: me,
        })
      },
    },
  )

  useEffect(
    () => {
      if (myPicture !== me.picture) {
        updateUser()
      }
    },
    [myPicture],
  )

  const updateName = ({ target }) => {
    setMyName(target.value)
  }

  const updatePicture = async () => {
    const file = await pickPicture()

    if (!file) return

    const { secure_url } = await uploadProfilePicture(file)

    setMyPicture(secure_url)
  }

  return (
    <Style className={name}>
      <div className="SettingsForm-picture">
        <img src={myPicture || '/assets/default-profile-pic.jpg'} />
        <EditIcon onClick={updatePicture} />
      </div>
      <TextField
        className="SettingsForm-name-input"
        label="Name"
        value={myName}
        onChange={updateName}
        onBlur={updateUser}
        margin="normal"
        placeholder="Enter your name"
      />
    </Style>
  )
}
