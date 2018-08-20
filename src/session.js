import gql from 'graphql-tag';
import client from './apollo';

const newUUID = () => {
  const p8 = (s) => {
    let p = (Math.random().toString(16) + "000000000").substr(2 ,8);
    return s ? "-" + p.substr(0,4) + "-" + p.substr(4,4) : p ;
  };
  return p8() + p8(true) + p8(true) + p8();
};

const MUTATION_NEW_USER = gql`
mutation newUser($uuid: uuid) {
  insert_user (
    objects:[{
      id: $uuid
    }]
  ) {
    returning {
      id
      created_at
    }
  }
}
`;

const getUserId = () => {
  return new Promise((resolve, reject) => {
    let uid = window.localStorage.getItem('uid');
    if (!uid) {
      client.mutate({
        mutation: MUTATION_NEW_USER,
        variables: {
          uuid: newUUID(),
        }
      }).then(({data}) => {
        if (data.insert_user.returning.length > 0) {
          const user = data.insert_user.returning[0];
          window.localStorage.setItem('uid', user.id);
          window.localStorage.setItem('createdAt', user.created_at);
          resolve(user.id);
        } else {
          reject('no data');
        }
      }).catch((error) => {
        console.error(error);
        reject(error);
      });
    } else {
      resolve(uid);
    }
  });
};

export {
  getUserId,
  userId
};
