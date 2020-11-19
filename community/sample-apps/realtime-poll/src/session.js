import gql from 'graphql-tag';
import client from './apollo';
import {
  MUTATION_MARK_USER_ONLINE,
  MUTATION_NEW_USER,
} from './GraphQL';

const newUUID = () => {
  const p8 = (s) => {
    let p = (Math.random().toString(16) + "000000000").substr(2, 8);
    return s ? "-" + p.substr(0, 4) + "-" + p.substr(4, 4) : p;
  };
  return p8() + p8(true) + p8(true) + p8();
};


const getUserId = () => {
  return new Promise((resolve, reject) => {
    // let uid = window.localStorage.getItem('uid');
    // if (!uid) {
    client.mutate({
      mutation: gql`${MUTATION_NEW_USER}`,
      variables: {
        uuid: newUUID(),
      }
    }).then(({ data }) => {
      if (data.insert_user.returning.length > 0) {
        const user = data.insert_user.returning[0];
        console.log("getUserId user.id:", user.id);
        // window.localStorage.setItem('uid', user.id);
        // window.localStorage.setItem('createdAt', user.created_at);
        reportUserOnline(user.id);
        resolve(user.id);
      } else {
        reject('no data');
      }
    }).catch((error) => {
      console.error(error);
      reject(error);
    });
    // } else {
    //   reportUserOnline(uid);
    //   resolve(uid);
    // }
  });
};

const reportUserOnline = (userId) => {
  window.setInterval(() => {
    client.mutate({
      mutation: gql`${MUTATION_MARK_USER_ONLINE}`,
      variables: {
        uuid: userId,
      },
    });
  }, 10000);
};

export {
  getUserId
};
