import { AsyncStorage } from 'react-native';

const handleError = async (error, navigate) => {
  const errObj = JSON.parse(JSON.stringify(error));
  if (
    errObj &&
    errObj.networkError &&
    errObj.networkError.result &&
    errObj.networkError.result.errors &&
    errObj.networkError.result.errors[0] &&
    errObj.networkError.result.errors[0].code === 'invalid-jwt'
  ) {
    console.log(errObj).networkError.result;
    await AsyncStorage.removeItem('@todo-graphql:session');
    navigate('Loading');
  }
  console.log(errObj);
};

export default handleError;