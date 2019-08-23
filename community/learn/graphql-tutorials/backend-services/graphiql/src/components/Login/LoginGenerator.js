import LoginComponent from './Login';

const generatedLoginComponent = connect => {
  const mapStateToProps = state => {
    return {
      ...state.apiexplorer,
    };
  };
  return connect(mapStateToProps)(LoginComponent);
};

export default generatedLoginComponent;
