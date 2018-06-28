import ApiExplorer from './ApiExplorer';

const generatedApiExplorer = connect => {
  const mapStateToProps = state => {
    return {
      ...state.apiexplorer,
      credentials: {},
      dataApiExplorerData: { ...state.dataApiExplorer },
    };
  };
  return connect(mapStateToProps)(ApiExplorer);
};

export default generatedApiExplorer;
