import ApiExplorer from './ApiExplorer';

const generatedApiExplorer = connect => {
  const mapStateToProps = state => {
    return {
      ...state.apiexplorer,
      credentials: {},
      dataApiExplorerData: { ...state.dataApiExplorer },
      dataHeaders: state.tables.dataHeaders,
      tables: state.tables.allSchemas,
    };
  };
  return connect(mapStateToProps)(ApiExplorer);
};

export default generatedApiExplorer;
