import React from 'react';
import PropTypes from 'prop-types';
import QueryAnalyzer from './QueryAnalyzer';
import GraphiQL from 'graphiql';
import { print, parse } from 'graphql';
import { isValidGraphQLOperation } from '../utils';
import { getGraphQLQueryPayload } from '../../../Common/utils/graphqlUtils';

export default class AnalyzeButton extends React.Component {
  constructor(props) {
    super(props);
    // Ensure props are correct
    if (typeof props.analyzeFetcher !== 'function') {
      throw new TypeError('GraphiQL requires a analyzeFetcher function.');
    }
    this.state = {
      optionsOpen: false,
      highlight: null,
      isAnalysing: false,
      analyseQuery: '',
    };
  }
  render() {
    const { operations, mode } = this.props;
    const optionsOpen = this.state.optionsOpen;
    const hasOptions = operations && operations.length > 1;

    let options = null;
    if (hasOptions && optionsOpen) {
      const highlight = this.state.highlight;
      const validOperations = operations.filter(isValidGraphQLOperation);
      if (validOperations.length) {
        options = (
          <ul className="execute-options">
            {validOperations.map(operation => {
              return (
                <li
                  key={operation.name ? operation.name.value : '*'}
                  className={operation === highlight ? 'selected' : null}
                  onMouseOver={() => this.setState({ highlight: operation })}
                  onFocus={() => this.setState({ highlight: operation })}
                  onMouseOut={() => this.setState({ highlight: null })}
                  onBlur={() => this.setState({ highlight: null })}
                  onMouseUp={() => this._onOptionSelected(operation)}
                >
                  {operation.name ? operation.name.value : '<Unnamed>'}
                </li>
              );
            })}
          </ul>
        );
      }
    }
    let onMouseDown;
    if (!this.state.isAnalysing && hasOptions && !optionsOpen) {
      onMouseDown = this._onOptionsOpen;
    }

    return (
      <span className="analyse-button-wrap">
        <GraphiQL.Button
          onClick={this.handleAnalyseClick.bind(this)}
          onMouseDown={onMouseDown}
          title="Analyze Query"
          label={this.state.isAnalysing ? 'Analyzing' : 'Analyze'}
        />
        {options}
        {this.state.analyseQuery && (
          <QueryAnalyzer
            show={this.state.isAnalysing}
            mode={mode}
            analyseQuery={this.state.analyseQuery}
            clearAnalyse={this.clearAnalyse.bind(this)}
            dispatch={this.props.dispatch}
            {...this.props}
          />
        )}
      </span>
    );
  }
  handleAnalyseClick = () => {
    try {
      const parsedQuery = this.props.query ? parse(this.props.query) : '';
      if (!parsedQuery) {
        // Don't do anything and return
        throw new Error('No valid query');
      }
    } catch (e) {
      throw new Error(`Error analysing query: ${e.message}`);
    }
    if (this.props.operations && this.props.operations.length > 1) {
      this.setState({ optionsOpen: !this.state.optionsOpen });
      return;
    }
    if (this.props.operations.length === 1) {
      // Handle analyse click
      this.onRun(
        (this.props.operations &&
          this.props.operations[0].name &&
          this.props.operations[0].name.value) ||
          ''
      );
    }
  };
  clearAnalyse = () => {
    this.setState({ isAnalysing: false, analyseQuery: '' });
  };
  onRun = operation => {
    let parseQuery = null;
    try {
      parseQuery = parse(this.props.query);
    } catch (e) {
      throw new Error(`Error analysing query: ${e.message}.`);
    }

    let jsonVariables = null;
    try {
      jsonVariables =
        this.props.variables && this.props.variables.trim() !== ''
          ? JSON.parse(this.props.variables)
          : null;
    } catch (e) {
      throw new Error(`Variables are invalid JSON: ${e.message}.`);
    }

    if (typeof jsonVariables !== 'object') {
      throw new Error('Variables are not a JSON object.');
    }

    const plainQuery = print(parseQuery);
    const query = getGraphQLQueryPayload(plainQuery, jsonVariables);
    if (operation) {
      query.operationName = operation;
    }
    const analyseQuery = {
      query,
      is_relay: this.props.mode === 'relay',
    };
    this.setState({
      analyseQuery,
      isAnalysing: true,
      optionsOpen: false,
      highlight: null,
    });
  };
  _onOptionSelected = operation => {
    this.setState({ optionsOpen: false });
    this.onRun(operation.name && operation.name.value);
  };

  _onOptionsOpen = downEvent => {
    let initialPress = true;
    const downTarget = downEvent.target;
    this.setState({ highlight: null, optionsOpen: true });

    let onMouseUp = upEvent => {
      if (initialPress && upEvent.target === downTarget) {
        initialPress = false;
      } else {
        document.removeEventListener('mouseup', onMouseUp);
        onMouseUp = null;
        const isOptionsMenuClicked =
          downTarget.parentNode.compareDocumentPosition(upEvent.target) &
          Node.DOCUMENT_POSITION_CONTAINED_BY;
        if (!isOptionsMenuClicked) {
          // menu calls setState if it was clicked
          this.setState({ optionsOpen: false });
        }
      }
    };

    document.addEventListener('mouseup', onMouseUp);
  };
}

AnalyzeButton.propTypes = {
  operations: PropTypes.array,
  query: PropTypes.string,
  variables: PropTypes.string,
  analyzeFetcher: PropTypes.func.isRequired,
};
