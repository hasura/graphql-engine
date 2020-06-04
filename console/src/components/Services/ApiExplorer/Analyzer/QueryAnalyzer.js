import React from 'react';
import PropTypes from 'prop-types';
import Modal from 'react-modal';
import AnalyzeText from './AnalyzeText';

export default class QueryAnalyser extends React.Component {
  constructor() {
    super();
    Modal.setAppElement('body');
    this.state = {
      analyseData: [],
      activeNode: 0,
    };
  }

  componentDidMount() {
    this.props
      .analyzeFetcher(this.props.analyseQuery.query)
      .then(r => {
        if (r.ok) {
          return r.json();
        }
        return r.text().then(rText => Promise.reject(new Error(rText)));
      })
      .then(data => {
        this.setState({
          analyseData: Array.isArray(data) ? data : [data],
          activeNode: 0,
        });
      })
      .catch(e => {
        alert(`Unable to fetch: ${e.message}.`);
        this.props.clearAnalyse();
      });
  }
  render() {
    const { show, clearAnalyse } = this.props;
    return (
      <Modal
        className="modalWrapper"
        overlayClassName="myOverlayClass"
        isOpen={show && this.state.analyseData.length > 0}
      >
        <div className="modalHeader">
          <div className="modalTitle">Query Analysis</div>
          <div className="modalClose">
            <button onClick={clearAnalyse} className="form-control">
              x
            </button>
          </div>
        </div>
        <div className="modalBody">
          <div className="wd25">
            <div className="topLevelNodesWrapper">
              <div className="title">Top level nodes</div>
              <AnalyzeText
                data={this.state.analyseData}
                activeNode={this.state.activeNode}
                onClick={this.handleAnalyseNodeChange}
              />
            </div>
          </div>
          <div className="wd75">
            <div className="analysisWrapper">
              <div className="plansWrapper">
                <div className="plansTitle">Generated SQL</div>
                <div className="codeBlock">
                  <div className="copyGenerated">
                    <div className="copyTooltip">
                      <span className="tooltiptext" id="copySql">
                        Copy
                      </span>
                      <i
                        className={'fa fa-copy'}
                        onClick={this.copyToClip.bind(this, 'sql', 'copySql')}
                        onMouseLeave={this.resetCopy.bind(this, 'copySql')}
                      />
                    </div>
                  </div>
                  {window.hljs && window.sqlFormatter ? (
                    <pre>
                      <code
                        dangerouslySetInnerHTML={{
                          __html:
                            this.state.activeNode >= 0 &&
                            this.state.analyseData.length > 0 &&
                            window.hljs.highlight(
                              'sql',
                              window.sqlFormatter.format(
                                this.state.analyseData[this.state.activeNode]
                                  .sql,
                                { language: 'sql' }
                              )
                            ).value,
                        }}
                      />
                    </pre>
                  ) : (
                    <code>
                      {this.state.activeNode >= 0 &&
                      this.state.analyseData.length > 0
                        ? this.state.analyseData[this.state.activeNode].sql
                        : ''}
                    </code>
                  )}
                </div>
              </div>
              <div className="plansWrapper">
                <div className="plansTitle">Execution Plan</div>
                <div className="codeBlock">
                  <div className="copyGenerated">
                    <div className="copyTooltip">
                      <span className="tooltiptext" id="copyPlan">
                        Copy
                      </span>
                      <i
                        className={'fa fa-copy'}
                        onClick={this.copyToClip.bind(this, 'plan', 'copyPlan')}
                        onMouseLeave={this.resetCopy.bind(this, 'copyPlan')}
                      />
                    </div>
                  </div>
                  {/*
                  <pre>
                    <code>
                      {this.state.activeNode >= 0
                        && this.state.analyseData.length > 0
                        ? this.state.analyseData[
                            this.state.activeNode
                          ].plan.map((k, i) => <div key={ i }>{k}</div> )
                        : ''}
                    </code>
                  </pre>
                  */}
                  <pre>
                    <code>
                      {this.state.activeNode >= 0 &&
                      this.state.analyseData.length > 0
                        ? this.state.analyseData[
                            this.state.activeNode
                          ].plan.join('\n')
                        : ''}
                    </code>
                  </pre>
                </div>
              </div>
            </div>
          </div>
        </div>
      </Modal>
    );
  }
  /*
  fetchAnalyse() {
    const options = {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      credentials: 'include',
    };
    options.body = JSON.stringify(this.props.analyseQuery);
    return fetch('http://localhost:8080/v1alpha1/graphql/explain', options);
  }
  */

  handleAnalyseNodeChange = e => {
    const nodeKey = e.target.getAttribute('data-key');
    if (nodeKey) {
      this.setState({ activeNode: parseInt(nodeKey, 10) });
    }
  };
  copyToClip(type, id) {
    let text = '';
    if (this.state.analyseData.length > 0) {
      if (type === 'sql') {
        text = window.sqlFormatter
          ? window.sqlFormatter.format(
              this.state.analyseData[this.state.activeNode].sql,
              { language: 'sql' }
            )
          : this.state.analyseData[this.state.activeNode].sql;
      } else {
        text = this.state.analyseData[this.state.activeNode].plan.join('\n');
      }
    }
    const textArea = document.createElement('textarea');
    textArea.value = text;
    document.body.appendChild(textArea);
    textArea.focus();
    textArea.select();

    try {
      const successful = document.execCommand('copy');
      const tooltip = document.getElementById(id);
      tooltip.innerHTML = 'Copied';
      if (!successful) {
        throw new Error('Copy was unsuccessful');
      }
    } catch (err) {
      alert('Oops, unable to copy - ' + err);
    }
    document.body.removeChild(textArea);
  }
  resetCopy(id) {
    const tooltip = document.getElementById(id);
    tooltip.innerHTML = 'Copy';
  }
}

QueryAnalyser.propTypes = {
  show: PropTypes.bool.isRequired,
  analyseQuery: PropTypes.oneOfType([PropTypes.string, PropTypes.object]),
  clearAnalyse: PropTypes.func.isRequired,
  analyzeFetcher: PropTypes.func.isRequired,
};
