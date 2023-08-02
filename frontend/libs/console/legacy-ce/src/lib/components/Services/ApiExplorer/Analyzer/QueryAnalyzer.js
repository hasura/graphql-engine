import React from 'react';
import PropTypes from 'prop-types';
import Modal from 'react-modal';
import sqlFormatter from 'sql-formatter';
import hljs from 'highlight.js';
import { Button } from '../../../../new-components/Button';
import RootFields from './RootFields';
import { ImCopy } from 'react-icons/im';

export default class QueryAnalyser extends React.Component {
  constructor() {
    super();
    Modal.setAppElement('body');
    this.state = {
      analyseData: [],
      activeNode: 0,
      previouslyCopied: null,
    };
  }

  copyToClip(type, text) {
    try {
      navigator.clipboard.writeText(text).then(() => {
        this.setState(
          {
            previouslyCopied: type,
          },
          () =>
            // This variable decides if a "Copied!" message should be shown.
            // Awaiting 2 seconds to highlight this for the user
            setTimeout(() => {
              this.setState({
                previouslyCopied: null,
              });
            }, 2000)
        );
      });
    } catch (error) {
      alert(`failed to copy`);
    }
  }

  componentDidMount() {
    const { dispatch, analyseQuery } = this.props;
    this.props
      .analyzeFetcher(analyseQuery.query, dispatch)
      .then(data => {
        // todo: unsure if this guard is necessary. Replaces previous guard that would silently return
        // this was previously necessary as the analyze fetcher would handle errors without throwing
        if (!data) {
          console.error(
            'Missing data from analyze result. This should never happen.'
          );
          throw new Error('Missing data from analyze result.');
        }
        this.setState({
          analyseData: Array.isArray(data) ? data : [data],
          activeNode: 0,
        });
      })
      .catch(e => {
        alert(`Unable to fetch: ${e?.message ?? ''}`);
        this.props.clearAnalyse();
      });
  }

  render() {
    const { show, clearAnalyse } = this.props;
    return (
      <Modal
        className="flex flex-col p-10 rounded-xl w-full z-[101]"
        overlayClassName="fixed top-0 left-0 right-0 bottom-6 bg-white bg-opacity-75 z-[101]"
        isOpen={show && this.state.analyseData.length > 0}
      >
        <div className="bg-[#43495a] border-b border-gray-200 py-sm px-md flex justify-between">
          <div className="font-xl font-bold text-[#ffc627]">Query Analysis</div>
          <div className="text-[#ccc] font-bold">
            <Button onClick={clearAnalyse}>X</Button>
          </div>
        </div>
        <div className="flex min-h-full bg-white border border-gray-500">
          <div className="w-1/4 border border-gray-500">
            <div className="h-8/12">
              <div className="p-md pr-0 font-bold text-lg bg-[#fff3d5] text-[#767e93] mb-md">
                Top level nodes
              </div>
              <RootFields
                data={this.state.analyseData}
                activeNode={this.state.activeNode}
                onClick={this.handleAnalyseNodeChange}
              />
            </div>
          </div>
          <div className="w-3/4 z-100 bg-white">
            <div className="w-full">
              <div className="p-md pt-0">
                <div className="text-[#767e93] font-bold py-sm">
                  Generated SQL
                </div>
                <div className="w-full overflow-y-scroll h-[calc(30vh)] mb-sm">
                  <Button
                    icon={<ImCopy />}
                    onClick={() =>
                      this.copyToClip(
                        'sql',
                        sqlFormatter.format(
                          this.state.analyseData[this.state.activeNode].sql,
                          { language: 'sql' }
                        )
                      )
                    }
                    className="absolute right-24 mt-2"
                  >
                    {this.state.previouslyCopied === 'sql' ? 'Copied!' : 'Copy'}
                  </Button>
                  <pre className="bg-[#fdf9ed]">
                    <code
                      dangerouslySetInnerHTML={{
                        __html:
                          this.state.activeNode >= 0 &&
                          this.state.analyseData.length > 0 &&
                          hljs.highlight(
                            'sql',
                            sqlFormatter.format(
                              this.state.analyseData[this.state.activeNode].sql,
                              { language: 'sql' }
                            )
                          ).value,
                      }}
                    />
                  </pre>
                </div>
              </div>
              <div className="p-md pt-0">
                <div>
                  <div className="text-[#767e93] font-bold py-md pt-0">
                    Execution Plan
                  </div>
                  <div className="w-full h-[calc(30vh)] overflow-y-scroll mb-sm">
                    <pre className="bg-[#fdf9ed]">
                      <Button
                        icon={<ImCopy />}
                        onClick={() =>
                          this.copyToClip(
                            'plan',
                            this.state.analyseData[
                              this.state.activeNode
                            ].plan.join('\n')
                          )
                        }
                        className="absolute right-24"
                      >
                        {this.state.previouslyCopied === 'plan'
                          ? 'Copied!'
                          : 'Copy'}
                      </Button>
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
        </div>
      </Modal>
    );
  }
  handleAnalyseNodeChange = e => {
    const nodeKey = e.target.getAttribute('data-key');
    if (nodeKey) {
      this.setState({ activeNode: parseInt(nodeKey, 10) });
    }
  };
}

QueryAnalyser.propTypes = {
  show: PropTypes.bool.isRequired,
  analyseQuery: PropTypes.oneOfType([PropTypes.string, PropTypes.object]),
  clearAnalyse: PropTypes.func.isRequired,
  analyzeFetcher: PropTypes.func.isRequired,
};
