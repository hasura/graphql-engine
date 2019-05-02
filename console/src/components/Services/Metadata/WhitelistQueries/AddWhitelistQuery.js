import React from 'react';
import Button from '../../../Common/Button/Button';
import AceEditor from 'react-ace';

class AddWhitelistQuery extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      openOption: null,
    };
  }

  render() {
    const { openOption } = this.state;

    const styles = require('./WhitelistQueries.scss');

    const handleManualClick = () => {
      const newOpenOption = openOption === 'manual' ? null : 'manual';
      this.setState({ openOption: newOpenOption });
    };

    const handleUploadClick = () => {
      const newOpenOption = openOption === 'upload' ? null : 'upload';
      this.setState({ openOption: newOpenOption });
    };

    const getOpenAddSection = () => {
      let addSection;

      switch (openOption) {
        case 'manual':
          addSection = (
            <div>
              <AceEditor
                data-test="whitelist_query_add"
                mode="graphql"
                theme="github"
                name="whitelist_query_add"
                value={''}
                minLines={8}
                maxLines={100}
                width="100%"
                showPrintMargin={false}
                onChange={() => {}}
              />
              <Button
                className={styles.add_mar_top_small}
                size={'sm'}
                color={'yellow'}
              >
                Add To Whitelist
              </Button>
            </div>
          );
          break;
        case 'upload':
          addSection = (
            <div>
              <input
                type="text"
                className={'form-control input-sm ' + styles.inline_block}
                placeholder={'Enter GraphQL file url'}
              />
              <Button
                className={styles.add_mar_top_small}
                size={'sm'}
                color={'yellow'}
              >
                Upload File To Whitelist
              </Button>
            </div>
          );
          break;
        default:
          addSection = '';
      }

      if (addSection) {
        addSection = <div className={styles.add_mar_top}>{addSection}</div>;
      }
      return addSection;
    };

    return (
      <div>
        <h4 className={styles.subheading_text}>Add a new query to whitelist</h4>
        <div className={styles.subsection}>
          <Button onClick={handleManualClick}>Add query manually</Button>
          <span>&nbsp;&nbsp;OR&nbsp;&nbsp;</span>
          <Button onClick={handleUploadClick}>Upload query</Button>
          <div>{getOpenAddSection()}</div>
        </div>
      </div>
    );
  }
}

export default AddWhitelistQuery;
