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
      let queryInput;
      let submitBtn;

      switch (openOption) {
        case 'manual':
          queryInput = (
            <div>
              <div className={styles.add_mar_bottom_mid}>
                <b>Query:</b>
              </div>
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
            </div>
          );

          submitBtn = (
            <Button size={'sm'} color={'yellow'}>
              Add To Whitelist
            </Button>
          );
          break;
        case 'upload':
          queryInput = (
            <div>
              <div className={styles.add_mar_bottom_mid}>
                <b>Query file:</b>
              </div>
              <input
                type="text"
                className={'form-control input-sm ' + styles.inline_block}
                placeholder={'https://xyz.com/query.graphql'}
              />
            </div>
          );

          submitBtn = (
            <Button size={'sm'} color={'yellow'}>
              Upload File To Whitelist
            </Button>
          );
          break;
        default:
          queryInput = '';
      }

      if (queryInput) {
        const nameInput = (
          <div>
            <div className={styles.add_mar_bottom_mid}>
              <b>Query name:</b>
            </div>
            <input
              type="text"
              className={'form-control input-sm ' + styles.inline_block}
              placeholder={'query_name'}
            />
          </div>
        );

        addSection = (
          <div className={styles.add_mar_top}>
            <div>{nameInput}</div>
            <div className={styles.add_mar_top}>{queryInput}</div>
            <div className={styles.add_mar_top}>{submitBtn}</div>
          </div>
        );
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
