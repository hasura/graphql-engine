import React from 'react';
import Button from '../../../Common/Button/Button';
import AceEditor from 'react-ace';
import styles from './AllowedQueries.scss';

import { addAllowedQuery } from '../Actions';

class AddAllowedQuery extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      openOption: null,
      newQuery: {},
    };
  }

  render() {
    const { dispatch, isFirstQuery } = this.props;
    const { openOption, newQuery } = this.state;

    const handleAddClick = option => {
      return () => {
        const newOpenOption = openOption === option ? null : option;

        this.setState({
          openOption: newOpenOption,
          newQuery: {},
        });
      };
    };

    const getOpenAddSection = () => {
      let addSection;
      let queryInput;
      let submitBtnTxt;

      const getManualQueryInput = () => {
        const handleQueryChange = val => {
          this.setState({
            newQuery: {
              ...newQuery,
              query: val,
            },
          });
        };

        return (
          <div>
            <div className={styles.add_mar_bottom_mid}>
              <b>Query:</b>
            </div>
            <AceEditor
              data-test="allowed_query_add"
              mode="graphql"
              theme="github"
              name="allowed_query_add"
              value={newQuery.query}
              minLines={8}
              maxLines={100}
              width="100%"
              showPrintMargin={false}
              onChange={handleQueryChange}
            />
          </div>
        );
      };

      const getUploadQueryInput = () => {
        const handleQueryFileChange = e => {
          this.setState({
            newQuery: {
              ...newQuery,
              queryFile: e.target.value,
            },
          });
        };

        return (
          <div>
            <div className={styles.add_mar_bottom_mid}>
              <b>Query file:</b>
            </div>
            <input
              type="text"
              className={'form-control input-sm ' + styles.inline_block}
              placeholder={'https://xyz.com/query.graphql'}
              value={newQuery.queryFile}
              onChange={handleQueryFileChange}
            />
          </div>
        );
      };

      switch (openOption) {
        case 'manual':
          queryInput = getManualQueryInput();
          submitBtnTxt = 'Add To Allow-List';
          break;
        case 'upload':
          queryInput = getUploadQueryInput();
          submitBtnTxt = 'Upload File To Allowed List';
          break;
        default:
          queryInput = '';
      }

      if (queryInput) {
        const getNameInput = () => {
          const handleNameChange = e => {
            this.setState({
              newQuery: {
                ...newQuery,
                name: e.target.value,
              },
            });
          };

          return (
            <div>
              <div className={styles.add_mar_bottom_mid}>
                <b>Query name:</b>
              </div>
              <input
                type="text"
                className={'form-control input-sm ' + styles.inline_block}
                placeholder={'query_name'}
                value={newQuery.name}
                onChange={handleNameChange}
              />
            </div>
          );
        };

        const getSubmitBtn = () => {
          const handleSubmit = () => {
            dispatch(addAllowedQuery(newQuery, isFirstQuery));
          };

          return (
            <Button size={'sm'} color={'yellow'} onClick={handleSubmit}>
              {submitBtnTxt}
            </Button>
          );
        };

        addSection = (
          <div key={openOption} className={styles.add_mar_top}>
            <div>{getNameInput()}</div>
            <div className={styles.add_mar_top}>{queryInput}</div>
            <div className={styles.add_mar_top}>{getSubmitBtn()}</div>
          </div>
        );
      }
      return addSection;
    };

    return (
      <div>
        <h4 className={styles.subheading_text}>
          Add a new query to allow-list
        </h4>
        <div className={styles.subsection}>
          <Button onClick={handleAddClick('manual')}>Add query</Button>
          {/*<Button onClick={handleAddClick('manual')}>Add query manually</Button>*/}
          {/*<span>&nbsp;&nbsp;OR&nbsp;&nbsp;</span>*/}
          {/*<Button onClick={handleAddClick('upload')}>Upload query</Button>*/}

          <div>{getOpenAddSection()}</div>
        </div>
      </div>
    );
  }
}

export default AddAllowedQuery;
