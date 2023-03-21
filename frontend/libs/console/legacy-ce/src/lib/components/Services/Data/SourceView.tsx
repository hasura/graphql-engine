import React, { useState } from 'react';
import { connect } from 'react-redux';
import Helmet from 'react-helmet';
import { FaDatabase, FaTrash } from 'react-icons/fa';
import { Analytics, REDACT_EVERYTHING } from '../../../features/Analytics';
import { Button } from '../../../new-components/Button';
import { Dispatch, ReduxState } from '../../../types';
import { mapDispatchToPropsEmpty } from '../../Common/utils/reactUtils';
import styles from '../../Common/Common.module.scss';
import { createNewSchema, deleteSchema } from './Schema/Actions';
import { updateCurrentSchema } from './DataActions';
import {
  getDataSourceBaseRoute,
  getSchemaPermissionsRoute,
} from '../../Common/utils/routesUtils';
import _push from './push';
import { isFeatureSupported } from '../../../dataSources';
import { isTemplateGalleryEnabled } from './Schema/TemplateGallery/templateGalleryConfig';
import BreadCrumb from '../../Common/Layout/BreadCrumb/BreadCrumb';
import TemplateGallery from './Schema/TemplateGallery/TemplateGallery';
import { useAppDispatch, useAppSelector } from '../../../storeHooks';

interface Props {
  dispatch: Dispatch;
  schemaList: string[];
  currentDataSource: string;
}

const AddSchema: React.VFC = () => {
  const dispatch = useAppDispatch();
  const currentDataSource = useAppSelector(
    state => state.tables.currentDataSource
  );
  const [isCreateActive, setIsCreateActive] = useState(false);
  const [createSchemaName, setCreateSchemaName] = useState('');

  const handleCreateSchema = () => {
    const schemaName = createSchemaName;
    const successCb = () =>
      dispatch(updateCurrentSchema(schemaName, currentDataSource));

    dispatch(createNewSchema(schemaName, successCb));
  };

  if (!isFeatureSupported('schemas.create.enabled')) {
    return null;
  }

  if (!isCreateActive) {
    return (
      <Button
        data-test="data-create-schema"
        mode="primary"
        size="md"
        className={`${styles.add_mar_left}, ${styles.display_flex}`}
        onClick={() => setIsCreateActive(true)}
      >
        Create Schema
      </Button>
    );
  }

  return (
    <div className={styles.display_inline} style={{ paddingLeft: '10px' }}>
      <div className={styles.display_inline}>
        <input
          type="text"
          placeholder="Enter Schema name"
          className={`form-control input-sm ${styles.display_inline}`}
          value={createSchemaName}
          onChange={(e: any) => {
            e.persist();
            setCreateSchemaName(e.target.value);
          }}
        />
      </div>
      <Button
        data-test="data-create-schema"
        mode="primary"
        size="md"
        className={styles.add_mar_left}
        onClick={handleCreateSchema}
      >
        Create Schema
      </Button>
      <Button
        color="white"
        size="md"
        className={styles.add_mar_left_mid}
        onClick={() => {
          setIsCreateActive(false);
          setCreateSchemaName('');
        }}
      >
        Cancel
      </Button>
    </div>
  );
};

const SourceView: React.FC<Props> = props => {
  const { currentDataSource, schemaList, dispatch } = props;

  const handleView = (schema: string) => {
    dispatch(updateCurrentSchema(schema, currentDataSource));
  };

  const handlePermissionsSummary = (schema: string) => {
    dispatch(_push(getSchemaPermissionsRoute(schema, currentDataSource)));
  };

  const handleDelete = (schema: string) => {
    const successCb = () => {
      dispatch(
        updateCurrentSchema('public', currentDataSource, false, schemaList)
      );
    };
    dispatch(deleteSchema(schema, successCb));
  };

  return (
    <Analytics name="SourceView" {...REDACT_EVERYTHING}>
      <div
        style={{ paddingTop: '20px', paddingLeft: '15px' }}
        className="bootstrap-jail"
      >
        <Helmet title="Source - Data | Hasura" />
        <div style={{ display: 'flex', flexDirection: 'column' }}>
          <div>
            <BreadCrumb
              breadCrumbs={[
                { url: `/data`, title: 'Data' },
                {
                  url: getDataSourceBaseRoute(currentDataSource),
                  title: currentDataSource,
                  prefix: <FaDatabase />,
                },
              ]}
            />
          </div>
          <div className={`${styles.display_flex}`}>
            <h2
              className={`${styles.headerText} ${styles.display_inline} ${styles.add_mar_right_mid}`}
            >
              {currentDataSource}
            </h2>
            <AddSchema />
          </div>
        </div>
        <div>
          <hr className="my-md" />
          <div id="schema-list-view" className="space-y-sm">
            {schemaList.length ? (
              schemaList.map((schema, key: number) => {
                return (
                  <div className="flex gap-3 py-1 items-center" key={key}>
                    <Button size="sm" onClick={() => handleView(schema)}>
                      View
                    </Button>
                    <Button
                      size="sm"
                      onClick={() => handlePermissionsSummary(schema)}
                    >
                      Permissions Summary
                    </Button>
                    {isFeatureSupported('schemas.delete.enabled') ? (
                      <Button size="sm" onClick={() => handleDelete(schema)}>
                        <FaTrash aria-hidden="true" />
                      </Button>
                    ) : null}
                    <div
                      key={key}
                      className={`${styles.display_inline} ${styles.padd_small_left}`}
                    >
                      {schema}
                    </div>
                  </div>
                );
              })
            ) : (
              <div>There are no schemas at the moment</div>
            )}
          </div>
          <hr className="my-md" />
          {isTemplateGalleryEnabled ? <TemplateGallery /> : null}
        </div>
      </div>
    </Analytics>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    schemaList: state.tables.schemaList,
    currentDataSource: state.tables.currentDataSource,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);
export default connector(SourceView);
