import React from 'react';
import { useQuery } from '@apollo/react-hooks';
import { Link } from 'react-router';
import { Dialog } from '@hasura/console-legacy-ce';
import { Button } from '@hasura/console-legacy-ce';
import CustomCopy from './CustomCopy';

import { transformedVals } from '../Usage/utils';

import { fetchQueryLogForOperation } from '../Usage/graphql.queries';

import {
  checkIfEmptyReturnAppropriateValue,
  createFilter,
} from '../Error/utils';

import { getActualIfAliased, transformToTwoPlaces } from '../Usage/utils';

import { SHOW_ONLY_ERRORS_SYMBOL, relativeModulePath } from '../constants';
import styles from '../Metrics.module.scss';

const Modal = props => {
  const { onHide, data: d, groupBys, RenderLink } = props;
  const {
    operation_id: id,
    operation_name: operationName,
    role,
    request_count: requestCount,
    average_execution_time: averageExecutionTime,
    average_response_size: averageResponseSize,
    error_count: errorCount,
  } = d;

  const getLinkToOperationsPage = (withError = false) => {
    /* Get the groupBys */
    if (groupBys.length > 0) {
      const filters = groupBys.map(n => {
        const getValue = () => {
          const valLen = d[getActualIfAliased(n)].length;
          /* Return appropriate
           * values so as to link to operations
           * with right filters applied
           * */
          if (valLen === 0) {
            return checkIfEmptyReturnAppropriateValue(n);
          }
          return d[getActualIfAliased(n)];
        };
        const val = getValue();
        return createFilter(n, val);
      });
      /* Show only errors on the operations page */
      if (withError) {
        filters.push({
          type: SHOW_ONLY_ERRORS_SYMBOL,
          value: SHOW_ONLY_ERRORS_SYMBOL,
        });
      }
      return {
        pathname: `${relativeModulePath}/operations`,
        search: `?filters=${window.encodeURI(JSON.stringify(filters))}`,
      };
    }
    return null;
  };

  const computeRoles = () => {
    return role ? role : 'N/A';
  };

  const computeAverageExecutionTime = () => {
    return transformedVals.average_execution_time(averageExecutionTime);
  };

  const computeAverageResponseSize = () => {
    return transformedVals.average_response_size(averageResponseSize);
  };

  const renderQueryIfExists = () => {
    if (id || operationName) {
      const vars = {};
      if (id) {
        vars.opId = id;
      }
      if (operationName) {
        vars.opName = operationName;
      }
      const { loading, error, data } = useQuery(fetchQueryLogForOperation, {
        variables: vars,
      });

      if (loading) {
        return 'Loading ...';
      }
      if (error) {
        return 'Failed to fetch query log';
      }
      if (data && data.operation_logs.length > 0) {
        const { query } = data.operation_logs[0];
        const renderOperationQuery = () => {
          const renderItem = [];
          if (query) {
            const { query: graphQLQuery, variables } = query;
            const queryElement = (
              <CustomCopy
                key="graphql-query"
                label={'OPERATION STRING'}
                copy={graphQLQuery}
              />
            );
            renderItem.push(queryElement);
            if (variables) {
              try {
                const formattedVar = JSON.stringify(variables, null, 2);
                const variablesElement = [
                  <CustomCopy
                    key="graphql-query-var"
                    label={'QUERY VARIABLES'}
                    copy={formattedVar}
                  />,
                ];
                renderItem.push(variablesElement);
              } catch (e) {
                //
              }
            }
            return renderItem;
          }
          return null;
        };
        return (
          <div className={styles.noPadd + ' col-md-6'}>
            {renderOperationQuery()}
          </div>
        );
      }
    }
  };

  const computeErrorRate = () => {
    try {
      const errorRate = errorCount / requestCount;
      return `${transformToTwoPlaces(errorRate * 100)} %`;
    } catch (e) {
      console.error(e);
      return 'N/A';
    }
  };

  const getRequestsLink = () => {
    const params = getLinkToOperationsPage();
    const indivRequestsBtn = <Button>Inspect individual requests</Button>;
    if (!RenderLink) {
      return <Link to={params}>{indivRequestsBtn}</Link>;
    }
    return (
      <RenderLink pageName="operations" search={params.search}>
        {indivRequestsBtn}
      </RenderLink>
    );
  };

  const getErrorLink = () => {
    const params = getLinkToOperationsPage(true);
    const indivErrorsBtn = <Button>Inspect errors</Button>;
    if (!RenderLink) {
      return <Link to={params}>{indivErrorsBtn}</Link>;
    }
    return (
      <RenderLink pageName="operations" search={params.search}>
        {indivErrorsBtn}
      </RenderLink>
    );
  };

  return (
    <Dialog
      hasBackdrop
      size="xxl"
      id="dateModal"
      onClose={onHide}
      title="Inspect"
      portal
    >
      <div className={styles.modalWrapper}>
        <div className={styles.modalContainer}>
          <div className={styles.noPadd + ' col-md-6 ' + styles.borderRight}>
            <div className={styles.infoWrapper}>
              <div className={styles.infoField}>
                <div className={styles.information}>
                  ID: <span>{id || 'N/A'}</span>
                  <span className={styles.rightAlign}>{getRequestsLink()}</span>
                </div>
                <div className={styles.information}>
                  OPERATION NAME: <span>{operationName || 'N/A'}</span>
                </div>
                <div className={styles.information}>
                  EXECUTED BY ROLES: <span>{computeRoles()}</span>
                </div>
                <div className={styles.information}>
                  REQUESTS: <span>{requestCount} Requests</span>
                </div>
                <div className={styles.information}>
                  AVERAGE EXECUTION TIME:{' '}
                  <span>{computeAverageExecutionTime()} ms</span>
                </div>
                <div className={styles.information}>
                  AVERAGE RESPONSE SIZE:{' '}
                  <span>{computeAverageResponseSize()} kB</span>
                </div>
                <div className={styles.information}>
                  ERROR RATE: <span>{computeErrorRate()}</span>
                  <span className={styles.addPaddLeft}>{getErrorLink()}</span>
                </div>
              </div>
            </div>
          </div>
          {renderQueryIfExists()}
          {/*
        <div className={styles.noPadd + ' col-md-6'}>
          {renderOperationQuery()}
          {renderGeneratedSql()}
        </div>
        */}
        </div>
      </div>
    </Dialog>
  );
};

export default Modal;
