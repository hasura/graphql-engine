import { useState } from 'react';
import { FaEye, FaQuestionCircle, FaTimes, FaUserSecret } from 'react-icons/fa';
import {
  getHeadersSectionIsOpen,
  setHeadersSectionIsOpen,
  parseAuthHeader,
} from './utils';
import {
  ADMIN_SECRET_HEADER_KEY,
  HASURA_CLIENT_NAME,
  HASURA_COLLABORATOR_TOKEN,
} from '@hasura/shared/types';
import type { DataHeader } from '@hasura/shared/types';
import {
  Checkbox,
  CheckboxProps,
  Collapsible,
  Spinner,
  Table,
  Text,
  Tooltip,
} from '@hasura/shared/ui';
import { ServerConfig } from '@hasura/metadata/api';
import { Flex } from '@radix-ui/themes';
import { useAppContext } from '@hasura/shared/context';
import clsx from 'clsx';

/* When the page is loaded for the first time, hydrate the header state from the localStorage
 * Keep syncing the localStorage state when user modifies.
 * */

const ActionIcon = ({ message, dataHeaderID }) => (
  <Tooltip side="left" content={message}>
    <FaQuestionCircle
      className="cursor-pointer mr-2"
      data-header-id={dataHeaderID}
      aria-hidden="true"
    />
  </Tooltip>
);

const tableCellInputClassNames =
  'w-full border-0 outline-none focus:ring-0 focus:outline-none';
const tableHeaderCellClassNames = 'uppercase tracking-wider';

type Props = {
  headers: DataHeader[];
  serverConfig: ServerConfig | undefined;
  handleHeaderFocus: () => void;
  handleHeaderUnfocus: () => void;
  removeRequestHeader: (index: number) => void;
  changeRequestHeader: (header: DataHeader, index: number) => void;
  analyzeBearerToken: (
    token: string | null | undefined,
    dataHeaderIndex: number,
  ) => void;
  analyzingToken: {
    isAnalyzing: boolean;
    headerRow: number;
  };
};

const HeaderTable = ({
  headers,
  handleHeaderFocus,
  handleHeaderUnfocus,
  removeRequestHeader,
  changeRequestHeader,
  analyzingToken,
  analyzeBearerToken,
  serverConfig,
}: Props) => {
  const { envVars } = useAppContext();
  const [adminSecretVisible, setAdminSecretVisible] = useState(false);
  const [headersSectionIsOpen, setHeadersSectionIsOpenState] = useState(
    getHeadersSectionIsOpen(),
  );

  const isJWTSet = serverConfig?.is_jwt_set ?? false;

  const onDeleteHeaderClicked = (index: number) => () => {
    removeRequestHeader(index);
  };

  const onHeaderKeyChanged =
    (index: number): React.ChangeEventHandler<HTMLInputElement> =>
    (e) => {
      const newKey = e.target.value;
      const item = headers[index];
      if (item) {
        if (item.key === newKey) {
          return;
        }

        return changeRequestHeader(
          {
            ...item,
            key: newKey,
          },
          index,
        );
      }

      changeRequestHeader(
        {
          key: newKey,
          value: '',
          selected: true,
          isDisabled: false,
        },
        index,
      );
    };

  const onHeaderValueChanged =
    (index: number): React.ChangeEventHandler<HTMLInputElement> =>
    (e) => {
      const newValue = e.target.value;

      const item = headers[index];
      if (item) {
        if (item.value === newValue) {
          return;
        }

        return changeRequestHeader(
          {
            ...item,
            value: newValue,
          },
          index,
        );
      }

      changeRequestHeader(
        {
          key: '',
          value: newValue,
          selected: true,
          isDisabled: false,
        },
        index,
      );
    };

  const onIsActiveChanged =
    (index: number): CheckboxProps['onChange'] =>
    (value) => {
      const selected = value === true;
      const item = headers[index];
      if (item) {
        if (item.selected === selected) {
          return;
        }

        return changeRequestHeader(
          {
            ...item,
            selected,
          },
          index,
        );
      }

      changeRequestHeader(
        {
          selected,
          key: '',
          value: '',
          isDisabled: false,
        },
        index,
      );
    };

  const onShowAdminSecretClicked = () => {
    setAdminSecretVisible(!adminSecretVisible);
  };

  const getHeaderRows = () => {
    return headers.map((header, i) => {
      const isNewHeader = i === headers.length - 1;
      const isAdminSecret =
        header.key.toLowerCase() === ADMIN_SECRET_HEADER_KEY;
      const consoleId = envVars.consoleId;
      const isClientName =
        header.key.toLowerCase() === HASURA_CLIENT_NAME && consoleId;

      const isCollaboratorToken =
        header.key.toLowerCase() === HASURA_COLLABORATOR_TOKEN && consoleId;

      const getHeaderActiveCheckBox = () => {
        if (!isNewHeader) {
          return (
            <Table.Cell>
              <Flex align="center" justify="center">
                <Checkbox
                  name="sponsored"
                  value={header.selected}
                  data-header-id={i}
                  onChange={onIsActiveChanged(i)}
                  data-element-name="selected"
                />
              </Flex>
            </Table.Cell>
          );
        }

        return <Table.Cell />;
      };

      const getColSpan = () => {
        return isNewHeader ? 2 : 1;
      };

      const getHeaderKey = () => {
        return (
          <Table.Cell>
            <input
              className={tableCellInputClassNames}
              value={header.key || ''}
              disabled={header.isDisabled === true}
              data-header-id={i}
              placeholder="Enter Key"
              name="key"
              data-element-name="key"
              onChange={onHeaderKeyChanged(i)}
              onFocus={handleHeaderFocus}
              onBlur={handleHeaderUnfocus}
              type="text"
              data-test={`header-key-${i}`}
              autoComplete="off"
            />
          </Table.Cell>
        );
      };

      const getHeaderValue = () => {
        const type = isAdminSecret && !adminSecretVisible ? 'password' : 'text';

        return (
          <Table.Cell colSpan={getColSpan()}>
            <input
              className={tableCellInputClassNames}
              value={header.value || ''}
              disabled={header.isDisabled === true}
              data-header-id={i}
              placeholder="Enter Value"
              name="value"
              data-element-name="value"
              onChange={onHeaderValueChanged(i)}
              onFocus={handleHeaderFocus}
              onBlur={handleHeaderUnfocus}
              data-test={`header-value-${i}`}
              type={type}
              autoComplete="off"
            />
          </Table.Cell>
        );
      };

      const getHeaderAdminVal = () => {
        if (isAdminSecret) {
          return (
            <Tooltip side="left" content="Show admin secret">
              <FaEye
                className="cursor-pointer mr-2"
                data-header-id={i}
                aria-hidden="true"
                onClick={onShowAdminSecretClicked}
              />
            </Tooltip>
          );
        }

        return null;
      };

      const getJWTInspectorIcon = () => {
        // Check whether key is Authorization and value starts with Bearer
        const { isAuthHeader, token } = parseAuthHeader(header);

        const getAnalyzeIcon = () => {
          if (analyzingToken.isAnalyzing && analyzingToken.headerRow === i) {
            return <Spinner className="mr-2" />;
          }

          return (
            <FaUserSecret
              className="cursor-pointer mr-2"
              data-header-index={i}
              onClick={() => analyzeBearerToken(token, i)}
            />
          );
        };

        if (isAuthHeader && isJWTSet) {
          return (
            <Tooltip content="Decode JWT" side="left">
              {getAnalyzeIcon()}
            </Tooltip>
          );
        }

        return null;
      };

      const getHeaderActions = () => {
        if (i >= headers.length - 1) {
          return null;
        }

        return (
          <Table.Cell>
            <Flex justify="end" align="center" gap="2">
              {getHeaderAdminVal()}
              {getJWTInspectorIcon()}
              {isClientName && (
                <ActionIcon
                  message="Hasura client name is a header that indicates where the request is being made from. This is used by GraphQL Engine for providing detailed metrics."
                  dataHeaderID={i}
                />
              )}
              {isCollaboratorToken && (
                <ActionIcon
                  message="Hasura collaborator token is an admin-secret alternative when you login using Hasura. This is used by GraphQL Engine to authorise your requests."
                  dataHeaderID={i}
                />
              )}
              {!isAdminSecret && (
                <FaTimes
                  className="cursor-pointer mr-4"
                  data-header-id={i}
                  aria-hidden="true"
                  onClick={onDeleteHeaderClicked(i)}
                />
              )}
            </Flex>
          </Table.Cell>
        );
      };

      return (
        <Table.Row key={i}>
          {getHeaderActiveCheckBox()}
          {getHeaderKey()}
          {getHeaderValue()}
          {getHeaderActions()}
        </Table.Row>
      );
    });
  };

  const toggleHandler = () => {
    const newIsOpen = !headersSectionIsOpen;

    setHeadersSectionIsOpen(newIsOpen);
    setHeadersSectionIsOpenState(newIsOpen);
  };

  return (
    <Collapsible
      triggerChildren={<Text weight="bold">Request Headers</Text>}
      open={headersSectionIsOpen}
      onOpenChange={toggleHandler}
      triggerClassName="mb-2"
      disableContentStyles
    >
      <Table.Root className="min-w-full divide-y" variant="surface" size="1">
        <Table.Header>
          <Table.Row>
            <Table.RowHeaderCell
              className={clsx(tableHeaderCellClassNames, 'w-16 align-center')}
            >
              <Text weight="bold" size="1">
                Enable
              </Text>
            </Table.RowHeaderCell>
            <Table.RowHeaderCell className={tableHeaderCellClassNames}>
              <Text weight="bold" size="1">
                Key
              </Text>
            </Table.RowHeaderCell>
            <Table.RowHeaderCell className={tableHeaderCellClassNames}>
              <Text weight="bold" size="1">
                Value
              </Text>
            </Table.RowHeaderCell>
            <Table.RowHeaderCell className={tableHeaderCellClassNames} />
          </Table.Row>
        </Table.Header>
        <Table.Body>{getHeaderRows()}</Table.Body>
      </Table.Root>
    </Collapsible>
  );
};

export default HeaderTable;
