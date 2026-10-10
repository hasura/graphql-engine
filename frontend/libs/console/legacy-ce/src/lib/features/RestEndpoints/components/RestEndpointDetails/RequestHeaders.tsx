import { FaPlus } from 'react-icons/fa';
import { Button, Collapsible, CardedTable, Checkbox } from '@hasura/shared/ui';

import { DataHeader } from '@hasura/shared/types';

type RequestHeadersProps = {
  headers: DataHeader[];
  setHeaders: (headers: DataHeader[]) => void;
};

export const RequestHeaders = (props: RequestHeadersProps) => {
  const { headers, setHeaders } = props;

  const showRemove = !!(
    headers.length > 1 ||
    headers?.[0]?.key ||
    headers?.[0]?.value
  );
  return (
    <Collapsible
      defaultOpen
      triggerChildren={
        <div className="font-semibold text-muted">Request Headers</div>
      }
    >
      <div className="relative">
        <div className="absolute top-0 right-0">
          <Button
            leftIcon={FaPlus}
            size="sm"
            onClick={() => {
              setHeaders([
                ...headers,
                { key: '', value: '', isDisabled: true, selected: true },
              ]);
            }}
          >
            Add Header
          </Button>
        </div>
        <div className="font-semibold text-muted mb-4">Headers List</div>
        <CardedTable
          columns={[
            <Checkbox
              key="select-all"
              value={headers.every((header) => header.isDisabled)}
              onChange={(checked) =>
                setHeaders(
                  headers.map((header) => ({
                    ...header,
                    selected: !!checked,
                  })),
                )
              }
            />,
            'Name',
            'Value',
          ]}
          data={headers.map((header, i) =>
            [
              <Checkbox
                key={`checkbox-${i}`}
                value={header.isDisabled}
                onChange={(checked) =>
                  setHeaders(
                    headers.map((h) => ({
                      ...h,
                      selected: h.key === header.key ? !!checked : h.isDisabled,
                    })),
                  )
                }
              />,
              <input
                key={`name-${i}`}
                data-testid={`header-name-${i}`}
                placeholder="Enter name..."
                className="w-full"
                value={header.key}
                onChange={(e) =>
                  setHeaders(
                    headers.map((h) => ({
                      ...h,
                      name: h.key === header.key ? e.target.value : h.key,
                    })),
                  )
                }
              />,
              <input
                key={`value-${i}`}
                data-testid={`header-value-${i}`}
                placeholder="Enter value..."
                className="w-full border-0"
                type={
                  header.key === 'x-hasura-admin-secret' ? 'password' : 'text'
                }
                value={header.value}
                onChange={(e) =>
                  setHeaders(
                    headers.map((h) => ({
                      ...h,
                      value: h.key === header.key ? e.target.value : h.value,
                    })),
                  )
                }
              />,
              showRemove && (
                <Button
                  mode="destructive"
                  size="sm"
                  onClick={() => {
                    const newHeaders = headers
                      .slice(0, i)
                      .concat(headers.slice(i + 1));

                    if (newHeaders.length === 0) {
                      newHeaders.push({
                        key: '',
                        value: '',
                        isDisabled: true,
                        selected: false,
                      });
                    }
                    setHeaders(newHeaders);
                  }}
                >
                  Remove
                </Button>
              ),
            ].filter(Boolean),
          )}
        />
      </div>
    </Collapsible>
  );
};
