import { FaPlus } from 'react-icons/fa';
import { Button } from '../../../../new-components/Button';
import { Collapsible } from '../../../../new-components/Collapsible';
import { Header } from './RestEndpointDetails';
import { CardedTable } from '../../../../new-components/CardedTable';
import { Checkbox } from '../../../../new-components/Form';

type RequestHeadersProps = {
  headers: Header[];
  setHeaders: (headers: Header[]) => void;
};

export const RequestHeaders = (props: RequestHeadersProps) => {
  const { headers, setHeaders } = props;

  const showRemove = !!(
    headers.length > 1 ||
    headers?.[0]?.name ||
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
            icon={<FaPlus />}
            size="sm"
            onClick={() => {
              setHeaders([...headers, { name: '', value: '', selected: true }]);
            }}
          >
            Add Header
          </Button>
        </div>
        <div className="font-semibold text-muted mb-4">Headers List</div>
        <CardedTable
          showActionCell={showRemove}
          columns={[
            <Checkbox
              checked={headers.every(header => header.selected)}
              onCheckedChange={checked =>
                setHeaders(
                  headers.map(header => ({
                    ...header,
                    selected: !!checked,
                  }))
                )
              }
            />,
            'Name',
            'Value',
          ]}
          data={headers.map((header, i) =>
            [
              <Checkbox
                checked={header.selected}
                onCheckedChange={checked =>
                  setHeaders(
                    headers.map(h => ({
                      ...h,
                      selected: h.name === header.name ? !!checked : h.selected,
                    }))
                  )
                }
              />,
              <input
                data-testid={`header-name-${i}`}
                placeholder="Enter name..."
                className="w-full"
                value={header.name}
                onChange={e =>
                  setHeaders(
                    headers.map(h => ({
                      ...h,
                      name: h.name === header.name ? e.target.value : h.name,
                    }))
                  )
                }
              />,
              <input
                data-testid={`header-value-${i}`}
                placeholder="Enter value..."
                className="w-full border-0"
                type={
                  header.name === 'x-hasura-admin-secret' ? 'password' : 'text'
                }
                value={header.value}
                onChange={e =>
                  setHeaders(
                    headers.map(h => ({
                      ...h,
                      value: h.name === header.name ? e.target.value : h.value,
                    }))
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
                        name: '',
                        value: '',
                        selected: true,
                      });
                    }
                    setHeaders(newHeaders);
                  }}
                >
                  Remove
                </Button>
              ),
            ].filter(Boolean)
          )}
        />
      </div>
    </Collapsible>
  );
};
