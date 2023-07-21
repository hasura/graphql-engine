import { supportedNumericTypes } from '../../../../components/Services/ApiExplorer/Rest/utils';
import { CardedTable } from '../../../../new-components/CardedTable';
import { Collapsible } from '../../../../new-components/Collapsible';
import { Variable } from './RestEndpointDetails';

type VariablesProps = {
  variables: Variable[];
  setVariables: (variables: Variable[]) => void;
};

export const Variables = (props: VariablesProps) => {
  const { variables, setVariables } = props;

  if (variables.length === 0) {
    return null;
  }

  return (
    <Collapsible
      defaultOpen
      triggerChildren={
        <div className="font-semibold text-muted">Request Variables</div>
      }
    >
      <div className="relative">
        <div className="absolute top-0 right-0"></div>
        <div className="font-semibold text-muted mb-4">Variables List</div>
        <CardedTable
          showActionCell
          columns={['Name', 'Type', 'Value']}
          data={variables.map((variable, i) => [
            <span className="font-semibold text-muted">{variable.name}</span>,
            variable.type,
            <input
              type={
                supportedNumericTypes.includes(variable.type)
                  ? 'number'
                  : 'text'
              }
              data-testid={`variable-${variable.name}`}
              placeholder="Enter value..."
              className="w-full font-normal text-muted border-0"
              value={variable.value}
              onChange={e =>
                setVariables(
                  variables.map(v => ({
                    ...v,
                    value: v.name === variable.name ? e.target.value : v.value,
                  }))
                )
              }
            />,
          ])}
        />
      </div>
    </Collapsible>
  );
};
