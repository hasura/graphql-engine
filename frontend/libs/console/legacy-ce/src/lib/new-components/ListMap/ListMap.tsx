import React, { useState } from 'react';
import { RiCloseCircleFill } from 'react-icons/ri';
import { FaPlusCircle } from 'react-icons/fa';
import { useFormContext } from 'react-hook-form';
import { Button } from '../Button';
import clsx from 'clsx';
import { BsArrowRight } from 'react-icons/bs';
import { Select } from './Select';

interface Props {
  from: {
    options: string[];
    label?: string;
    icon?: React.ReactElement | React.ReactElement[];
    placeholder?: string;
  };
  to: {
    type: 'array' | 'string'; // we can add boolean and other types later on if there is a need
    options?: string[];
    label?: string;
    icon?: React.ReactElement | React.ReactElement[];
    placeholder?: string;
  };
  value?: Record<string, string>;
  onChange?: (e: Record<string, string>) => void;
  name: string;
  className?: string;
  existingRelationshipName?: string;
}

const getIcon = (
  icon: React.ReactElement | React.ReactElement[] | undefined
) => {
  if (!icon) {
    return undefined;
  }

  try {
    if ((icon as React.ReactElement[])?.length)
      return (icon as React.ReactElement[]).map((el, i) =>
        React.cloneElement(el, {
          className: clsx('mr-1.5', el.props.className),
          key: i,
        })
      );

    return React.cloneElement(icon as React.ReactElement, {
      className: clsx('mr-1.5', (icon as React.ReactElement).props.className),
    });
  } catch (err) {
    return null;
  }
};

const initLocalMaps = (values: Record<string, string>) => {
  if (Object.entries(values).length)
    return [
      ...Object.entries(values).map(([from, to]) => ({
        from,
        to,
      })),
    ];
  return [{ from: '', to: '' }];
};

export const ListMap = (props: Props) => {
  const {
    from: source,
    to: target,
    onChange,
    name,
    className,
    existingRelationshipName,
  } = props;

  const formContext = useFormContext();

  const mapping = formContext.watch(name);
  const initValue = React.useMemo(
    () => props.value ?? mapping ?? {},
    [props, mapping]
  );

  const [localMaps, setLocalMaps] = useState<{ from: string; to: string }[]>(
    initLocalMaps(initValue)
  );

  const updateLocalMaps = (items: { from: string; to: string }[]) => {
    setLocalMaps(items);

    if (onChange)
      onChange(
        items
          .filter(item => item.from && item.to)
          .reduce(
            (resultMap, { from, to }) => ({ ...resultMap, [from]: to }),
            {}
          )
      );

    formContext?.setValue(
      name,
      items
        .filter(item => item.from && item.to)
        .reduce((resultMap, { from, to }) => ({ ...resultMap, [from]: to }), {})
    );
  };

  React.useEffect(() => {
    if (existingRelationshipName) {
      setLocalMaps(initLocalMaps(initValue));
    }
  }, [existingRelationshipName, initValue]);

  return (
    <div
      id="reference"
      className={clsx(
        `rounded bg-gray-50 border border-gray-300 p-md mb-md`,
        className
      )}
    >
      <div className="grid grid-cols-12 gap-3 mb-xs text-muted font-semibold">
        <div className="col-span-5 flex items-center">
          {getIcon(source.icon)}
          {source.label}
        </div>
        <div className="col-span-1 text-center" />
        <div className="col-span-5 flex items-center">
          {getIcon(target.icon)}
          {target.label}
        </div>
        <div className="col-span-1 text-center" />
      </div>
      {localMaps.map(({ from, to }, i) => {
        return (
          <div
            className="grid grid-cols-12 gap-3 mb-xs text-muted font-semibold"
            key={i}
          >
            <div className="col-span-5 flex items-center">
              <Select
                options={[
                  ...source.options.filter(
                    op => !localMaps.map(x => x.from).includes(op)
                  ),
                  from,
                ].filter(Boolean)}
                value={from}
                onChange={e => {
                  updateLocalMaps(
                    localMaps.map((item, j) => {
                      if (i !== j) return item;
                      return { ...item, from: e.target.value };
                    })
                  );
                }}
                placeholder={source.placeholder ?? 'Enter Source value'}
                data-testid={`${name}_source_input_${i}`}
              />
            </div>
            <div className="col-span-1 flex items-center justify-center">
              <BsArrowRight />
            </div>
            <div className="col-span-5 flex items-center">
              {target.type === 'array' ? (
                <Select
                  options={target.options ?? []}
                  value={to}
                  onChange={e => {
                    updateLocalMaps(
                      localMaps.map((item, j) => {
                        if (i !== j) return item;
                        return { ...item, to: e.target.value };
                      })
                    );
                  }}
                  placeholder={target.placeholder ?? 'Enter Target value'}
                  data-testid={`${name}_target_input_${i}`}
                />
              ) : (
                <input
                  type="text"
                  className="block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
                  value={to}
                  onChange={e => {
                    updateLocalMaps(
                      localMaps.map((item, j) => {
                        if (i !== j) return item;
                        return { ...item, to: e.target.value };
                      })
                    );
                  }}
                  placeholder={target.placeholder ?? 'Enter Target value'}
                  data-testid={`${name}_target_input_${i}`}
                />
              )}
            </div>
            <div className="col-span-1 flex items-center justify-center">
              <RiCloseCircleFill
                onClick={() => {
                  updateLocalMaps([...localMaps.filter((_, j) => j !== i)]);
                }}
                className="cursor-pointer"
              />
            </div>
          </div>
        );
      })}
      <Button
        onClick={() => {
          updateLocalMaps([...localMaps, { from: '', to: '' }]);
        }}
        data-testid={`${name}_add_new_row`}
        icon={<FaPlusCircle />}
      >
        Add New Row
      </Button>
    </div>
  );
};
