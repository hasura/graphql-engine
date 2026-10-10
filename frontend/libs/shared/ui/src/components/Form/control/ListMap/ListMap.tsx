import React, { useState } from 'react';
import { RiCloseCircleFill } from 'react-icons/ri';
import { FaPlusCircle } from 'react-icons/fa';
import { useFormContext } from 'react-hook-form';
import { Button } from '../../../Button';
import clsx from 'clsx';
import { BsArrowRight } from 'react-icons/bs';
import { Input, Select } from '../../base';
import { Card } from '../../../Card';
import { Flex, Text } from '@radix-ui/themes';

interface Props {
  from: {
    options: string[];
    label?: string;
    icon?: React.ReactElement<any> | React.ReactElement<any>[];
    placeholder?: string;
  };
  to: {
    type: 'array' | 'string'; // we can add boolean and other types later on if there is a need
    options?: string[];
    label?: string;
    icon?: React.ReactElement<any> | React.ReactElement<any>[];
    placeholder?: string;
  };
  value?: Record<string, string>;
  onChange?: (e: Record<string, string>) => void;
  name: string;
  className?: string;
  existingRelationshipName?: string;
}

const getIcon = (
  icon: React.ReactElement<any> | React.ReactElement<any>[] | undefined,
) => {
  if (!icon) {
    return undefined;
  }

  try {
    if ((icon as React.ReactElement<any>[])?.length)
      return (icon as React.ReactElement<any>[]).map((el, i) =>
        React.cloneElement(el, {
          className: clsx('mr-1.5', el.props.className),
          key: i,
        }),
      );

    return React.cloneElement(icon as React.ReactElement<any>, {
      className: clsx(
        'mr-1.5',
        (icon as React.ReactElement<any>).props.className,
      ),
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
    [props, mapping],
  );

  const [localMaps, setLocalMaps] = useState<{ from: string; to: string }[]>(
    initLocalMaps(initValue),
  );

  const updateLocalMaps = (items: { from: string; to: string }[]) => {
    setLocalMaps(items);

    if (onChange)
      onChange(
        items
          .filter((item) => item.from && item.to)
          .reduce(
            (resultMap, { from, to }) => ({ ...resultMap, [from]: to }),
            {},
          ),
      );

    formContext?.setValue(
      name,
      items
        .filter((item) => item.from && item.to)
        .reduce(
          (resultMap, { from, to }) => ({ ...resultMap, [from]: to }),
          {},
        ),
    );
  };

  React.useEffect(() => {
    if (existingRelationshipName) {
      setLocalMaps(initLocalMaps(initValue));
    }
  }, [existingRelationshipName, initValue]);

  return (
    <Card id="reference" className={clsx(`mb-4`, className)}>
      <div className="grid grid-cols-12 gap-3 mb-1">
        <Flex align="center" gap="2" className="col-span-5">
          {getIcon(source.icon)}
          <Text weight="bold">{source.label}</Text>
        </Flex>
        <div className="col-span-1 text-center" />
        <Flex align="center" gap="2" className="col-span-5">
          {getIcon(target.icon)}
          {target.label}
        </Flex>
        <div className="col-span-1 text-center" />
      </div>
      {localMaps.map(({ from, to }, i) => {
        return (
          <div className="grid grid-cols-12 gap-3 mb-1" key={i}>
            <div className="col-span-5">
              <Select
                options={[
                  ...source.options.filter(
                    (op) => !localMaps.map((x) => x.from).includes(op),
                  ),
                  from,
                ]
                  .filter(Boolean)
                  .map((option) => ({
                    label: option,
                    value: option,
                  }))}
                value={from}
                onChange={(value) => {
                  updateLocalMaps(
                    localMaps.map((item, j) => {
                      if (i !== j) return item;
                      return { ...item, from: value };
                    }),
                  );
                }}
                placeholder={source.placeholder ?? 'Enter Source value'}
                data-testid={`${name}_source_input_${i}`}
              />
            </div>
            <div className="col-span-1">
              <BsArrowRight />
            </div>
            <div className="col-span-5">
              {target.type === 'array' ? (
                <Select
                  options={
                    target.options?.map((option) => ({
                      label: option,
                      value: option,
                    })) ?? []
                  }
                  value={to}
                  onChange={(value) => {
                    updateLocalMaps(
                      localMaps.map((item, j) => {
                        if (i !== j) return item;
                        return { ...item, to: value };
                      }),
                    );
                  }}
                  placeholder={target.placeholder ?? 'Enter Target value'}
                  data-testid={`${name}_target_input_${i}`}
                />
              ) : (
                <Input
                  type="text"
                  value={to}
                  onChange={(e) => {
                    updateLocalMaps(
                      localMaps.map((item, j) => {
                        if (i !== j) return item;
                        return { ...item, to: e.target.value };
                      }),
                    );
                  }}
                  placeholder={target.placeholder ?? 'Enter Target value'}
                  data-testid={`${name}_target_input_${i}`}
                />
              )}
            </div>
            <div className="col-span-1">
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
      <div className="mt-4">
        <Button
          onClick={() => {
            updateLocalMaps([...localMaps, { from: '', to: '' }]);
          }}
          data-testid={`${name}_add_new_row`}
          leftIcon={FaPlusCircle}
          mode="default"
          size="1"
        >
          Add New Row
        </Button>
      </div>
    </Card>
  );
};
