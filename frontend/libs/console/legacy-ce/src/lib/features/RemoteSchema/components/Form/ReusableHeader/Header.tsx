import { FaTimes } from 'react-icons/fa';
import { DropdownButton, DropdownMenu, Input } from '@hasura/shared/ui';
import styles from './Header.module.scss';
import type { UseRemoteSchemaHeaderForm } from '../../../hooks/useRemoteSchemaHeaderForm';

type Props = {
  className?: string;
  form: UseRemoteSchemaHeaderForm;
  isDisabled?: boolean;
  typeOptions: any[];
  placeHolderText?: (text: string) => string;
  keyInputPlaceholder?: string;
};

const Header = ({
  className,
  form,
  typeOptions,
  isDisabled,
  keyInputPlaceholder,
  placeHolderText,
}: Props) => {
  const getTitle = (val, k) => {
    return val.filter((v) => v.value === k);
  };

  const headerKeyChange =
    (index: number): React.ChangeEventHandler<HTMLInputElement> =>
    (e) => {
      form.changeHeaderKey(e.target.value, index);
    };

  const checkAndAddNew =
    (index: number): React.ChangeEventHandler<HTMLInputElement> =>
    () => {
      if (form.headers[index]?.name && form.headers[index]?.name.length > 0) {
        form.addNewHeader();
      }
    };

  const headerValueChange =
    (index: number): React.ChangeEventHandler<HTMLInputElement> =>
    (e) => {
      form.changeHeaderValue(e.target.value, index);
    };

  const headerTypeChange = (index: number, typeValue: string) => {
    form.changeHeaderType(typeValue, index);
  };

  const deleteHeader = (index: number) => () => {
    form.removeHeader(index);
  };

  const generateHeaderHtml = form.headers.map((h, i) => {
    const title = getTitle(typeOptions, h.type);
    return (
      <div
        className={
          styles.common_header_wrapper +
          ' ' +
          styles.display_flex +
          ' form-group'
        }
        key={i}
      >
        <input
          type="text"
          className={
            styles.input +
            ' form-control ' +
            styles.add_mar_right +
            ' ' +
            styles.defaultWidth
          }
          value={h.name}
          onChange={headerKeyChange(i)}
          onBlur={i === form.headers.length - 1 ? checkAndAddNew(i) : undefined}
          placeholder={keyInputPlaceholder}
          disabled={isDisabled}
          data-test={`remote-schema-header-test${i + 1}-key`}
        />
        <span className={styles.header_colon}>:</span>
        <span className={styles.value_wd + ' flex'}>
          <DropdownButton
            disabled={isDisabled}
            id={'common-header-' + (i + 1)}
            data-test={`remote-schema-header-test${i + 1}-dropdown-button`}
            items={typeOptions.map((o) => (
              <DropdownMenu.Item
                key={o.value}
                onChange={() => headerTypeChange(i, o.value)}
              >
                {o.display_text}
              </DropdownMenu.Item>
            ))}
          >
            {title.length > 0 ? title[0].display_text : 'Value'}
          </DropdownButton>
          <Input
            type="text"
            value={h.value}
            onChange={headerValueChange(i)}
            disabled={isDisabled}
            placeholder={placeHolderText ? placeHolderText(h.type) : undefined}
            data-test={`remote-schema-header-test${i + 1}-input`}
          />
        </span>
        {i !== form.headers.length - 1 && !isDisabled ? (
          <FaTimes
            className={styles.fontAwosomeClose + ' h-lg w-lg'}
            onClick={deleteHeader(i)}
          />
        ) : null}
      </div>
    );
  });

  return <div className={className}>{generateHeaderHtml}</div>;
};

export default Header;
