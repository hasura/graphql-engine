import React from 'react';
import Toggle from 'react-toggle';
import './Toggle.css';
import 'react-toggle/style.css';

type ToggleProps = {
  onChange: () => void;
  icons: boolean;
  checked: boolean;
  className: string;
};

const ToggleComponent = (props: ToggleProps) => (
  <Toggle className={props.className || ''} {...props} />
);

export default ToggleComponent;
