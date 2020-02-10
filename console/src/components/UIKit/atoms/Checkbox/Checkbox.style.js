import styled from 'styled-components';

import { BaseStyledDiv } from '../Common.style';

export const CheckboxStyles = styled(BaseStyledDiv)`
  input[type='checkbox'] {
    /* take it out of document flow */
    position: absolute;
    /* hide it */
    opacity: 0;

    & + label {
      position: relative;
      cursor: pointer;
    }

    /* Box */
    & + label:before {
      content: '';
      display: inline-block;
      vertical-align: text-top;
      width: 18px;
      height: 18px;
      background: transparent;
      border: 2px solid #939390;
      border-radius: 2px;
      margin-right: 8px;
    }

    /* Box hover */
    &:hover + label:before {
      border: 2px solid #454236;
    }

    /* Box checked */
    &:checked + label:before {
      background: #f8d721;
      border: 2px solid #f8d721;
    }

    /* Disabled State */

    label. &:disabled + label {
      color: #b8b8b8;
      cursor: auto;
    }

    /* Disabled */

    box. &:disabled + label:before {
      box-shadow: none;
      background: #ddd;
    }

    /* Checkmark. Could be replaced with an image */

    &:checked + label:after {
      content: '';
      position: absolute;
      left: 4px;
      top: 8px;
      background: white;
      width: 2px;
      height: 2px;
      box-shadow: 2px 0 0 white, 4px 0 0 white, 4px -2px 0 white,
        4px -4px 0 white, 4px -6px 0 white, 4px -8px 0 white;
      transform: rotate(45deg);
    }
  }
`;
