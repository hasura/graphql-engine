import PropTypes from 'prop-types';
import React from 'react';
import { connect } from 'react-redux';
import styled from 'styled-components';

// ************************************ //

// Heading Styled Component

const Heading = styled.h1`
  color: ${props => props.theme.colors.button.primary};
`;

// ************************************ //

const UIKit = () => <Heading>Hello</Heading>;

// ************************************ //

UIKit.propTypes = {
  dispatch: PropTypes.func.isRequired,
};

// ************************************ //

export default connect()(UIKit);
