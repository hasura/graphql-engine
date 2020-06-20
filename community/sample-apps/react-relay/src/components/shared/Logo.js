import React from "react";
import styled from "@emotion/styled";

const LogoText = styled.h1`
  font-family: "IMFell", "Open Sans", "Helvetica Neue", sans-serif;
  font-size: 10rem;
  color: grey;
  margin: 0;
  text-align: center;
`;

const Logo = () => {
  return <LogoText>Melange</LogoText>;
};

export default Logo;
