import styled from "react-emotion";

const notificationTypes = {
  warning: {
    dark: "goldenrod",
    light: "papayawhip"
  },
  error: {
    dark: "firebrick",
    light: "rosybrown"
  },
  info: {
    dark: "#663399",
    light: "#FFFFFF"
  }
};

const getColor = (type = "info", shade = "dark") =>
  notificationTypes[type][shade];

const Notification = styled('section')`
  color: ${props => getColor(props.type, "light")};
  background: ${props => getColor(props.type)};
  width: 100%;
  padding: 1rem;
  text-align: center;
`;

export default Notification;
