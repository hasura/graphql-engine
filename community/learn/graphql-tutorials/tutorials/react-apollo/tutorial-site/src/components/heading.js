import system from "system-components/emotion";

const Heading = system(
  {
    is: "h2",
    fontSize: 5,
    fontWeight: "700",
    lineHeight: 1.5,
    mt: 4,
    mb: 3
  },
  "fontFamily",
  "color",
  "textAlign"
);
Heading.displayName = "Heading";

export default Heading;
