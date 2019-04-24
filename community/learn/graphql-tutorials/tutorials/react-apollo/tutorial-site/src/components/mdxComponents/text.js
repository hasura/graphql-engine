import system from "system-components/emotion";

const Text = system(
  {
    m: 0
  },
  "space",
  "color",
  "fontFamily",
  "fontSize",
  "fontWeight",
  "textAlign",
  "lineHeight"
);
Text.displayName = "Text";

export default Text;
