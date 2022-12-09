export const createDisplayJson = (value: Record<string, any>) => {
  // the form records the operators as well as the values
  // this allows react hook form to correctly render the display
  // however these are not needed for displaying the output and need to be removed
  const object = Object.entries(value)
    .filter(([key]) => key !== 'operators')
    .reduce<Record<string, any>>((acc, [key, v]) => {
      acc[key] = v;
      return acc;
    }, {});

  return JSON.stringify(object);
};
