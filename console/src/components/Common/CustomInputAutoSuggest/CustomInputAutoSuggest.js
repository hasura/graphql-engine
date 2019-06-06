import React, { useState } from 'react';
import PropTypes from 'prop-types';
import Autosuggest from 'react-autosuggest';

const CustomInputAutoSuggest = props => {
  const [suggestions, setSuggestions] = useState([]);
  const { options, theme } = props;
  const getSuggestions = value => {
    const inputValue = value.trim().toLowerCase();
    const inputLength = inputValue.length;

    return inputLength === 0
      ? [...options]
      : options.filter(
        option =>
          option.value.toLowerCase().slice(0, inputLength) === inputValue
      );
  };
  const onSuggestionsFetchRequested = ({ value }) => {
    setSuggestions(getSuggestions(value));
  };
  const getSuggestionValue = suggestion => suggestion.value;
  const onSuggestionsClearRequested = () => {
    setSuggestions([]);
  };
  const renderSuggestion = suggestion => <div>{suggestion.value}</div>;

  return (
    <Autosuggest
      suggestions={suggestions}
      onSuggestionsFetchRequested={onSuggestionsFetchRequested}
      onSuggestionsClearRequested={onSuggestionsClearRequested}
      getSuggestionValue={getSuggestionValue}
      renderSuggestion={renderSuggestion}
      inputProps={{ ...props }}
      theme={theme}
      shouldRenderSuggestions={() => true}
    />
  );
};

CustomInputAutoSuggest.propTypes = {
  options: PropTypes.array.isRequired,
};

export default CustomInputAutoSuggest;
