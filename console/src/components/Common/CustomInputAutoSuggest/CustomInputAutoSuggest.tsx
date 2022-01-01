import React, { useState } from 'react';
import Autosuggest from 'react-autosuggest';
import type {
  GetSectionSuggestions,
  GetSuggestionValue,
  InputProps,
  RenderSectionTitle,
  RenderSuggestion,
  SuggestionsFetchRequested,
} from 'react-autosuggest';
import styles from './Theme.scss';

interface AutoSuggestOption {
  label: string;
  value: string;
}

interface AutoSuggestSection {
  title: string;
  suggestions: AutoSuggestOption[];
}

interface CustomInputAutoSuggest extends InputProps<AutoSuggestOption> {
  options: AutoSuggestSection[];
  theme: any;
}

const CustomInputAutoSuggest: React.FC<CustomInputAutoSuggest> = ({
  options,
  theme = styles,
  ...inputProps
}) => {
  const [suggestions, setSuggestions] = useState<AutoSuggestSection[]>([]);

  const getSuggestions = (value: string) => {
    const inputValue = value.trim().toLowerCase();
    const inputLength = inputValue.length;
    const filterResults = () => {
      return options.map(option => {
        return {
          title: option.title,
          suggestions: option.suggestions.filter(
            op => op.value.toLowerCase().slice(0, inputLength) === inputValue
          ),
        };
      });
    };
    return inputLength === 0 ? [...options] : filterResults();
  };

  const onSuggestionsFetchRequested: SuggestionsFetchRequested = ({
    value,
  }) => {
    setSuggestions(getSuggestions(value));
  };

  const getSuggestionValue: GetSuggestionValue<AutoSuggestOption> = suggestion =>
    suggestion.value;

  const onSuggestionsClearRequested = () => {
    setSuggestions([]);
  };

  const renderSuggestion: RenderSuggestion<AutoSuggestOption> = suggestion => (
    <div>{suggestion.value}</div>
  );

  /* Don't render the section when there are no suggestions in it */
  const renderSectionTitle: RenderSectionTitle = (
    section: AutoSuggestSection
  ) => {
    return section.suggestions.length > 0 ? section.title : null;
  };

  const getSectionSuggestions: GetSectionSuggestions<
    AutoSuggestOption,
    AutoSuggestSection
  > = section => {
    return section.suggestions;
  };

  return (
    <Autosuggest<AutoSuggestOption, AutoSuggestSection>
      suggestions={suggestions}
      onSuggestionsFetchRequested={onSuggestionsFetchRequested}
      onSuggestionsClearRequested={onSuggestionsClearRequested}
      getSuggestionValue={getSuggestionValue}
      renderSuggestion={renderSuggestion}
      inputProps={{ ...inputProps }}
      theme={theme}
      multiSection
      renderSectionTitle={renderSectionTitle}
      shouldRenderSuggestions={() => true}
      getSectionSuggestions={getSectionSuggestions}
    />
  );
};

export default CustomInputAutoSuggest;
