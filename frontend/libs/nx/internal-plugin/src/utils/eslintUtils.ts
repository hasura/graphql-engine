import { Linter } from 'eslint';

export function isRuleLevelAndOption(
  rule: Linter.RuleEntry,
): rule is Linter.RuleSeverityAndOptions {
  return Array.isArray(rule) && rule.length > 1;
}

export function modifyEslintRuleOptions(
  eslintRc: Linter.BaseConfig,
  ruleName: string,
  newOptions: (oldValue: Record<string, any>) => Record<string, any>,
): Linter.BaseConfig {
  if (!eslintRc.overrides) {
    return eslintRc;
  }
  return {
    ...eslintRc,
    overrides: eslintRc.overrides.map((overide) => {
      if (!overide.rules) {
        return overide;
      }
      return {
        ...overide,
        rules: Object.fromEntries(
          Object.entries(overide.rules).map(([rule, ruleConfig]) => {
            if (rule !== ruleName) {
              return [rule, ruleConfig];
            }

            if (!ruleConfig) {
              return [rule, ruleConfig];
            }

            if (isRuleLevelAndOption(ruleConfig)) {
              const [ruleLevel, ruleOptions] = ruleConfig;
              // `RuleSeverityAndOptions` types the options element as `unknown`;
              // at this point it is the rule's options object (see the
              // `isRuleLevelAndOption` guard), matching `newOptions`' parameter.
              return [
                rule,
                [ruleLevel, newOptions(ruleOptions as Record<string, any>)],
              ];
            }
            return [rule, [ruleConfig, newOptions({})]];
          }),
        ),
      };
    }),
  };
}
