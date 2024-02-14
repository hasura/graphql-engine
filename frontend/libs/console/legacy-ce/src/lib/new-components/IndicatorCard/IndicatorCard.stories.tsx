import React from 'react';
import { Meta } from '@storybook/react';
import { IndicatorCard } from './IndicatorCard';

export default {
  title: 'components/IndicatorCard',
  component: IndicatorCard,
} as Meta<typeof IndicatorCard>;

export const showcase = () => (
  <div className="space-y-4">
    <IndicatorCard>
      This is a card component with an `info` indication state.
    </IndicatorCard>
    <IndicatorCard status="positive">
      This is a card component with an `positive` indication state.
    </IndicatorCard>
    <IndicatorCard status="negative">
      This is a card component with an `negative` indication state.
    </IndicatorCard>
    <IndicatorCard headline="Card headline goes here">
      This is a card component with an `info` indication state.
    </IndicatorCard>
    <IndicatorCard status="positive" headline="Card headline goes here">
      This is a card component with an `positive` indication state.
    </IndicatorCard>
    <IndicatorCard status="negative" headline="Card headline goes here">
      This is a card component with an `negative` indication state.
    </IndicatorCard>
    <IndicatorCard showIcon>
      This is a card component with an `info` indication state.
    </IndicatorCard>
    <IndicatorCard showIcon status="positive">
      This is a card component with an `positive` indication state.
    </IndicatorCard>
    <IndicatorCard showIcon status="negative">
      This is a card component with an `negative` indication state.
    </IndicatorCard>
    <IndicatorCard showIcon headline="Card headline goes here">
      This is a card component with an `info` indication state.
    </IndicatorCard>
    <IndicatorCard
      showIcon
      status="positive"
      headline="Card headline goes here"
    >
      This is a card component with an `positive` indication state.
    </IndicatorCard>
    <IndicatorCard
      showIcon
      status="negative"
      headline="Card headline goes here"
    >
      This is a card component with an `negative` indication state.
    </IndicatorCard>
  </div>
);
