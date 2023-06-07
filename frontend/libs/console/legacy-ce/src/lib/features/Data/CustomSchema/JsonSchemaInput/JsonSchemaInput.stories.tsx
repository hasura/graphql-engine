import { StoryObj, Meta } from '@storybook/react';
import { useState } from 'react';
import { JsonSchemaInput, JsonSchemaInputProps } from './JsonSchemaInput';

export default {
  component: JsonSchemaInput,
} as Meta<typeof JsonSchemaInput>;

const listingsAndReviews = {
  title: 'listingsAndReview',
  properties: {
    _id: {
      bsonType: '',
    },
    access: {
      bsonType: 'string',
    },
    accommodates: {
      bsonType: 'int',
    },
    address: {
      bsonType: 'object',
      properties: {
        country: {
          bsonType: 'string',
        },
        country_code: {
          bsonType: 'string',
        },
        government_area: {
          bsonType: 'string',
        },
        location: {
          bsonType: 'object',
          properties: {
            coordinates: {
              bsonType: 'array',
              items: {
                bsonType: 'double',
              },
            },
            is_location_exact: {
              bsonType: 'bool',
            },
            type: {
              bsonType: 'string',
            },
          },
        },
        market: {
          bsonType: 'string',
        },
        street: {
          bsonType: 'string',
        },
        suburb: {
          bsonType: 'string',
        },
      },
    },
    amenities: {
      bsonType: 'array',
      items: {
        bsonType: 'string',
      },
    },
    availability: {
      bsonType: 'object',
      properties: {
        availability_30: {
          bsonType: 'int',
        },
        availability_365: {
          bsonType: 'int',
        },
        availability_60: {
          bsonType: 'int',
        },
        availability_90: {
          bsonType: 'int',
        },
      },
    },
    bathrooms: {
      bsonType: 'decimal',
    },
    bed_type: {
      bsonType: 'string',
    },
    bedrooms: {
      bsonType: 'int',
    },
    beds: {
      bsonType: 'int',
    },
    calendar_last_scraped: {
      bsonType: 'date',
    },
    cancellation_policy: {
      bsonType: 'string',
    },
    cleaning_fee: {
      bsonType: 'decimal',
    },
    description: {
      bsonType: 'string',
    },
    extra_people: {
      bsonType: 'decimal',
    },
    first_review: {
      bsonType: 'date',
    },
    guests_included: {
      bsonType: 'decimal',
    },
    host: {
      bsonType: 'object',
      properties: {
        host_about: {
          bsonType: 'string',
        },
        host_has_profile_pic: {
          bsonType: 'bool',
        },
        host_id: {
          bsonType: 'string',
        },
        host_identity_verified: {
          bsonType: 'bool',
        },
        host_is_superhost: {
          bsonType: 'bool',
        },
        host_listings_count: {
          bsonType: 'int',
        },
        host_location: {
          bsonType: 'string',
        },
        host_name: {
          bsonType: 'string',
        },
        host_neighbourhood: {
          bsonType: 'string',
        },
        host_picture_url: {
          bsonType: 'string',
        },
        host_response_rate: {
          bsonType: 'int',
        },
        host_response_time: {
          bsonType: 'string',
        },
        host_thumbnail_url: {
          bsonType: 'string',
        },
        host_total_listings_count: {
          bsonType: 'int',
        },
        host_url: {
          bsonType: 'string',
        },
        host_verifications: {
          bsonType: 'array',
          items: {
            bsonType: 'string',
          },
        },
      },
    },
    house_rules: {
      bsonType: 'string',
    },
    images: {
      bsonType: 'object',
      properties: {
        medium_url: {
          bsonType: 'string',
        },
        picture_url: {
          bsonType: 'string',
        },
        thumbnail_url: {
          bsonType: 'string',
        },
        xl_picture_url: {
          bsonType: 'string',
        },
      },
    },
    interaction: {
      bsonType: 'string',
    },
    last_review: {
      bsonType: 'date',
    },
    last_scraped: {
      bsonType: 'date',
    },
    listing_url: {
      bsonType: 'string',
    },
    maximum_nights: {
      bsonType: 'string',
    },
    minimum_nights: {
      bsonType: 'string',
    },
    monthly_price: {
      bsonType: 'decimal',
    },
    name: {
      bsonType: 'string',
    },
    neighborhood_overview: {
      bsonType: 'string',
    },
    notes: {
      bsonType: 'string',
    },
    number_of_reviews: {
      bsonType: 'int',
    },
    price: {
      bsonType: 'decimal',
    },
    property_type: {
      bsonType: 'string',
    },
    review_scores: {
      bsonType: 'object',
      properties: {
        review_scores_accuracy: {
          bsonType: 'int',
        },
        review_scores_checkin: {
          bsonType: 'int',
        },
        review_scores_cleanliness: {
          bsonType: 'int',
        },
        review_scores_communication: {
          bsonType: 'int',
        },
        review_scores_location: {
          bsonType: 'int',
        },
        review_scores_rating: {
          bsonType: 'int',
        },
        review_scores_value: {
          bsonType: 'int',
        },
      },
    },
    reviews: {
      bsonType: 'array',
      items: {
        bsonType: 'object',
        properties: {
          _id: {
            bsonType: 'string',
          },
          comments: {
            bsonType: 'string',
          },
          date: {
            bsonType: 'date',
          },
          listing_id: {
            bsonType: 'string',
          },
          reviewer_id: {
            bsonType: 'string',
          },
          reviewer_name: {
            bsonType: 'string',
          },
        },
      },
    },
    room_type: {
      bsonType: 'string',
    },
    security_deposit: {
      bsonType: 'decimal',
    },
    space: {
      bsonType: 'string',
    },
    summary: {
      bsonType: 'string',
    },
    transit: {
      bsonType: 'string',
    },
    weekly_price: {
      bsonType: 'decimal',
    },
  },
};

const json = JSON.stringify(listingsAndReviews, null, 2);

export const Primary: StoryObj<JsonSchemaInputProps> = {
  render: args => {
    const [value, setValue] = useState<string>(json);
    return <JsonSchemaInput value={value} onChange={setValue} />;
  },
};
