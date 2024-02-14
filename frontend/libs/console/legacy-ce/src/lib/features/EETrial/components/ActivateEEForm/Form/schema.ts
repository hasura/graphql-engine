import { z } from 'zod';

const internationalPhoneNumberRegex =
  /\+(9[976]\d|8[987530]\d|6[987]\d|5[90]\d|42\d|3[875]\d|2[98654321]\d|9[8543210]|8[6421]|6[6543210]|5[87654321]|4[987654310]|3[9643210]|2[70]|7|1)\W*\d\W*\d\W*\d\W*\d\W*\d\W*\d\W*\d\W*\d\W*(\d{1,2})$/;

export const registrationSchema = z.object({
  firstName: z.string().min(1, { message: 'Please add your first name' }),
  lastName: z.string().min(1, { message: 'Please add your last name' }),
  email: z
    .string()
    .min(1, { message: 'Please add your work email' })
    .email({ message: 'Please add a valid work email' }),
  organization: z.string().min(1, { message: 'Please add your organization' }),
  jobFunction: z.string(),
  phoneNumber: z
    .string()
    .regex(internationalPhoneNumberRegex, {
      message: 'Please add valid phone number',
    })
    .optional()
    .or(z.literal('')),
  password: z.string().min(8).nonempty({
    message:
      'Password must include at least 8 characters and at most 64 characters with at least: 1 upper case letter, 1 lower case letter, 1 number and 1 special character.',
  }),
  consent: z
    .boolean()
    .refine(
      value => value === true,
      'Please agree to our Terms of Service and Privacy Policy'
    ),
  hasuraUseCase: z.string(),
  eeUseCase: z.array(z.string()).nonempty({
    message: 'Please select at least one use case',
  }),
});

export const activationSchema = z.object({
  email: z
    .string()
    .min(1, { message: 'Please add your work email' })
    .email({ message: 'Please add a valid work email' }),
  password: z.string().min(8).nonempty({
    message:
      'Password must include at least 8 characters and at most 64 characters with at least: 1 upper case letter, 1 lower case letter, 1 number and 1 special character.',
  }),
});

export type RegisterEEFormSchema = z.infer<typeof registrationSchema>;
export type ActivateEEFormSchema = z.infer<typeof activationSchema>;
