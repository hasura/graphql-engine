export const twBaseStyle = `
   text-base 
   rounded-md
   flex
   items-center
   py-[.625rem]
   pr-3
   relative
   pl-3
   mx-2
   select-none
   group-data-[disabled]/item:!pointer-events-none
   group-data-[disabled]/item:!text-neutral-200`;

export const twSelectableItem = `pl-6`;

export const twDefault = `
   text-gray-900           
   group-data-[highlighted]/item:!bg-gray-100        
   group-data-[state=open]/item:bg-gray-100 
   group-data-[state=open]/item:text-gray-900`;

export const twDangerous = `
   !text-red-600
   group-data-[highlighted]/item:!bg-red-100`;
