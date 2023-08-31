const twStyles = {
  outerContainer: `h-full block w-full bg-white border-r border-solid border-r-[#e5e5e5]`,
  linksContainer: `flex flex-col`,
  link: {
    default: `px-4 py-4  flex border-b border-gray-200 items-center gap-2 text-subtitle`,
    active: `border-l-4 border-l-cyan-800`,
  },
  sideBarItem: {
    default: `text-muted hover:bg-gray-100 cursor-pointer active:opacity-60 select-none w-full data-[isloading="true"]:opacity-100 data-[isloading="true"]:hover:bg-transparent data-[isloading="true"]:cursor-progress`,
    active: `bg-blue-100 text-blue-900 hover:bg-blue-100 active:opacity-100`,
  },
  treeItem: `flex items-center w-full relative gap-1.5 px-2 py-3 h-[36px] disabled:opacity-75 transition-opacity disabled:pointer-events-none disabled:cursor-wait`,
};

//rename export for friendlier name
export const styles = twStyles;
