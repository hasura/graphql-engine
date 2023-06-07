import { PermissionsIcon } from '../../../../Permissions/PermissionsTable/components/PermissionsIcons';

interface PermissionAccessCellProps extends React.ComponentProps<'button'> {
  access: 'fullAccess' | 'partialAccess' | 'noAccess';
  isEditable: boolean;
  isCurrentEdit: boolean;
}

export const PermissionAccessCell: React.FC<PermissionAccessCellProps> = ({
  access,
  isEditable,
  isCurrentEdit,
  ...rest
}) => {
  if (!isEditable) {
    return (
      <td className="p-md whitespace-nowrap text-center cursor-not-allowed opacity-30">
        <PermissionsIcon type={access} selected={isCurrentEdit} />
      </td>
    );
  }

  return (
    <td>
      <button
        type="submit"
        className={`cursor-pointer h-20 border-none w-full whitespace-nowrap text-center ${
          isCurrentEdit ? 'bg-amber-300' : 'hover:bg-indigo-50'
        }`}
        {...rest}
      >
        <PermissionsIcon type={access} selected={isCurrentEdit} />
      </button>
    </td>
  );
};
