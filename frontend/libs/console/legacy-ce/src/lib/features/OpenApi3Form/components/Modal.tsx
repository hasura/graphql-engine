import React from 'react';
import { Dialog } from '../../../new-components/Dialog';
import { Button } from '../../../new-components/Button';
import { FaExternalLinkAlt, FaFileImport, FaRegEdit } from 'react-icons/fa';
import { Badge } from '../../../new-components/Badge';

type props = {
  handleActionForm: () => void;
  handleOpenApiForm: () => void;
};

export const ActionInitialModal = ({
  handleActionForm,
  handleOpenApiForm,
}: props) => {
  return (
    <Dialog onClose={() => {}}>
      <>
        <div className="px-6 py-8">
          <div className="font-bold text-lg">
            Let&apos;s create an Action from
          </div>
          <span className="text-sm font-normal text-muted">
            Two options available to create an Action
          </span>
          <div className="flex gap-8 pt-4">
            <div className="w-1/2 border-solid bg-white border p-3">
              <p className="font-bold text-muted text-sm pb-1 flex items-center ml-auto">
                Action Form
                <Badge className="ml-auto" color="gray">
                  Default
                </Badge>
              </p>
              <p className="text-xs font-normal text-muted pb-4">
                Create your Action via form input manually.
              </p>

              <Button icon={<FaRegEdit />} size="sm" onClick={handleActionForm}>
                Fill Action Form
              </Button>
            </div>
            <div className="w-1/2 rounded border-solid bg-white border p-3">
              <p className="font-bold text-muted text-sm pb-1 flex items-center">
                OpenAPI Spec
                <Badge className="ml-auto" color="purple">
                  New
                </Badge>
              </p>
              <p className="text-xs font-normal text-muted pb-4">
                Generate Actions from OpenAPI spec (OAS).
              </p>

              <Button
                icon={<FaFileImport />}
                size="sm"
                type="submit"
                mode="primary"
                onClick={handleOpenApiForm}
              >
                Import from OAS
              </Button>
            </div>
          </div>
        </div>
        <div className="border-t border-gray-300 py-2 px-6">
          <a
            href="https://hasura.io/docs/latest/actions/create/"
            target="_blank"
            className="font-normal text-secondary text-italic text-sm flex items-center"
            rel="noopener noreferrer"
          >
            Learn more about creating Actions in Hasura
            <FaExternalLinkAlt className="pl-1" />
          </a>
        </div>
      </>
    </Dialog>
  );
};
