import { CardedTable } from '../../../new-components/CardedTable';
import React from 'react';
import { FaTrash } from 'react-icons/fa';
import { useListAvailableAgentsFromMetadata } from '../hooks';
import { useRemoveAgent } from '../hooks/useRemoveAgent';

export const ManageAgentsTable = () => {
  const { data, isLoading } = useListAvailableAgentsFromMetadata();

  const { removeAgent } = useRemoveAgent();

  if (isLoading) return <>Loading...</>;

  if (!data) return <>Something went wrong while fetching data</>;

  if (!data.length)
    return (
      <div className="text-gray-600 my-md">
        <i>There are no data connector agents connected to Hasura.</i>
      </div>
    );

  return (
    <div className="mt-md">
      <CardedTable.Table>
        <CardedTable.TableHead>
          <CardedTable.TableHeadRow>
            <CardedTable.TableHeadCell>Agent Name</CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>URL</CardedTable.TableHeadCell>
            <CardedTable.TableHeadCell>Actions</CardedTable.TableHeadCell>
          </CardedTable.TableHeadRow>
        </CardedTable.TableHead>

        <CardedTable.TableBody>
          {data.map((agent, id) => {
            return (
              <CardedTable.TableBodyRow key={`${agent.name}-${id}`}>
                <CardedTable.TableBodyCell>
                  {agent.name}
                </CardedTable.TableBodyCell>
                <CardedTable.TableBodyCell>
                  {agent.url}
                </CardedTable.TableBodyCell>
                <CardedTable.TableBodyCell>
                  <div className="flex items-center justify-end whitespace-nowrap text-right opacity-0 transition-all duration-200 group-hover:opacity-100">
                    <button
                      onClick={() => {
                        removeAgent({ name: agent.name });
                      }}
                      className="flex px-2 py-0.5 items-center font-semibold rounded text-red-700 hover:bg-red-50 focus:bg-red-100"
                    >
                      <FaTrash className="fill-current mr-1" />
                      Remove
                    </button>
                  </div>
                </CardedTable.TableBodyCell>
              </CardedTable.TableBodyRow>
            );
          })}
        </CardedTable.TableBody>
      </CardedTable.Table>
    </div>
  );
};
