import { useEffect } from 'react';
import { applyAllowList, deleteCollectionIfExist } from './Actions';

const PerformApply = ({ query, dispatch, updateStatus }) => {
  const { name: collectionName } = query[0].args;
  useEffect(() => {
    const performApply = () => {
      return dispatch(applyAllowList(query))
        .then(() => {
          updateStatus('Applied Successfully');
          setTimeout(() => updateStatus('Apply'), 5000);
        })
        .catch(applyErr => {
          updateStatus(applyErr.toString());
        });
    };
    dispatch(deleteCollectionIfExist(collectionName))
      .then(() => {
        return performApply();
      })
      .catch(err => {
        /*
         * Sample error response for collection doesn't exist
          {"path":"$.args.collection","error":"query collection with name \"my_collection\" does not exists","code":"not-exists"}
          */
        if (err.code === 'not-exists') {
          // Run only if collection is not found in the db.
          return performApply();
        }

        updateStatus(err.toString());
      });
  }, []);
  return null;
};

export default PerformApply;
