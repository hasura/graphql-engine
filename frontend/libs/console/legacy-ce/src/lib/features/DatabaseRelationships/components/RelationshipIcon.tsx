import { FaArrowRight } from 'react-icons/fa';
import oneToMany from '../images/one-to-many.svg';
import oneToOne from '../images/one-to-one.svg';

export const RelationshipIcon = ({
  type,
}: {
  type: 'one-to-one' | 'one-to-many' | 'other';
}) => {
  return (
    <>
      {type === 'one-to-many' ? (
        <img src={oneToMany} alt="One-To-Many" style={{ height: 25 }} />
      ) : type === 'one-to-one' ? (
        <img src={oneToOne} alt="One-To-One" style={{ height: 25 }} />
      ) : (
        //fallback to a basic arrow
        <FaArrowRight />
      )}
    </>
  );
};
