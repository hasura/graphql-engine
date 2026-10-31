import { Button } from '@hasura/shared/ui';
import { FaDownload } from 'react-icons/fa';
import {
  downloadBinaryValue,
  formatByteSize,
  getBinaryValueSize,
} from '../utils';

interface BinaryValueProps {
  value: string;
  fileName: string;
  'data-testid'?: string;
}

/**
 * A binary value is offered as a download instead of being shown, as it is
 * often too large to render and not readable as text anyway.
 */
export const BinaryValue = ({
  value,
  fileName,
  'data-testid': testId,
}: BinaryValueProps) => (
  <Button
    size="sm"
    mode="default"
    leftIcon={FaDownload}
    onClick={() => downloadBinaryValue(fileName, value)}
    data-testid={testId}
  >
    Download ({formatByteSize(getBinaryValueSize(value))})
  </Button>
);
