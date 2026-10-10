import { FaCheck } from 'react-icons/fa';
import { Dialog, JsonCodeBlock, Tooltip } from '@hasura/shared/ui';
import { TokenInfo } from './utils';
import { ServerConfig } from '@hasura/metadata/api';

type Props = {
  resetAnalyzingToken: () => void;
  tokenInfo: TokenInfo;
  serverConfig: ServerConfig | undefined;
};

const TokenAnalyzeDialog = ({
  resetAnalyzingToken,
  serverConfig,
  tokenInfo,
}: Props) => {
  const onAnalyzeBearerClose = () => {
    resetAnalyzingToken();
  };

  const getHasuraClaims = () => {
    const payload = tokenInfo.payload;
    if (!payload) {
      return null;
    }

    const {
      claims_namespace: claimNameSpace = 'https://hasura.io/jwt/claims',
      claims_format: claimFormat = 'json',
    } = serverConfig?.jwt ?? {};

    const isValidPayload = Object.keys(payload).length;
    const payloadHasValidNamespace = claimNameSpace in payload;
    const isSupportedFormat =
      ['json', 'stringified_json'].indexOf(claimFormat) !== -1;

    if (!isValidPayload || !payloadHasValidNamespace || !isSupportedFormat) {
      return null;
    }

    let claimData = '';

    const generateValidNameSpaceData = (claimD) => {
      return JSON.stringify(claimD, null, 2);
    };

    try {
      claimData =
        claimFormat === 'stringified_json'
          ? generateValidNameSpaceData(JSON.parse(payload[claimNameSpace]))
          : generateValidNameSpaceData(payload[claimNameSpace]);
    } catch (e) {
      claimData =
        claimFormat === 'stringified_json'
          ? String(payload[claimNameSpace])
          : generateValidNameSpaceData(payload[claimNameSpace]);
    }

    return [
      <br key="hasura_claim_element_break" />,
      <span
        key="hasura_claim_label"
        className={'uppercase m-0 text-black pb-2 border-b border-[#9b9b9b80]'}
      >
        Hasura Claims:
        <span>hasura headers</span>
      </span>,
      <JsonCodeBlock key="hasura_claim_value" value={claimData} />,
      <br key="hasura_claim_element_break_after" />,
    ];
  };

  const analyzeBearerBody = tokenInfo.error ? (
    <span>{tokenInfo.error}</span>
  ) : (
    <div className="p-4">
      <span className={'uppercase text-black border-b border-[#9b9b9b80]'}>
        Token Validity:
        <span className="mb-2 text-md">
          <Tooltip content="Valid JWT token">
            <span className="text-[#28a745]">
              <FaCheck />
            </span>
          </Tooltip>
        </span>
      </span>
      {tokenInfo.error || <br />}
      {getHasuraClaims() || <br />}
      <span
        className={'uppercase mb-2 text-black pb-2 border-b border-[#9b9b9b80]'}
      >
        Header:
        <span>Algorithm & Token Type</span>
      </span>
      <JsonCodeBlock value={tokenInfo.header} />
      <br />
      <span
        className={'uppercase m-0 text-black pb-2 border-b border-[#9b9b9b80]'}
      >
        Full Payload:
        <span>Data</span>
      </span>
      <JsonCodeBlock value={tokenInfo.payload} />
    </div>
  );

  return (
    <Dialog
      onClose={onAnalyzeBearerClose}
      title={tokenInfo.error ? 'Error decoding JWT' : 'Decoded JWT'}
      size="xxl"
    >
      {analyzeBearerBody}
    </Dialog>
  );
};

export default TokenAnalyzeDialog;
