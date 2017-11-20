data Method
data Status
data Header

data Request = Request {
    requestMethod      :: Method
  , httpVersion        :: HttpVersion
  , rawPathInfo        :: ByteString
  , rawQueryString     :: ByteString
  , requestHeaders     :: RequestHeaders
  , isSecure           :: Bool
  , remoteHost         :: SockAddr
  , pathInfo           :: [Text]
  , queryString        :: Query
  , requestBody        :: Source IO ByteString
  , vault              :: Vault
  ,requestBodyLength   :: RequestBodyLength
  , requestHeaderHost  :: Maybe B.ByteString
  , requestHeaderRange :: Maybe B.ByteString
  }

data Response
  = ResponseFile Status ResponseHeaders FilePath (Maybe FilePart)
  | ResponseBuilder Status ResponseHeaders Builder
  | ResponseSource Status ResponseHeaders (forall b. WithSource IO (C.Flush Builder) b)
  | ResponseRaw (forall b. WithRawApp b) Response

type Application = Request -> IO Response
