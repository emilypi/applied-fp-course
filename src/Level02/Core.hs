{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp) where

import           Network.Wai                (Application, Request, Response,
                                             pathInfo, requestMethod,
                                             responseLBS, strictRequestBody)
import           Network.Wai.Handler.Warp   (run)

import           Network.HTTP.Types         (Status, StdMethod (GET, POST),
                                             hContentType, parseMethod,
                                             status200, status400, status404)

import qualified Data.ByteString.Lazy       as LBS
import           Data.ByteString.Lazy.Char8 (pack)

import           Data.Either                (either)

import           Data.Text                  (Text, unpack)
import           Data.Text.Encoding         (decodeUtf8)

import           Control.Applicative        (liftA2)
import           Level02.Types              (ContentType (..), Error (..),
                                             RqType (..), getCommentText,
                                             getTopic, mkCommentText, mkTopic,
                                             renderContentType)

-- --------------------------------------------
-- - Don't start here, go to Level02.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse s ct =
  responseLBS s [(hContentType, renderContentType ct)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 = mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 = mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 = mkResponse status400

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest t lbs =
  let f = mkCommentText . decodeUtf8 . LBS.toStrict
  in liftA2 AddRq (mkTopic t) (f lbs)
-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest =
  (<$>) ViewRq . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse (EmptyTextError e)            = resp400 Text (pack e)
mkErrorResponse (UnsupportedOperationError e) = resp404 Text (pack e)

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest r =
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  case requestMethod r of
    "GET" ->
      case pathInfo r of
        ["list"]    -> return mkListRequest
        [t, "view"] -> return $ mkViewRequest t
        _ -> return $ Left (UnsupportedOperationError "Operation Unsupported")
    "POST" ->
      case pathInfo r of
        [t, "add"] -> mkAddRequest t <$> strictRequestBody r
        _   -> return $ Left (UnsupportedOperationError "Operation Unsupported")
    _ -> return $ Left (UnsupportedOperationError "Operation Unsupported")

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq t ct) =
  Right $ resp200 Text $ pack ("Add Request - \nCommentText:\t" ++ unpack (getCommentText ct) ++ "\nTopic:\t" ++ unpack (getTopic t))
handleRequest (ViewRq t) =
  Right $ resp200 Text $ pack ("View Request - \nTopic:\t " ++ unpack (getTopic t))
handleRequest ListRq =
  Right $ resp200 Text (pack "DummyList")


-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app
  :: Application
app r cb = do
  mk <- mkRequest r
  let res = mk >>= handleRequest
  case res of
    Left t  -> cb (mkErrorResponse t)
    Right a -> cb a




runApp :: IO ()
runApp = run 3000 app
