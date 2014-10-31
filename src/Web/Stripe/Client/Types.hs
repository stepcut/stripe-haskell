{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
-- |
-- Module      : Web.Stripe.Client.Types
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Client.Types
  ( -- * Types
    Stripe
  , StripeF       (..)
  , StripeRequest (..)
  , StripeConfig  (..)
  , APIVersion    (..)
  ) where

import           Control.Monad.Free         (Free)
import           Data.Aeson.Types           (Parser, Value)
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text)
import           Network.Http.Client        (Connection, Method)
import           Web.Stripe.Client.Error    (StripeError (..))

------------------------------------------------------------------------------
-- | HTTP Params
type Params = [(ByteString, ByteString)]

------------------------------------------------------------------------------
-- | Stripe Request holding `Method`, URL and `Params` for a Request
data StripeRequest a = StripeRequest
    { method      :: Method -- ^ Method of StripeRequest (i.e. `GET`, `PUT`, `POST`, `PUT`)
    , endpoint    :: Text   -- ^ Endpoint of StripeRequest
    , queryParams :: Params -- ^ Query Parameters of StripeRequest
    } deriving (Functor, Show)

------------------------------------------------------------------------------
-- | The `Stripe` Functor
data StripeF a = StripeF (StripeRequest a) (ByteString -> Either String a)
    deriving Functor

------------------------------------------------------------------------------
-- | The `Stripe` Monad
type Stripe = Free StripeF
-- type Stripe a = EitherT StripeError (ReaderT (StripeConfig, Connection) IO) a

------------------------------------------------------------------------------
-- | Stripe secret key
data StripeConfig = StripeConfig
    { secretKey :: ByteString
    } deriving Show

------------------------------------------------------------------------------
-- | API Version
data APIVersion =
    V20141007 -- ^ Stripe API Version for this package release
    deriving Eq

instance Show APIVersion where
    show V20141007 = "2014-10-07"

