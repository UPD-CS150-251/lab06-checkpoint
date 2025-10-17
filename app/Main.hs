{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Miso as M
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import Miso.String (fromMisoString, ms)
import qualified Network.HTTP.Req as Req

serverIpAddr :: Text
serverIpAddr = "10.0.100.197"

serverPort :: Int
serverPort = 15000

data Model = Model
  { code :: M.MisoString
  , request :: Request M.MisoString
  }
  deriving (Eq)

data Request a
  = Done a
  | Pending
  | NotStarted
  deriving (Show, Eq)

initModel :: Model
initModel = Model{code = "", request = NotStarted}

data Msg
  = MsgUpdateCode M.MisoString
  | MsgSubmit
  | MsgGotResponse M.MisoString
  deriving (Show, Eq)

update :: Msg -> M.Transition Model Msg
update (MsgUpdateCode code) = do
  model <- M.get
  let model' = model{code}
  M.put model'
--
update MsgSubmit = do
  model <- M.get
  let model' = model{code = "", request = Pending}
  M.put model'

  M.io $ do
    y <- Req.runReq Req.defaultHttpConfig $ do
      r <-
        Req.req
          Req.POST
          (Req.http serverIpAddr)
          (Req.ReqBodyUrlEnc $ "code" Req.=: (fromMisoString model.code :: String))
          Req.bsResponse
          (Req.port serverPort)
      pure . decodeUtf8 . Req.responseBody $ r

    M.consoleLog . ms $ y

    pure $ MsgGotResponse $ ms y
--
update (MsgGotResponse resp) = do
  model <- M.get
  let model' = model{request = Done resp}
  M.put model'

view :: Model -> M.View Model Msg
view model =
  H.div_
    []
    ( [ H.button_ [H.onClick MsgSubmit] [M.text "Submit"]
      | model.request /= Pending
      ]
        <> [ H.br_ []
           , H.textarea_
               [ P.rows_ "30"
               , P.cols_ "50"
               , H.onInput MsgUpdateCode
               , P.value_ model.code
               ]
               []
           , H.br_ []
           , H.pre_ [] [M.text . M.ms . show $ model.request]
           ]
    )

app :: M.App Model Msg
app = M.component initModel update view

main :: IO ()
main = M.run $ M.startApp app
