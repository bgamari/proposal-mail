{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C8
import Data.Aeson
import qualified Data.Text as T

import Servant
import Servant.Server
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Servant.GitHub.Webhook
import GitHub
import GitHub.Data.Id

type Api = "/proposals"
           :> GitHubEvent '[ 'WebhookPullRequestEvent ]
           :> GitHubSignedReqBody '[JSON] PullRequestEvent
           :> Post '[JSON] ()

api :: Proxy Api
api = Proxy

server :: Server Api
server = newProposalPullRequest

newProposalPullRequest :: RepoWebhookEvent -> ((), PullRequestEvent) -> Handler ()
newProposalPullRequest WebhookPingEvent _ = liftIO $ putStrLn "ping"
newProposalPullRequest WebhookPullRequestEvent ((), ev) = do
    liftIO $ putStrLn "new-pr"
    let prId = pullRequestId $ pullRequestEventPullRequest ev
        user = simpleUserLogin $ pullRequestSender ev
    case pullRequestEventAction ev of
      PullRequestOpened -> do
          liftIO $ putStrLn "opened"
      _ ->
          liftIO $ print ev

main :: IO ()
main = do
    key <- C8.readFile "keys"
    run 8080 (app (gitHubKey $ pure key))

app :: GitHubKey -> Application
app k = serveWithContext api (k :. EmptyContext) server

pullRequestIssueId :: Id PullRequest -> Id Issue
pullRequestIssueId (Id n) = Id n
