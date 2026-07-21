-- | The CLI entry point: @serve@ runs the HTTP API plus every background loop in
-- one process; @reproject@ rebuilds the facts projection from stored observations.
module Main
  ( main
  ) where

import           Data.Version        (showVersion)
import           Options.Applicative
import           Paths_pet_report    (version)
import           PetReport.App       (App (..), withApp)
import qualified PetReport.Effect.Db as Db
import           PetReport.Trace     (StartupEvent (..), startupTracer,
                                      traceWith)
import qualified PetReport.Web       as Web

data Command
  = Serve
  | Reproject
  deriving stock (Show)

commandP :: Parser Command
commandP =
  hsubparser
    ( command "serve" (info (pure Serve) (progDesc "Run the HTTP API and all background work"))
        <> command "reproject" (info (pure Reproject) (progDesc "Rebuild the facts projection from observations"))
    )

-- | @--version@, so a bug report can name the build it came from. 'simpleVersioner' keeps
-- it a top-level flag only.
versionP :: Parser (a -> a)
versionP = simpleVersioner (showVersion version)

main :: IO ()
main = do
  cmd <-
    execParser
      ( info
          (commandP <**> versionP <**> helper)
          ( fullDesc
              <> progDesc
                "Reads Frigate events, asks a local vision model what each one shows, and \
                \writes the day up. Configured by environment (see .env.example) and by the \
                \in-app setup; this CLI only chooses which process to run."
              <> header "pet-report - a daily journal of how your pets are doing"
          )
      )
  case cmd of
    Serve -> withApp Web.runServer
    Reproject -> withApp $ \app -> do
      n <- Db.reprojectAll (appDb app)
      traceWith (startupTracer (appTracer app)) (Reprojected n)
