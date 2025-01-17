module Main (main) where
import Control.Monad.IO.Class (MonadIO (..))
import HW5.Action (HIO (runHIO), HiPermission (..), setOf)
import HW5.Evaluator (eval)
import HW5.Parser (parse)
import HW5.Pretty (prettyValue)
import System.Console.Haskeline (defaultSettings, getInputLine, outputStrLn, runInputT)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings hiRepl
  where
    hiRepl = do
      input <- getInputLine "hi> "
      case input of
        Nothing -> return ()
        Just "quit" -> return ()
        Just expr -> do
          case parse expr of
            Left pError -> outputStrLn (errorBundlePretty pError)
            Right pSuccess -> do
              let permissions = setOf [AllowRead, AllowWrite, AllowTime]
              evaluated <- liftIO (runHIO (eval pSuccess) permissions)
              case evaluated of
                Left evalError    -> outputStrLn (show evalError)
                Right evalSuccess -> outputStrLn (show (prettyValue evalSuccess))
          hiRepl
