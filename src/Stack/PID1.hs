module Stack.PID1
    ( run
    ) where

-- This is a valid PID 1 process in Haskell, intended as a Docker
-- entrypoint. It will handle reaping orphans and handling TERM and
-- INT signals. This is a work in progress, please do not use it in
-- production!

import           Control.Concurrent       (forkIO, newEmptyMVar, takeMVar,
                                           threadDelay, tryPutMVar)
import           Control.Exception        (assert, catch, throwIO)
import           Control.Monad            (forever, void)
import           System.Exit              (ExitCode (ExitFailure), exitWith)
import           System.IO.Error          (isDoesNotExistError)
import           System.Posix.Process     (ProcessStatus (..), exitImmediately,
                                           getAnyProcessStatus)
import           System.Posix.Signals     (Handler (Catch), Signal,
                                           installHandler, sigINT, sigKILL,
                                           sigTERM, signalProcess)
import           System.Posix.Types       (CPid)
import           System.Process           (createProcess, env, proc)
import           System.Process.Internals (ProcessHandle__ (..),
                                           modifyProcessHandle)

run :: FilePath -- ^ command
    -> [String] -- ^ arguments
    -> Maybe [(String, String)] -- ^ env override
    -> IO a
run cmd args environment = do
    (Nothing, Nothing, Nothing, ph) <- createProcess (proc cmd args)
        { env = environment
        }

    -- Determine the child PID. We want to exit once this child
    -- process is dead.
    p_ <- modifyProcessHandle ph $ \p_ -> return (p_, p_)
    child <-
        case p_ of
            ClosedHandle e -> assert False (exitWith e)
            OpenHandle pid -> return pid

    -- Set up an MVar to indicate we're ready to start killing all
    -- children processes. Then start a thread waiting for that
    -- variable to be filled and do the actual killing.
    killChildrenVar <- newEmptyMVar
    _ <- forkIO $ do
        takeMVar killChildrenVar
        killAllChildren

    -- Helper function to start killing, used below
    let startKilling = void $ tryPutMVar killChildrenVar ()

    -- Install signal handlers for TERM and INT, which will start
    -- killing all children
    void $ installHandler sigTERM (Catch startKilling) Nothing
    void $ installHandler sigINT  (Catch startKilling) Nothing

    -- Loop on reaping child processes
    reap startKilling child

reap :: IO () -> CPid -> IO a
reap startKilling child = do
    -- Track the ProcessStatus of the child
    childStatus <- newEmptyMVar

    -- Keep reaping one child. Eventually, when all children are dead,
    -- we'll get an exception. We catch that exception and, assuming
    -- it's the DoesNotExistError we're expecting, know that all
    -- children are dead and exit.
    forever (reapOne childStatus) `catch` \e ->
        if isDoesNotExistError e
            -- no more child processes
            then do
                takeMVar childStatus >>= exitImmediately . toExitCode
                error "This can never be reached"
            -- some other exception occurred, reraise it
            else throwIO e
  where
    reapOne childStatus = do
        -- Block until a child process exits
        mres <- getAnyProcessStatus True False
        case mres of
            -- This should never happen, if there are no more child
            -- processes we'll get an exception instead
            Nothing -> assert False (return ())
            -- Got a new dead child. If it's the child we created in
            -- main, then start killing all other children. Otherwise,
            -- we're just reaping.
            Just (pid, status)
                | pid == child -> do
                    -- Take the first status of the child. It's possible -
                    -- however unlikely - that the process ID could end up
                    -- getting reused and there will be another child exiting
                    -- with the same PID. Just ignore that.
                    void $ tryPutMVar childStatus status
                    startKilling
                | otherwise -> return ()

killAllChildren :: IO ()
killAllChildren = do
    -- Output optional, and probably a bad idea in practice. Fun
    -- though for testing.
    -- putStrLn "Sending all processes the TERM signal"

    -- Send all children processes the TERM signal
    signalProcess sigTERM (-1) `catch` \e ->
        if isDoesNotExistError e
            then return ()
            else throwIO e

    -- Wait for five seconds. We don't need to put in any logic about
    -- whether there are still child processes; if all children have
    -- exited, then the reap loop will exit and our process will shut
    -- down.
    threadDelay $ 5 * 1000 * 1000

    -- OK, some children didn't exit. Now time to get serious!
    -- putStrLn "Sending all processes the KILL signal"
    signalProcess sigKILL (-1) `catch` \e ->
        if isDoesNotExistError e
            then return ()
            else throwIO e

toExitCode :: ProcessStatus -> ExitCode
toExitCode (Exited ec) = ec
toExitCode (Terminated sig _) = signalToEC sig
toExitCode (Stopped sig) = signalToEC sig

signalToEC :: Signal -> ExitCode
signalToEC sig = ExitFailure (fromIntegral sig + 128)
