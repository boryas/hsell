import Control.Monad
import GHC.IO.Handle
import System.IO
import System.Process

-- TODO: create a Command typeclass that admits both commands and pipelines
-- doing so should help unify the various idiosyncracies like ($.), ($||), and ($<)
type Command = String
type Pipeline = [CreateProcess]

{- Functions and operators for creating and running pipelines -}
emptyPipe :: IO Pipeline
emptyPipe = return []

qProc :: Command -> CreateProcess
qProc = ((`ap` tail) . (. head)) proc . words

_pipe :: Pipeline -> CreateProcess -> IO Pipeline
_pipe [] newp = return [newp]
_pipe (p:ps) newp = do
        (pipe_rd, pipe_wr) <- createPipe
        --print . show $ (cmdspec p, cmdspec newp, pipe_rd, pipe_wr)
        let pi = newp { std_in = UseHandle pipe_rd }
            po = p { std_out = UseHandle pipe_wr } in
            return (pi:(po:ps))

pipe :: IO Pipeline -> CreateProcess -> IO Pipeline
pipe iopl newp = do
        line <- iopl
        _pipe line newp

instrumentedCreateProcess p = do
        print . show $ cmdspec p
        createProcess p

runPipeline :: IO Pipeline -> IO [(Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)]
runPipeline iopl = do
        pl <- iopl
        mapM createProcess (reverse pl)

(>%) = runPipeline

initPipeline :: Command -> IO Pipeline
initPipeline = pipe emptyPipe . qProc
($.) = initPipeline

stringPipe :: IO Pipeline -> Command -> IO Pipeline
stringPipe = (. qProc) . pipe
($|) = stringPipe

($|%) :: Command -> Command -> IO Pipeline
($|%) cmd1 cmd2 = (initPipeline cmd1) $| cmd2

($||) :: Command -> Command -> IO Pipeline
($||) = ($|) . initPipeline

{- Functions and operators for redirection -}
-- TODO: Handle "foo" > "bar", where foo is a cmd and bar is a file.
--       Currently, you have to init the pipe first.
data RedirStream = In | Out | Err

_redirect :: Pipeline -> RedirStream -> Handle -> IO Pipeline
_redirect [] _ _ = emptyPipe
_redirect [p] In hin =
        let redir = p { std_in = UseHandle hin } in
            return [redir]
_redirect (p:ps) Out hout =
        let redir = p { std_out = UseHandle hout } in
            return (redir:ps)
_redirect (p:ps) Err herr =
        let redir = p { std_err = UseHandle herr } in
            return (redir:ps)

redirect :: IO Pipeline -> RedirStream -> Handle -> IO Pipeline
redirect iopl stream hout = do
        pl <- iopl
        _redirect pl stream hout

($>) :: IO Pipeline -> String -> IO Pipeline
($>) iopl fout = do
        hout <- openFile fout WriteMode
        redirect iopl Out hout

-- N.B.: Input redirection only works on the first command in a pipeline
--       instead of breaking the pipeline "properly" like output.
--       Furthermore, only the syntax "cmd < file" is supported. Though, I am
--       partial to "< file cmd" in general.
($<) :: Command -> String -> IO Pipeline
($<) cmd fin = do
        hin <- openFile fin ReadMode
        redirect (initPipeline cmd) In hin

($?>) :: IO Pipeline -> String -> IO Pipeline
($?>) iopl fout = do
        herr <- openFile fout WriteMode
        redirect iopl Err herr

-- TODO: shorthand for sinking a pipeline before running it
_sink :: Pipeline -> Pipeline
_sink [] = []
_sink (p:ps) = (p { std_out = CreatePipe }):ps

sink :: IO Pipeline -> IO Pipeline
sink iopl = do
        pl <- iopl
        return (_sink pl)

_collect :: [(Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)] -> Maybe Handle
_collect procs =
        let out = last procs in
            case out of (_, Just hout, _, _) -> Just hout
                        _ -> Nothing

collect :: IO [(Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)] -> IO (Maybe Handle)
collect ioprocs = do
        procs <- ioprocs
        return $ _collect procs

-- TODO: MaybeT or traverse to make it pointfree, and return a Nothing instead of an error (?!)
(>%>) :: IO Pipeline -> IO String
(>%>) iopl = do
        (Just hout) <- collect . runPipeline . sink $ iopl
        hGetContents hout

main = do
        -- just one command --
        (>%) $ ($.) "ls -la" $> "ls-la.out"
        -- pipes and out redirect
        (>%) $ "ls -la" $|| "grep foo" $| "wc" $> "count-foo.out"
        -- pipe and in/out redirect
        (>%) $ "grep tmpfs" $< "/proc/mounts" $| "wc -l" $> "tmpfs-mounts.out"
        -- pipe and err/out redirect
        (>%) $ (($.) "dd if=/dev/zero bs=4k count=1") $?> "dd-err" $| "wc -l" $> "wc-dd-zeros.out"

        -- sink command into a string
        d <- (>%>) $ "ls -la" $|| "grep foo"
        putStrLn $ "sank cmd and got output:\n" ++ d ++ "(length " ++ (show . length $ d) ++ ")"
