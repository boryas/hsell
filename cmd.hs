import Control.Monad
import GHC.IO.Handle
import System.Process

type Pipeline = [CreateProcess]

nullPipeline :: IO Pipeline
nullPipeline = return []

qProc :: String -> CreateProcess
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

myCreateProcess p = do
        --print . show $ cmdspec p
        createProcess p

runPipeline :: IO Pipeline -> IO [(Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)]
runPipeline iopl = do
        pl <- iopl
        mapM createProcess (reverse pl)

(>%) = runPipeline

initPipeline :: String -> IO Pipeline
initPipeline = pipe nullPipeline . qProc

stringPipe :: IO Pipeline -> String -> IO Pipeline
stringPipe = (. qProc) . pipe
($|) = stringPipe

($|%) :: String -> String -> IO Pipeline
($|%) cmd1 cmd2 = (initPipeline cmd1) $| cmd2
-- foo cmd1 cmd2 = stringPipe (initPipeline cmd1) cmd2

($||) :: String -> String -> IO Pipeline
($||) = ($|) . initPipeline

main = do
        --($$) $ ($%) "ls -la" $| "grep foo" $| "wc"
        (>%) $ "ls -la" $|| "grep foo" $| "wc"
