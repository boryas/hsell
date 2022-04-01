import GHC.IO.Handle
import System.Process

type Pipeline = [CreateProcess]

initPipeline :: IO Pipeline
initPipeline = return []

purePipe :: Pipeline -> CreateProcess -> IO Pipeline
purePipe [] newp = return [newp]
purePipe (p:ps) newp = do
        (pipe_rd, pipe_wr) <- createPipe
        --print . show $ (cmdspec p, cmdspec newp, pipe_rd, pipe_wr)
        let pi = newp { std_in = UseHandle pipe_rd }
            po = p { std_out = UseHandle pipe_wr } in
            return (pi:(po:ps))

pipe :: IO Pipeline -> CreateProcess -> IO Pipeline
pipe iopl newp = do
        line <- iopl
        purePipe line newp

myCreateProcess p = do
        --print . show $ cmdspec p
        createProcess p

runPipeline :: IO Pipeline -> IO [(Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)]
runPipeline iopl = do
        pl <- iopl
        mapM createProcess (reverse pl)

main = do
        runPipeline $ initPipeline `pipe` (proc "ls" ["/usr/local/bin"]) `pipe` (proc "wc" ["-l"]) `pipe` (proc "wc" []) `pipe` (proc "wc" [])
