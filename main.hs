import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified System.Random as R
import Control.Monad
import Control.Concurrent.Async

import Control.Concurrent (threadDelay)

data Human = Human { hname :: Int
                   }

data Router = Router { rname :: Int
                     }

data Message = Message { mname :: Int
                       , len :: Int
                       , msg :: [Char]
                       , history :: [Char]
                       }

delayThread t = lift $ threadDelay (t * 100000)

getMessage = forever $ do
            delayThread 4
            let m = Message { msg = "fuck is the message" , history = "|"}
            yield m

getShit = forever $ do
            delayThread 5
            let m = Message { msg = "... show me the way" , history = "|"}
            yield m

passon = forever $ do
            m <- await
            lift $ threadDelay (100000)
            yield $ msg m

printMessage = forever $ do
                m <- await
                lift $ print $ msg m ++ "    " ++ history m

signMessage n = forever $ do
                m <- await
                yield $ m { history = (history m) ++ "-> " ++ show n}


human_send h r_writer   = runEffect $ getMessage >-> toOutput r_writer

human_recv h h_reader   = runEffect $ fromInput h_reader >-> printMessage

-- router_send r h_writer  = runEffect $ getShit >-> toOutput h_writer
-- router_recv r r_reader  = runEffect $ fromInput r_reader >-> signMessage (rname r) >-> printMessage

router_route r r_reader h_writer = runEffect $ fromInput r_reader >-> signMessage (rname r) >-> toOutput h_writer

main = do

    (r_writer, r_reader) <- spawn unbounded
    (h_writer, h_reader) <- spawn unbounded

    let
        h = Human { hname = 5}
        r = Router { rname = 2}

    h_recv_Task <- async $ human_recv h h_reader
    h_send_Task <- async $ human_send h r_writer

    -- r_recv_Task <- async $ router_recv r r_reader
    -- r_send_Task <- async $ router_send r h_writer
    r_route_Task <- async $ router_route r r_reader h_writer

    print $ "goooooo"
    waitAny [h_recv_Task, h_send_Task]


