import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified System.Random as R
import Control.Monad
import Control.Concurrent.Async

import Control.Concurrent (threadDelay)

data Human = Human { hname :: Int
                   , houts :: [Int]
                   , hhandle :: (Output Message, Input Message)
                   }

data Router = Router { rname :: Int
                     , routs :: [Int]
                     , rhandle :: (Output Message, Input Message)
                     }

data Message = Message { mname :: Int
                       , len :: Int
                       , msg :: [Char]
                       , history :: [Char]
                       }

class Name x where
    name :: x -> Int
instance Name Human where name = hname
instance Name Router where name = rname

class Handle x where
    handle :: x -> (Output Message, Input Message)
instance Handle Human where handle = hhandle
instance Handle Router where handle = rhandle

class Outs x where
    outs :: x -> [Int]
instance Outs Human where outs = houts
instance Outs Router where outs = routs

delayThread t = lift $ threadDelay (t * 100000)

getMessage = forever $ do
            delayThread 4
            let m = Message { msg = "fuck is the message" , history = "|"}
            yield m

passon = forever $ do
            delayThread 3
            m <- await
            yield $ msg m

printMessage = forever $ do
                delayThread 1
                m <- await
                lift $ print $ msg m ++ "    " ++ history m

signMessage n = forever $ do
                delayThread 1
                m <- await
                yield $ m { history = (history m) ++ "-> " ++ show n}


human_send h r_writer   = runEffect $ getMessage >-> signMessage (name h) >-> toOutput r_writer
human_recv h h_reader   = runEffect $ fromInput h_reader >-> signMessage (name h) >-> printMessage
router_route r r_reader h_writers = runEffect $ fromInput r_reader >-> signMessage (name r) >-> toOutput (h_writers !! 2)


main = do
    let
        routers = [ Router { rname = x +10
                           , routs = [10 + mod (x+1) 3, x, x+5]
                           } | x <- [0..4]]
        humans = [ Human { hname = x
                         , houts = [10 + mod x 5]
                         } | x <- [0..9]]
        writer = fst
        reader = snd

    print $ map outs humans
    print $ map outs routers

    pipes <- sequence $ map (\_ -> spawn unbounded) [0..15]

    h_recv_tasks <- sequence $ [async $ human_recv (humans !! i) (reader $ pipes !! i) | i <-[0..10] ]
    h_send_tasks <- sequence $ [async $ human_send (humans !! i) (writer $ pipes !! r) | [r] <- map outs humans, i <-[0..10] ]
    r_route_tasks <- sequence $ [async $ router_route (routers !! (i-10)) (reader $ pipes !! i) [writer $ pipes !! j | j <- friends] | (i, friends) <- zip [10..15] $ map outs routers]

    waitAny [h_recv_tasks !! 0]

    -- let h = humans !! 0
    -- let r = routers !! 0
    -- (h_writer, h_reader) <- spawn unbounded
    -- (r_writer, r_reader) <- spawn unbounded
    -- h_recv_Task <- async $ human_recv h h_reader
    -- h_send_Task <- async $ human_send h r_writer
    -- r_route_Task <- async $ router_route r r_reader h_writer
    -- waitAny [h_recv_Task, h_send_Task]

