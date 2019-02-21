import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified System.Random as R
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)


data Human = Human { hname :: Int
                   , houts :: [Int]
                   , gen_rate :: Int

                   }

data Router = Router { rname :: Int
                     , routs :: [Int]
                     , table :: [(Int, Int)]
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

class Outs x where
    outs :: x -> [Int]
instance Outs Human where outs = houts
instance Outs Router where outs = routs


delayThread t = lift $ threadDelay (t * 100000)

getMessage h = forever $ do
            delayThread (gen_rate h)
            let m = Message { msg = "fuck is the message" , history = "|"}
            yield m

printMessage = forever $ do
                delayThread 1
                m <- await
                lift $ print $ msg m ++ "    " ++ history m

signMessage n = forever $ do
                delayThread 1
                m <- await
                yield $ m { history = (history m) ++ "-> " ++ show n}

human_send h r_writer   = runEffect $ getMessage h
                        >-> signMessage (name h)
                        >-> toOutput r_writer

human_recv h h_reader   = runEffect $ fromInput h_reader
                        >-> signMessage (name h)
                        >-> printMessage

router_route r r_reader h_writers = runEffect $ fromInput r_reader
                                    >-> signMessage (name r)
                                    >-> toOutput (h_writers !! 0)

get_rand_num :: Int -> Int -> R.StdGen -> (Int, R.StdGen)
get_rand_num x y g = R.randomR (x, y-1) g

get_rand_elem :: [Int] -> R.StdGen -> (Int, R.StdGen)
get_rand_elem l g = (l !! i, g')
                    where (i, g') = get_rand_num 0 (length l) g

get_rand_sublist :: [Int] -> R.StdGen -> ([Int], R.StdGen)
get_rand_sublist l g = work l g n
                   where
                       (n, _) = get_rand_num 1 (length l) g
                       work l g 0 = ([], g)
                       work l g n = (x:l', g'')
                           where
                               (x, g') = get_rand_elem l g
                               (l', g'') = work (substract x l) g' (n-1)

substract x l = filter (/= x) l

generate_routers :: Int -> [Int] -> [Int] -> R.StdGen -> ([Router], R.StdGen)
generate_routers 0 names l g = ([], g)
generate_routers n names l g = (Router { rname = head names
                                       , routs = rts
                                       }
                                       :l', g'')
                            where
                                (rts, g') = get_rand_sublist (substract (head names) l) g
                                (l', g'') = generate_routers (n-1) (tail names) l g'

generate_humans :: Int -> [Int] -> [Int] -> R.StdGen -> ([Human], R.StdGen)
generate_humans 0 names l g = ([], g)
generate_humans n names l g = (Human { hname = head names
                                     , houts = hts
                                     , gen_rate = 2
                                     }
                                     :l', g'')
                            where
                                (hts, g') = get_rand_sublist (substract (head names) l) g
                                (l', g'') = generate_humans (n-1) (tail names) l g'

main = do

    let
        num_humans = 2
        num_routers = 3
        num = num_humans + num_routers
        g = R.mkStdGen 0

        names = [x | x <- [1 .. num_humans + num_routers]]
        h_names = take num_humans names
        r_names = drop num_humans names

        writer = fst
        reader = snd

        -- routers = [ Router { rname = x +10
        --                    , routs = [10 + mod (x+1) 3, x, x+5]
        --                    } | x <- [0..4]]
        -- humans = [ Human { hname = x
        --                  , houts = [10 + mod x 5]
        --                  , gen_rate = 2
        --                  } | x <- [0..9]]

    -- print $ map outs humans
    -- print $ map outs routers
    -- pipes <- sequence $ map (\_ -> spawn unbounded) [0..15]
    -- h_recv_tasks <- sequence $ [async $ human_recv (humans !! i) (reader $ pipes !! i) | i <-[0..9] ]
    -- h_send_tasks <- sequence $ [async $ human_send (humans !! i) (writer $ pipes !! r) | [r] <- map outs humans, i <-[0..9] ]
    -- r_route_tasks <- sequence $ [async $ router_route (routers !! (i-10)) (reader $ pipes !! i) [writer $ pipes !! j | j <- friends] | (i, friends) <- zip [10..14] $ map outs routers]

        (routers, _) = generate_routers num_routers r_names names g
        (humans, _) = generate_humans num_humans h_names r_names g

    -- print $ get_rand_sublist [0,1,2,3,4,5,6,7,8,9] g

    print $ h_names
    print $ r_names
    print $ map outs humans
    print $ map outs routers

    pipes <- sequence $ map (\_ -> spawn unbounded) [0..num]

    h_recv_tasks <- sequence $ [async $ human_recv (humans !! i) (reader $ pipes !! i) | i <-[0..(num_humans-1)] ]
    h_send_tasks <- sequence $ [async $ human_send (humans !! i) (writer $ pipes !! r) | [r] <- map outs humans, i <-[0..(num_humans-1)] ]
    r_route_tasks <- sequence $ [async $ router_route (routers !! (i-num_humans-1)) (reader $ pipes !! i) [writer $ pipes !! j | j <- friends] | (i, friends) <- zip [(num_humans+1)..num] $ map outs routers]

                                        -- router_route r r_reader h_writers = runEffect $ fromInput r_reader


    waitAny h_send_tasks

    -- let h = humans !! 0
    -- let r = routers !! 0
    -- (h_writer, h_reader) <- spawn unbounded
    -- (r_writer, r_reader) <- spawn unbounded
    -- h_recv_Task <- async $ human_recv h h_reader
    -- h_send_Task <- async $ human_send h r_writer
    -- r_route_Task <- async $ router_route r r_reader h_writer
    -- waitAny [h_recv_Task, h_send_Task]


