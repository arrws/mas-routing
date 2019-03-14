module Generators where

import qualified System.Random as R
import Control.Monad


data Human = Human { h_id   :: Int
                   , h_out  :: Int
                   , h_rate :: Int
                   }

data Router = Router    { r_id      :: Int
                        , r_outs    :: [Int]
                        , r_table   :: [(Int, Int)]
                        }

data Message = Message { m_msg      :: [Char]
                       , m_trace    :: [Char]
                       , m_dest     :: Int
                       }


gen_rand_num :: Int -> Int -> R.StdGen -> (Int, R.StdGen)
gen_rand_num x y g = R.randomR (x, y-1) g

gen_rand_elem :: [Int] -> R.StdGen -> (Int, R.StdGen)
gen_rand_elem l g = (l !! i, g')
                    where (i, g') = gen_rand_num 0 (length l) g

gen_rand_sublist :: [Int] -> Int -> R.StdGen -> ([Int], R.StdGen)
gen_rand_sublist l max_links g = work l g n
                       where
                            (n, _) = gen_rand_num 2 max_links g
                            work l g 0 = ([], g)
                            work l g n = (x:l', g'')
                                where
                                    (x, g') = gen_rand_elem l g
                                    (l', g'') = work (substract x l) g' (n-1)

gen_routers :: Int -> [Int] -> [Int] -> Int -> R.StdGen -> ([Router], R.StdGen)
gen_routers 0 names l max_links g = ([], g)
gen_routers n names l max_links g = (Router { r_id = head names
                                                 , r_outs = rts
                                                 , r_table = [ (x, 666) | (_, x) <- zip names (cycle rts) ]
                                                 }
                                                 :l', g'')
                                        where
                                            (rts, g') = gen_rand_sublist (substract (head names) l) max_links g
                                            (l', g'') = gen_routers (n-1) (tail names) l max_links g'

gen_humans :: Int -> [Int] -> [Int] -> R.StdGen -> ([Human], R.StdGen)
gen_humans 0 names l g = ([], g)
gen_humans n names l g = (Human { h_id = head names
                                     , h_out = hts
                                     , h_rate = delay
                                     }
                                     :l', g'')
                            where
                                (hts, g') = gen_rand_elem (substract (head names) l) g
                                (delay, _) = gen_rand_num 0 5 g
                                (l', g'') = gen_humans (n-1) (tail names) l g'

substract x l = filter (/=x) l

gen_agents :: Int -> Int -> ([Human], [Router])
gen_agents num_humans num_routers  = (humans, routers)
    where
        max_links = 4   -- max router links

        names = [0..(num_humans + num_routers-1)]
        h_names = take num_humans names
        r_names = drop num_humans names

        g = R.mkStdGen 0
        (humans, _) = gen_humans num_humans h_names r_names g
        (routers, _) = gen_routers num_routers r_names names max_links g


