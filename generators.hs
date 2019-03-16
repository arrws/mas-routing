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

symHuman = " |"
symRouter = " "

nINF = 9999

gen_rand_num :: Int -> Int -> R.StdGen -> (Int, R.StdGen)
gen_rand_num lower upper g = R.randomR (lower, upper-1) g

gen_rand_elem :: [Int] -> R.StdGen -> (Int, R.StdGen)
gen_rand_elem l g = (l !! i, g')
                    where (i, g') = gen_rand_num 0 (length l) g

gen_rand_sublist :: [Int] -> Int -> R.StdGen -> ([Int], R.StdGen)
gen_rand_sublist l 0 g = ([], g)
gen_rand_sublist l n g = (x:l', g')
                        where
                            (x, _) = gen_rand_elem l g
                            (l', g') = gen_rand_sublist (substract x l) (n-1) g


substract x l = filter (/=x) l

gen_generators :: Int -> R.StdGen -> [R.StdGen]
gen_generators 0 _ = []
gen_generators n g = g':gen_generators (n-1) g'
                    where
                        (_, g') = R.randomR (2,4) g :: (Integer, R.StdGen)
gen_generators _ _ = []


make_router :: Int -> [Int] -> [(Int,Int)] -> Router
make_router id outs table = Router { r_id = id
                                   , r_outs = outs
                                   , r_table = table
                                   }

make_human :: Int -> Int -> Int -> Human
make_human id out r = Human { h_id = id
                            , h_out = out
                            , h_rate = r
                            }


gen_humans :: [Int] -> [Int] -> R.StdGen -> ([Human], R.StdGen)
gen_humans ids outs g = (zipWith3 make_human ids outss rates, last gs)
                        where
                            gs = gen_generators (length ids) g
                            lists = map (flip substract outs) ids
                            outss = map fst $ zipWith gen_rand_elem lists gs
                            rates = map (fst . gen_rand_num 0 5) gs


gen_routers :: [Int] -> [Int] -> Int -> R.StdGen -> ([Router], R.StdGen)
gen_routers ids outs max_links g = (zipWith3 make_router ids outss tables, last gs)
-- gen_routers ids outs max_links g = ([], last gs)
                                    where
                                        gs = gen_generators (length ids) g
                                        ns = map (fst . gen_rand_num 1 max_links) gs
                                        -- ns = replicate (length ids) 2
                                        lists = map (flip substract outs) ids
                                        outss = map fst $ zipWith3 gen_rand_sublist lists ns gs
                                        -- outss = replicate (length ids) []
                                        table = replicate (length ids) (head outs, nINF)
                                        tables = replicate (length ids) table



gen_agents :: Int -> Int -> ([Human], [Router])
gen_agents num_humans num_routers  = (humans, routers)
    where
        max_links = 1   -- max router links

        ids = [0..(num_humans + num_routers-1)]
        h_ids = take num_humans ids
        r_ids = drop num_humans ids

        g = R.mkStdGen 0
        (humans, _) = gen_humans h_ids r_ids g
        (routers, _) = gen_routers r_ids ids max_links g


