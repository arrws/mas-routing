module Generators where

import qualified System.Random as R
import Control.Monad
import Control.Monad.State
import Data.List


data Human = Human { h_id   :: Int
                   , h_out  :: Int
                   , h_rate :: Int
                   }
                   deriving (Eq, Show)

data Router = Router    { r_id      :: Int
                        , r_outs    :: [Int]
                        , r_table   :: [(Int, Int)]
                        }
                        deriving (Eq, Show)

data Message' = Routing  { n_table   :: [(Int, Int)]
                        , n_source  :: Int
                        }
             | Ping     { m_msg     :: [Char]
                        , m_dest    :: Int
                        }
            deriving (Eq, Show)

data Message = Message { msg    :: Message'
                       , trace  :: [Char]
                       }
            deriving (Eq, Show)

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
gen_rand_sublist l n g = (x:l', g'')
                        where
                            (x, g') = gen_rand_elem l g
                            (l', g'') = gen_rand_sublist (substract x l) (n-1) g'




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

mapr :: ((a, R.StdGen) -> (b, R.StdGen)) -> [a] -> R.StdGen -> ([b], R.StdGen)
mapr f list g = foldr (\e (acc, g) -> let (e', g') = f (e, g)
                                      in (e':acc, g')) ([], g) list

gen_humans :: [Int] -> [Int] -> R.StdGen -> ([Human], R.StdGen)
gen_humans h_ids links g = (zipWith3 make_human h_ids links rates, g')
                            where
                                (rates, g') = mapr (\(_, g) -> gen_rand_num 0 5 g) h_ids g


gen_routers :: [Int] -> [[Int]] -> R.StdGen -> ([Router], R.StdGen)
gen_routers r_ids links g = (zipWith3 make_router r_ids links tables, g)
                            where
                                tables = zipWith init_table (repeat $ (length links -1)) links


gen_links :: Int -> [Int] -> [Int] -> R.StdGen -> ([Int], [[Int]], R.StdGen)
gen_links nlinks h_ids r_ids g = (h_links_out, r_links, g')
                                    where

                                        (h_con, g') = mapr (\(id, g) -> gen_rand_sublist r_ids 2 g) h_ids g
                                        h_links_out = map (!! 0) h_con
                                        h_links_in = map (!! 1) h_con

                                        base = [[] | _ <- r_ids]

                                        gen (-1) ids con = con
                                        gen i ids con
                                                | (length $ con !! i) < 2   = gen i ids (add_link i ids con)
                                                | otherwise                 = gen (i-1) ids con
                                                where
                                                    add_link i ids con = con'
                                                                        where
                                                                            id = r_ids !! i
                                                                            used_nodes = id : (con !! i)
                                                                            avaible_nodes = substract_all used_nodes r_ids
                                                                            (new_id, _) = gen_rand_elem avaible_nodes gimme_seed
                                                                            new_i = get_pos new_id r_ids
                                                                            con' = insertin id new_i $ insertin new_id i con

                                        insertin x i con = take i con ++ [x:(con !! i)] ++ drop (i+1) con

                                        vec = zip h_links_in h_ids

                                        -- ll = foldl (\l (i, x) -> insert x i l) ??? base vec
                                        ll = f vec
                                        f [] = base
                                        f ((x,y):xs) = insertin y (x - length h_ids)  (f xs)
                                        r_links = zipWith (\x y -> x ++ y) ll $ gen (length r_ids -1) r_ids base


get_pos :: Int -> [Int] -> Int
get_pos x l = get x (length l -1) l
            where
                get x (-1) l = -1
                get x i l
                            | l !! i == x   = i
                            | otherwise     = get x (i-1) l


substract :: Int -> [Int]-> [Int]
substract x l = filter (/=x) l

substract_all :: [Int] -> [Int]-> [Int]
substract_all [] l = l
substract_all (x:xs) l = substract_all xs $ substract x l



init_table :: Int -> [Int] -> [(Int, Int)]
init_table l out = map (f out) [0..l]

f :: [Int] -> Int -> (Int, Int)
f x i
    | elem i x  = (i, 1)
    | otherwise = (last x, nINF)


gimme_seed = R.mkStdGen 2

gen_agents :: Int -> Int -> ([Human], [Router])
gen_agents num_humans num_routers  = (humans, routers)
    where
        nlinks = 8

        ids = [0..(num_humans + num_routers-1)]
        h_ids = take num_humans ids
        r_ids = drop num_humans ids

        g = gimme_seed
        (h_links, r_links, _) = gen_links nlinks h_ids r_ids g
        (humans, _) = gen_humans h_ids h_links g
        (routers, _) = gen_routers r_ids r_links g


