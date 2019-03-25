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

data Router = Router { r_id      :: Int
                     , r_outs    :: [Int]
                     , r_table   :: [(Int, Int)]
                     }
            deriving (Eq, Show)

data Message' = Routing { n_table   :: [(Int, Int)]
                        , n_source  :: Int
                        }
              | Ping    { m_msg     :: [Char]
                        , m_dest    :: Int
                        }
            deriving (Eq, Show)

data Message = Message { msg    :: Message'
                       , trace  :: [Char]
                       }
            deriving (Eq, Show)


symHuman = " |"
symRouter = " "

nINF = 999


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



build_router :: Int -> [Int] -> [(Int,Int)] -> Router
build_router id outs table = Router { r_id = id
                                    , r_outs = outs
                                    , r_table = table
                                    }

build_human :: Int -> Int -> Int -> Human
build_human id out r = Human { h_id = id
                             , h_out = out
                             , h_rate = r
                             }

mapr :: ((a, R.StdGen) -> (b, R.StdGen)) -> [a] -> R.StdGen -> ([b], R.StdGen)
mapr f list g = foldr (\e (acc, g) -> let (e', g') = f (e, g)
                                      in (e':acc, g')) ([], g) list


gen_humans :: [Int] -> [Int] -> R.StdGen -> ([Human], R.StdGen)
gen_humans h_ids links g = (zipWith3 build_human h_ids links rates, g')
                            where
                                (rates, g') = mapr (\(_, g) -> gen_rand_num 0 5 g) h_ids g


gen_routers :: [Int] -> [[Int]] -> R.StdGen -> ([Router], R.StdGen)
gen_routers r_ids links g = (zipWith3 build_router r_ids links tables, g)
                            where
                                tables = zipWith init_table (repeat $ last r_ids) links -- last r_ids ~ num_agents

init_table :: Int -> [Int] -> [(Int, Int)]
init_table n links = map (fn links) [0..n]
                where
                    fn :: [Int] -> Int -> (Int, Int)
                    fn x i
                        | elem i x  = (i, 1)
                        | otherwise = (last x, nINF) -- default route

gen_links :: Int -> [Int] -> [Int] -> R.StdGen -> ([Int], [[Int]], R.StdGen)
gen_links nlinks h_ids r_ids g = (h_links_out, r_links, g')
                                    where
                                        (h_links_out, g') = mapr (\(id, g) -> gen_rand_elem r_ids g) h_ids g
                                        (h_links_in, g'') = mapr (\(id, g) -> gen_rand_elem r_ids g) h_ids g'

                                        base = [[] | _ <- r_ids]
                                        r_indexes = map index h_links_in
                                        r_links_to_humans = foldl (\l (x, y) -> insert_into y x l) base $ zip r_indexes h_ids
                                        r_links_to_routers = foldr add_edges base [0..(length r_ids)-1]
                                        r_links = zipWith (++) r_links_to_humans r_links_to_routers

                                        index x = x - length h_ids

                                        add_edges i links = f i links (length $ links!!i)
                                                        where
                                                            f i links 0 = add_edge i $ add_edge i links
                                                            f i links 1 = add_edge i links
                                                            f i links _ = links

                                        add_edge i links = insert_into e i' $ insert_into e' i links
                                                        where
                                                            e = r_ids !! i
                                                            used_nodes = e:(links!!i)
                                                            free_nodes = substract_all used_nodes r_ids
                                                            (e', _) = gen_rand_elem free_nodes gimme_seed
                                                            i' = index e'

insert_into :: Int -> Int -> [[Int]] -> [[Int]]
insert_into x i links = take i links ++ [x:(links !! i)] ++ drop (i+1) links

substract :: Int -> [Int]-> [Int]
substract x l = filter (/=x) l

substract_all :: [Int] -> [Int]-> [Int]
substract_all [] l = l
substract_all (x:xs) l = substract_all xs $ substract x l


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


