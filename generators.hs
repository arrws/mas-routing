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
                       , trc  :: [Char]
                       }
            deriving (Eq, Show)


symHuman = " |"
symRouter = " "

nINF = 666

gimme_seed = R.mkStdGen 0


gen_rand_num :: Int -> Int -> State R.StdGen Int
gen_rand_num lower upper = do
                            g <- get
                            (x, g') <- return $ R.randomR (lower, upper-1) g
                            put g'
                            return x


gen_rand_elem :: [Int] -> State R.StdGen Int
-- gen_rand_elem l = gen_rand_num 0 (length l) >>= \i -> return $ l!!i
gen_rand_elem l = do
                    i <- gen_rand_num 0 (length l)
                    return $ l!!i



gen_rand_sublist :: [Int] -> Int -> State R.StdGen [Int]
gen_rand_sublist l 0 = return []
-- gen_rand_sublist l n = gen_rand_elem l >>= \x -> gen_rand_sublist (substract x l) (n-1) >>= \l' -> return $ x:l'
gen_rand_sublist l n = do
                        x <- gen_rand_elem l
                        l' <- gen_rand_sublist (substract x l) (n-1)
                        return $ x:l'




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



gen_humans :: [Int] -> [Int] -> [Human]
gen_humans h_ids links = zipWith3 build_human h_ids links rates
                            where
                                (rates, _) = mapr (gen_rand_num 1 5) h_ids gimme_seed


gen_routers :: [Int] -> [[Int]] -> [Router]
gen_routers r_ids links = (zipWith3 build_router r_ids links tables)
                            where
                                tables = zipWith init_table (repeat $ last r_ids) links -- last r_ids ~ num_agents



init_table :: Int -> [Int] -> [(Int, Int)]
init_table n links = map (fn links) [0..n]
                where
                    fn :: [Int] -> Int -> (Int, Int)
                    fn x i
                        | elem i x  = (i, 1)
                        | otherwise = (last x, nINF) -- default route



mapr :: State R.StdGen a -> [a] -> R.StdGen -> ([a], R.StdGen)
mapr f list = runState ( do
                            g <- get
                            let
                                l = foldr (\_ (acc, g) -> let (x, g') = runState f g in (x:acc, g')) ([], g) list
                            put $ snd l
                            return $ fst l
                       )
-- mapr f list = runState ( do
--                             g <- get
--                             let l = foldr (\_ acc -> let x = fst $ runState f g in x:acc) [] list
--                             return l
--                        )



gen_links :: [Int] -> [Int] -> ([Int], [[Int]])
-- gen_links x y z = (([],[]), gimme_seed)
gen_links h_ids r_ids = (h_links, r_links)
                        where
                            (h_links, _) = mapr (gen_rand_elem r_ids) h_ids gimme_seed

                            base = [[] | _ <- r_ids]
                            r_indexes = map index h_links
                            r_links_to_humans = foldr (\(x, y) l-> insert_into y x l) base $ zip r_indexes h_ids
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
                                                (e', _) = runState (gen_rand_elem free_nodes) gimme_seed
                                                i' = index e'

                            -- --         0, 1, 2, 3, 4, 5, ...
                            -- h_links = [3, 7, 8]
                            -- r_links = [ [4, 5, 0] -- 3
                            --           , [3, 6] -- 4
                            --           , [3, 7, 8] -- 5
                            --           , [4, 7] -- 6
                            --           , [5, 6, 8, 1] -- 7
                            --           , [7, 5, 2] -- 8
                            --           ]

insert_into :: Int -> Int -> [[Int]] -> [[Int]]
insert_into x i links = take i links ++ [x:(links !! i)] ++ drop (i+1) links

substract :: Int -> [Int]-> [Int]
substract x l = filter (/=x) l

substract_all :: [Int] -> [Int]-> [Int]
substract_all [] l = l
substract_all (x:xs) l = substract_all xs $ substract x l



