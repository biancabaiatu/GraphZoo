module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node a) = S.insert a S.empty
nodes (Overlay graph1 graph2) = S.union (nodes graph1) (nodes graph2)
nodes (Connect graph1 graph2) = S.union (nodes graph1) (nodes graph2)


{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty
edges (Node a) = S.empty
edges (Overlay graph1 graph2) = S.union (edges graph1) (edges graph2)
edges (Connect graph1 graph2) = S.union (edges graph1) (S.union (edges graph2) (S.cartesianProduct (nodes graph1) (nodes graph2)))

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node Empty = S.empty
outNeighbors node (Node a) = S.empty
outNeighbors node (Overlay graph1 graph2) = S.union (outNeighbors node graph1) (outNeighbors node graph2)
outNeighbors node (Connect graph1 graph2) = if S.member node (nodes graph1)
                                            then S.union (nodes graph2) (S.union (outNeighbors node graph1) (outNeighbors node graph2))
                                            else S.union (outNeighbors node graph1) (outNeighbors node graph2)
{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node Empty = S.empty
inNeighbors node (Node a) = S.empty
inNeighbors node (Overlay graph1 graph2) = S.union (inNeighbors node graph1) (inNeighbors node graph2)
inNeighbors node (Connect graph1 graph2) = if S.member node (nodes graph2)
                                            then S.union (nodes graph1) (S.union (inNeighbors node graph1) (inNeighbors node graph2))
                                            else S.union (inNeighbors node graph1) (inNeighbors node graph2)

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}


removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node Empty = Empty
removeNode node (Node a) = if (node == a) 
                            then Empty
                            else (Node a)
removeNode node (Overlay graph1 graph2) = (Overlay (removeNode node graph1) (removeNode node graph2))
removeNode node (Connect graph1 graph2) = (Connect (removeNode node graph1) (removeNode node graph2))

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}



splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news Empty = Empty
splitNode old news (Node a) = if (old == a)
                                then foldl (\acc node -> (Overlay acc (Node node))) Empty news
                                else (Node a)
splitNode old news (Overlay graph1 graph2) = (Overlay (splitNode old news graph1) (splitNode old news graph2))
splitNode old news (Connect graph1 graph2) = (Connect (splitNode old news graph1) (splitNode old news graph2))


{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node Empty = Empty
mergeNodes prop node (Node a) = if prop a
                                then (Node node)
                                else (Node a)
mergeNodes prop node (Overlay graph1 graph2) = (Overlay (mergeNodes prop node graph1) (mergeNodes prop node graph2))
mergeNodes prop node (Connect graph1 graph2) = (Connect (mergeNodes prop node graph1) (mergeNodes prop node graph2))

