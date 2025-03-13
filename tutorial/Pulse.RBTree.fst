module Pulse.RBTree

open Pulse.Lib.Pervasives

module T = RBTree

noeq
type node = {
    col : T.color;
    key : nat;
    left : tree_t;
    right : tree_t;
}

and node_ptr = ref (node)
//A nullable pointer to a node
and tree_t  = option (node_ptr)

let rec is_tree  (ct:tree_t) (ft:T.rbtree')
    : Tot vprop (decreases ft) =
  match ft with
  | T.E -> pure (ct == None)
  | T.T  col left' key right' ->
            exists* (p:node_ptr) (lct:tree_t) (rct:tree_t) (colr:T.color).
            pure (ct == Some p) **
            pts_to p {col = colr; left = lct; key = key; right = rct} **
            is_tree lct left' **
            is_tree rct right'
  

  
   