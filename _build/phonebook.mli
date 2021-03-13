type phone_bst =
   |EmptyLeaf
   |TreeNode of string * string * phone_bst * phone_bst
   
val empty_tree: phone_bst
val add_contact: phone_bst -> string -> string -> phone_bst
val list_alphabetical: phone_bst -> (string * string) list
val get_contact: phone_bst -> string -> string * string
val new_right: phone_bst -> phone_bst
val new_left: phone_bst -> phone_bst
val right_replacement: phone_bst -> phone_bst -> phone_bst -> phone_bst
val left_replacement: phone_bst -> phone_bst -> phone_bst -> phone_bst
val remove_contact: phone_bst ->  string -> phone_bst



   