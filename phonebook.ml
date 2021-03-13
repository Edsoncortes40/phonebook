(*Phonebook Binary Search tree Record type, option of EmptyLeaf or TreeNode of tuple (name, number, leftTree, rightTree) *)
type phone_bst =
   |EmptyLeaf
   |TreeNode of string * string * phone_bst * phone_bst

let empty_tree = EmptyLeaf

(*adds a contact node to the phonebook binary search tree)
let rec add_contact tree contact number =
   match tree with
   |EmptyLeaf -> TreeNode(contact, number, EmptyLeaf, EmptyLeaf)
   |TreeNode(name, phone, left, right) -> if (contact < name) then TreeNode(name, phone, add_contact left contact number, right)
                                                              else TreeNode(name, phone, left, add_contact right contact number)

(*returns a list of all the contacts and their phone numbers in alphabetical order*)
let rec list_alphabetical tree =
   match tree with
   |EmptyLeaf -> []
   |TreeNode(name, phone, left, right) -> list_alphabetical left @ [(name, phone)] @ list_alphabetical right

(*finds the contact and returns their phonebook data as a tuple (name, phone number), if contact isnt found then message is returned in tuple*)
let rec get_contact tree contact =
   match tree with
   |EmptyLeaf -> ("Name not found", "No number for this name!")
   |TreeNode(name, phone, left, right) -> if name = contact then (name, phone)
                                                    else if contact < name then get_contact left contact
                                                    else get_contact right contact

(*helper function to help get replacement right tree when removing a node*)
let rec new_right root =
   match root with
   |EmptyLeaf -> EmptyLeaf
   |TreeNode(name, phone, EmptyLeaf, EmptyLeaf) -> EmptyLeaf
   |TreeNode(name, phone, EmptyLeaf, right) -> right
   |TreeNode(name, phone, left, right) -> TreeNode(name, phone, new_right left, right) 

(*helper function to help get replacement left tree when removing a node*)
let rec new_left root =
   match root with
   |EmptyLeaf -> EmptyLeaf
   |TreeNode(name, phone, EmptyLeaf, EmptyLeaf) -> EmptyLeaf
   |TreeNode(name, phone, left, EmptyLeaf) -> left
   |TreeNode(name, phone, left, right) -> TreeNode(name, phone, left, new_left right)

(*finds and returns a phone bst, that will replace the to-be-deleted node. Finds the left-most node in the right subtree to replace*)
let rec right_replacement root newLeft newRight =
   match root with
   |EmptyLeaf -> EmptyLeaf
   |TreeNode(name, phone, EmptyLeaf, EmptyLeaf) -> TreeNode(name, phone, newLeft, newRight)
   |TreeNode(name, phone, EmptyLeaf, right) -> TreeNode(name, phone, newLeft, newRight)
   |TreeNode(name, phone, left, right) -> right_replacement left newLeft newRight

(*finds and returns a phone bst that will replace the to-be-deleted node. Finds the right-most node in the left subtree to replace*)
let rec left_replacement root newLeft newRight =
   match root with
   |EmptyLeaf -> EmptyLeaf
   |TreeNode(name, phone, EmptyLeaf, EmptyLeaf) -> TreeNode(name, phone, newLeft, newRight)
   |TreeNode(name, phone, left, EmptyLeaf) -> TreeNode(name, phone, newLeft, newRight)
   |TreeNode(name, phone, left, right) -> left_replacement right newLeft newRight

(*
 uses left_replacement/right_replacement functions and the new_right/new_left functions to find a contact in tree, and deletes it.
 Deletion makes sure to keep the properties of the binary search tree.
 If no contact in tree with this name, then tree is left as is.
*)
let rec remove_contact tree contact =
   match tree with
   |EmptyLeaf -> EmptyLeaf
   |TreeNode(name, phone, EmptyLeaf, right) -> if name = contact then (right_replacement right EmptyLeaf (new_right right)) 
                                               else if contact < name then tree 
                                               else TreeNode(name, phone, EmptyLeaf, remove_contact right contact)
   |TreeNode(name, phone, left, EmptyLeaf) -> if name = contact then (left_replacement left (new_left left) EmptyLeaf)
                                              else if contact < name then TreeNode(name, phone, remove_contact left contact, EmptyLeaf)
                                              else tree
   |TreeNode(name, phone, left, right) -> if name = contact then (right_replacement right left (new_right right))
                                              else if contact < name then TreeNode(name, phone, remove_contact left contact, right)
                                              else TreeNode(name, phone, left, remove_contact right contact)
 
