module BinarySearchTree

type TreeNode<'a> = { left: Option<TreeNode<'a>>; right: Option<TreeNode<'a>>; data:'a }

let left node = node.left
    //node |> Option.bind (fun n -> n.left)

let right node = node.right
    //node |> Option.bind (fun n -> n.right)

let data node = node.data

let rec create' (items:list<int>): TreeNode<int> option = 
    match items with
    | [] -> None
    | item :: items' ->
        let left = create' ( List.filter (fun x -> x <= item) items' )
        let right = create' ( List.filter (fun x -> x > item) items' )
        Some { left = left; right = right; data = item }

let create (items:list<int>): TreeNode<int> = 
    create' items |> Option.defaultValue { left = None; right = None; data = -1}
        
let rec sortedData' node = 
    match node with
    | None -> []
    | Some { left=l; right=r; data=d } -> List.append (sortedData' l) (d :: sortedData' r)

let sortedData node =
    node |> Option.Some |> sortedData'