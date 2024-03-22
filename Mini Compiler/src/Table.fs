(*
    This file contains the lookup table structure

    We choose to use a single linked list, because it is easy
    and adhire to many properties that simplifies the code.
    Also behavioral patterns of a compiler makes a list a good choice.

    the table type are merely a list with pairs of key and values
    the 'key must admit equality hence, we need to be able to decide
    wether to keys are the same.

    the 'item can be any type.

    we use the ref type to wrap the items because it makes it possible to do 
    update extending outside scopes i.e. if we update a value

    there is a lot of higher order functions and functions in the List lib.
    but we choose to implement all by hand to extend understanding for people
    looking at the code
*)
module Table


type Table<'key,'item when 'key:equality> =
    {
        // struct makes the tuple a value type rather than a reference type
        // this is an optimisational choice i.e. eliminating double reference
        // on lookup and double alloc calls to the heap
        Pairs: struct('key * 'item ref) list
    }
with
    static member empty = { Pairs = [] } : Table<'key,'item> // a generic empty table

// just to ease coding
let inline KeyOf struct(key, _) = key
let inline ItemOf struct(_, item: _ ref) = item.Value 


// look after a specific key in the table
let Lookup key tab =
    let rec loop pairs =
        match pairs with
        | [] -> ValueNone
        | pair :: _ when KeyOf pair = key -> ValueSome (ItemOf pair)
        | _ :: pairs -> loop pairs

    loop tab.Pairs


// add an item to the table with reference point key
let Bind key item tab =
    {
        Pairs = (key, ref item) :: tab.Pairs
    }

// add an item to the table checking no key in the table are
// equal to key i.e. not overshadowing a former key
let BindUnique key item tab =
    if List.exists ((=) key << KeyOf) tab.Pairs then
        ValueNone
    else
        Bind key item tab
        |> ValueSome

// try updating a key value pair
let Update key item tab = 
    let rec loop (pairs: struct('key * 'item ref) list) =
        match pairs with
        | [] -> ValueNone
        | (key', item') :: _ when key' = key ->
            item' := item
            ValueSome tab
        | _ :: pairs -> loop pairs
    loop tab.Pairs

// update an item referet to by the key
// if the key does not exists bind the pair to the table
let UpdateOrBind key item tab =
    Update key item tab
    |> ValueOption.defaultWith (fun _ -> Bind key item tab)

// remove latest binding of key
let Remove key tab =
    let rec loop head pairs =
        match pairs with
        | [] -> tab
        | pair :: pairs when KeyOf pair = key ->
            {
                // List.fold is a higher order function
                // taking an accumulator function, an accumulator value and
                // a list to be accumaleted over
                // here we simply add the pairs looked at back
                // to the list.
                //
                // OBS: the head list are in reverse this is why we can't just append it.
                Pairs = List.fold (fun pairs pair -> pair :: pairs) pairs head
            }
        | pair :: pairs -> loop (pair :: head) pairs

    loop [] tab.Pairs


let Union tab1 tab2 = { Pairs = tab1.Pairs @ tab2.Pairs }