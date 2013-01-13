open System

let choose n k = List.fold (fun s i -> s * (n-i+1)/i ) 1 [1..k]

let lFS str = List.ofArray(str.ToString().ToCharArray())

let for_all p = List.fold (fun a x -> a && (p x)) true;;

let rec genPrNDim pos len = if len = 0 then [[]] else let smaller = (genPrNDim pos (len - 1)) in [for c in pos do for sm in smaller do yield c :: sm ]

let isGood = List.exists 

let digitsSum x = x.ToString().ToCharArray() |> Array.map(fun b-> Int32.Parse(b.ToString())) |> Array.sum
let isMyGood x = digitsSum x <= 51

let rec isTransposition = function 
    | [] -> true 
    | xs -> let x = List.tryFind ( (=) (List.head xs)) (List.tail xs) in
                match x with 
                None -> List.tail xs |> isTransposition 
                | _ -> false
        
let allTransOn pos = 
    let xs = lFS pos in
    genPrNDim (xs) (List.length xs) |> List.filter isTransposition 

let hasSameDiffWithAt xs ys n =
    let a = List.nth xs n in
    let len = List.length xs in
    let stat = [for c in xs do yield ((List.findIndex ((=)c) ys), c) ] 
                |> List.sortBy (fun (i,b) -> b) in
    let withIndex = List.mapi( fun i x -> (i, x) ) xs in
    let m = List.findIndex ((=)a) ys in
    let pred (i,x) (j,_) = if x = a then false 
                                    else 
                                        (n - i + len) % len = (m - j + len) % len || 
                                        (i - n + len) % len = (m - j + len) % len 
                                        
                                    in
     let res = List.map2 pred withIndex stat |> List.exists((=) true) in
     res


let rec hasSameDiffI xs ys i = 
    if i >= List.length xs then false 
    else if hasSameDiffWithAt xs ys i then true else hasSameDiffI xs ys (i+1)

let hasSameDiff xs ys =  hasSameDiffI xs ys 0

let allTranspTest pos = 
    let basic = lFS pos in
    let transps = allTransOn pos in
    let stat =  List.map (fun t -> (hasSameDiff basic t, t) ) transps in
    let res = transps.Length = List.length(List.filter (hasSameDiff basic) transps) in
    res


let get72 k = 
    let isMyGood xs = 
        let str = new String(List.toArray xs) in
            false = (str.Contains("11") || str.Contains("101") || 
            str.Contains("1001"))
    let subsets = genPrNDim ['0';'1'] k in
        List.filter isMyGood subsets

let rec sum down up f = 
    if (down > up) then 0
    else f(down) + (sum (down+1) up f)

let rec a k acc = 
    if k = 0 then Array.set acc 0 1 
    else 
        if k % 2 = 0 then
            let n = (k/2) in
            let v = sum 0 (n-1) (fun i-> acc.[i] * acc.[2*n-i-1])
            Array.set acc k v
        else
            let n = (k-1)/2 in
            let s = sum 0 (n-1) (fun i->acc.[i] * acc.[2*n-i]) in
            let v = s + (choose acc.[n] 2) + acc.[n]
            Array.set acc k v

let an n = 
    let arr = Array.map(fun _-> -1) [|0..n|] in 
    let len = List.map (fun i -> a i arr) [0..n] |> List.length in
    arr
    
    



let main =
    an 5
    
