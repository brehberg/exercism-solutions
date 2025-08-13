let anagrams = (target, candidates) => {
    let sorted = input =>
        input
        |> Js.String.split("")
        |> Js.Array.sortInPlace
        |> Js.Array.joinWith("");

    let base = String.lowercase_ascii(target);
    let sortedBase = sorted(base);

    let isAnagram = candidate => {
        let word = String.lowercase_ascii(candidate);
        base != word && sortedBase == sorted(word);
    }
    
    List.filter(isAnagram, candidates);
}