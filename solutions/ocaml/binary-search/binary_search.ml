let find haystack needle =
    let rec search low high =
        let mid = low + (high - low) / 2 in
        if low > high then Error "value not in array"
        else if haystack.(mid) < needle then
            search (mid + 1) high
        else if haystack.(mid) > needle then
            search low (mid - 1)
        else Ok mid in
    search 0 (Array.length haystack - 1) ;;
