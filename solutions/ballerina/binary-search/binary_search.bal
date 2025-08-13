# Search an array for a value and return the index.
#
# + array - a sorted array of integers
# + value - the integer item to find
# + return - the index of the value, or nil if the value is not found
public function find(int[] array, int value) returns int? {
    return search(array, value, 0, array.length() - 1);
}

var search = function(int[] haystack, int needle, int low, int high) returns int? {
    if low > high {
        return ();
    }

    int mid = low + (high - low) / 2;
    if haystack[mid] < needle {
        return search(haystack, needle, mid + 1, high);
    } else if haystack[mid] > needle {
        return search(haystack, needle, low, mid - 1);
    } else {
        return mid;
    }
};
