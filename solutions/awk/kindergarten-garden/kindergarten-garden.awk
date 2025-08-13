# These variables are initialized on the command line (using '-v'):
# - name

BEGIN {
    FS = ""
    plants["C"] = "clover"
    plants["G"] = "grass"
    plants["R"] = "radishes"
    plants["V"] = "violets"

    n = split("Alice,Bob,Charlie,David,Eve,Fred,Ginny,Harriet,Ileana,Joseph,Kincaid,Larry", students, ",")
    for (i = 1; i <= n; i++)
        if (students[i] == name)
            offset = 2 * i - 1
}

{
    result = result " " plants[$(offset)] " " plants[$(offset+1)]
}

END {
    print(substr(result,2))
}
