<#
.SYNOPSIS
Implement a simple shift cipher like Caesar and a more secure substitution cipher.

.DESCRIPTION
Implement a simple cipher class to encode or decode a message with a key.
If there was no key provided, generate one minumum 100 characters long contains only lower case letter (a-z).

.EXAMPLE
$cipher = [SimpleCipher]::new("mykey")

$cipher.Encode("aaaaa")
Return: "mykey"

$cipher.Decode("ecmvcf")
Return: "secret"
#>

Class SimpleCipher {
    [ValidateNotNullOrEmpty()] [string]$_key
    [ValidateNotNullOrEmpty()] [int[]]hidden $offsets
    [int]static $min = [System.Text.Encoding]::UTF8.GetBytes("a")[0]
    [int]static $max = [System.Text.Encoding]::UTF8.GetBytes("z")[0]

    SimpleCipher() {
        # generate a truly random key of at least 100 lowercase characters in length
        $length = Get-Random -Minimum 100 -Maximum 151
        $random = (Get-Random -Count $length -Minimum ([SimpleCipher]::min) -Maximum ([SimpleCipher]::max + 1))
        $this._key = [System.Text.Encoding]::UTF8.GetString($random)
        $this.offsets = $random.forEach({ $_ - [SimpleCipher]::min })
    }

    SimpleCipher([string]$key) {
        $this._key = $key.ToLower()
        $this.offsets = [System.Text.Encoding]::UTF8.GetBytes($this._key).forEach({ $_ - [SimpleCipher]::min })
    }

    [string] Encode([string]$text) {
        $values = [System.Text.Encoding]::UTF8.GetBytes($text.ToLower())

        # shift the byte value for each letter to the right based on offsets from key
        for ($i = 0; $i -lt $values.Count; $i++) {
            $values[$i] += $this.offsets[$i % $this.offsets.Count]
            if ($values[$i] -gt [SimpleCipher]::max ) {
                $values[$i] -= ([SimpleCipher]::max - [SimpleCipher]::min + 1)
            }
        }        
        return [System.Text.Encoding]::UTF8.GetString($values)
    }

    [string] Decode([string]$text) {
        $values = [System.Text.Encoding]::UTF8.GetBytes($text.ToLower())

        # shift the byte value for each letter to the left based on offsets from key
        for ($i = 0; $i -lt $values.Count; $i++) {     
            $values[$i] -= $this.offsets[$i % $this.offsets.Count]
            if ($values[$i] -lt [SimpleCipher]::min ) {
                $values[$i] += ([SimpleCipher]::max - [SimpleCipher]::min + 1)
            }
        }
        return [System.Text.Encoding]::UTF8.GetString($values)
    }
}