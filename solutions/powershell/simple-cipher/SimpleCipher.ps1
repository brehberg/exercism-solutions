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
    [int]hidden $min = [System.Text.Encoding]::UTF8.GetBytes("a")[0]
    [int]hidden $max = [System.Text.Encoding]::UTF8.GetBytes("z")[0]

    SimpleCipher() {
        $random = (Get-Random -Count 105 -Minimum $this.min -Maximum $this.max)
        $this._key = -join ($random.forEach({ [char]$_ }))
        $this.offsets = [System.Text.Encoding]::UTF8.GetBytes($this._key).forEach({ $_ - $this.min })
    }

    SimpleCipher([string]$key) {
        $this._key = $key.ToLower()
        $this.offsets = [System.Text.Encoding]::UTF8.GetBytes($this._key).forEach({ $_ - $this.min })
    }

    [string] Encode([string]$text) {
        $values = [System.Text.Encoding]::UTF8.GetBytes($text.ToLower())

        # shift the byte value for each letter to the right based on offsets from key
        for ($i = 0; $i -lt $values.Count; $i++) {
            $values[$i] += $this.offsets[$i % $this.offsets.Count]
            if ($values[$i] -gt $this.max ) {
                $values[$i] -= ($this.max - $this.min + 1)
            }
        }        
        return [System.Text.Encoding]::UTF8.GetString($values)
    }

    [string] Decode([string]$text) {
        $values = [System.Text.Encoding]::UTF8.GetBytes($text.ToLower())

        # shift the byte value for each letter to the left based on offsets from key
        for ($i = 0; $i -lt $values.Count; $i++) {     
            $values[$i] -= $this.offsets[$i % $this.offsets.Count]
            if ($values[$i] -lt $this.min ) {
                $values[$i] += ($this.max - $this.min + 1)
            }
        }
        return [System.Text.Encoding]::UTF8.GetString($values)
    }
}