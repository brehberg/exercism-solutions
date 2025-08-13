<#
.SYNOPSIS
Implement two classes, one for tree node and one for binary search tree.

.DESCRIPTION
Create a binary search tree made by many different tree node.
Each tree node instance contain the value for the node and its children if exist.
The binary search tree should have these methods:
- Insert    : take in an array of number, create node and insert them follow the property of the binary search tree.
- GetData   : return the root node that contain the entire tree's data
- Search    : take in a number to find in the binary tree, return a boolean value
- Inorder   : return an array of values in order of inorder travel
- Postorder : return an array of values in order of postorder travel
- Preorder  : return an array of values in order of preorder travel

.EXAMPLE
$tree = [BinarySearchTree]::new(@(3,4,2))

$tree.Search(3)
Return: true

$tree.Inorder()
Return: @(2, 3, 4)

$tree.PreOrder()
Return: @(3, 2, 4)

$tree.Postorder()
Return: @(2, 4, 3)
#>
Class TreeNode {
    [int]hidden $data
    [TreeNode]hidden $left = $null
    [TreeNode]hidden $right = $null

    TreeNode([int]$value) {
        $this.data = $value
    }

    TreeNode([int]$value, [TreeNode]$leftchild, [TreeNode]$rightchild) {
        $this.data = $value
        $this.left = $leftchild
        $this.right = $rightchild
    }

    hidden [void] Insert([int]$value) {
        if ($value -le $this.data) {
            if (-not $this.left) {
                $this.left = [TreeNode]::new($value)
                return
            }
            $this.left.Insert($value)
        }
        else {
            if (-not $this.right) {
                $this.right = [TreeNode]::new($value)
                return
            }
            $this.right.Insert($value)
        }
    }
        
    hidden [boolean] Search([int]$value) {
        if ($value -eq $this.data) {
            return $true
        }
        if ($value -le $this.data) {
            return ($this.left) ? $this.left.Search($value) : $false
        }
        else {
            return ($this.right) ? $this.right.Search($value) : $false
        }
    }

    hidden [void] Traverse([string]$order, [ref]$values) {        
        if ($order -eq "pre") {
            $values.Value += $this.data
        }
        if ($this.left) {
            $this.left.Traverse($order, $values)
        }
        if ($order -eq "in") {
            $values.Value += $this.data
        }
        if ($this.right) {
            $this.right.Traverse($order, $values)
        }
        if ($order -eq "post") {
            $values.Value += $this.data
        }
    }

    [string] ToString() {
        return "Node{data=$($this.data),$($this.left),$($this.right)}"
    }
}


Class BinarySearchTree {
    [TreeNode]hidden $root = $null

    BinarySearchTree([int[]]$values) {
        foreach ($value in $values) {
            $this.Insert($value)
        }
    }

    [void] Insert([int]$value) {
        if (-not $this.root) {
            $this.root = [TreeNode]::new($value)
            return
        }
        $this.root.Insert($value)
    }

    [TreeNode] GetData() { return $this.root }

    [boolean] Search([int]$value) {
        return ($this.root) ? $this.root.Search($value) : $false
    }

    [int[]] Inorder() { return $this.Traverse("in") }
    [int[]] PreOrder() { return $this.Traverse("pre") }
    [int[]] PostOrder() { return $this.Traverse("post") }

    hidden [int[]] Traverse([string]$order) {
        $result = @()
        if ($this.root) {        
            $this.root.Traverse($order, [ref]$result)
        }
        return $result
    }

    [string] ToString() {
        <#
        .DESCRIPTION
        ToString method to help print out the tree in a nice format for viewing
        Adapt from : https://stackoverflow.com/questions/4965335/how-to-print-binary-tree-diagram-in-java
        #>
        $textBuilder = [System.Text.StringBuilder]::new()
        $this.PrintBinaryTree($textBuilder, "", "", $this.root)
        return $textBuilder.ToString()
    }

    hidden [void] PrintBinaryTree ([System.Text.StringBuilder]$string, [string]$prefix, [string]$childrenPrefix, [TreeNode]$root) {
        $string.Append("$prefix$($root.data)`n")

        if ($root.left -and $root.right) {
            #draw left child
            $this.PrintBinaryTree($string, ($childrenPrefix + "├──L:"), ($childrenPrefix + "│   "), $root.left)
        }
        elseif ($root.left) {
            $this.PrintBinaryTree($string, ($childrenPrefix + "└──L:"), ($childrenPrefix + "    "), $root.left)
        }
        
        if ($root.right) {
            #draw right child
            $this.PrintBinaryTree($string, ($childrenPrefix + "└──R:"), ($childrenPrefix + "    "), $root.right)
        }
    }

}