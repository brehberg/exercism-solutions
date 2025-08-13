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
    [int]$data
    [TreeNode]$left 
    [TreeNode]$right

    TreeNode([int]$value) {
        $this.data = $value
        $this.left = $null
        $this.right = $null
    }

    TreeNode([int]$value, [TreeNode]$leftchild, [TreeNode]$rightchild) {
        $this.data = $value
        $this.left = $leftchild
        $this.right = $rightchild
    }

    [string] ToString() {
        return "Node{data=$($this.data),$($this.left),$($this.right)}"
    }
}


Class BinarySearchTree {
    [TreeNode]$root

    BinarySearchTree([int[]]$values) {
        foreach ($value in $values) {
            $this.Insert($value)
        }
    }

    Insert([int]$value) {
        if ($null -eq $this.root) {
            $this.root = [TreeNode]::new($value)
        }
        else {
            $this.Insert($value, $this.root)
        }

    }
    
    Insert([int]$value, [TreeNode]$base) {
        if ($value -le $base.data) {
            if ($null -eq $base.left) {
                $base.left = [TreeNode]::new($value)
                return
            }
            $this.Insert($value, $base.left)
        }
        else {
            if ($null -eq $base.right) {
                $base.right = [TreeNode]::new($value)
                return
            }       
            $this.Insert($value, $base.right)     
        }
    }

    [TreeNode] GetData() {
        return $this.root
    }

    [boolean] Search([int]$value) {
        return $this.Inorder() -Contains $value
    }

    [int[]] Inorder() {
        return $this.Traverse($this.root, "in", [System.Collections.ArrayList]@())
    }

    [int[]] PreOrder() {
        return $this.Traverse($this.root, "pre", [System.Collections.ArrayList]@())
    }

    [int[]] PostOrder() {
        return $this.Traverse($this.root, "post", [System.Collections.ArrayList]@())
    }

    [System.Collections.ArrayList] Traverse(
        [TreeNode]$base, [string]$order,
        [System.Collections.ArrayList]$values) {
        
        if ($order -eq "pre") {
            $values.Add($base.data)
        }
        if ($null -ne $base.left) {
            $values = $this.Traverse($base.left, $order, $values)
        }
        if ($order -eq "in") {
            $values.Add($base.data)
        }
        if ($null -ne $base.right) {
            $values = $this.Traverse($base.right, $order, $values)
        }
        if ($order -eq "post") {
            $values.Add($base.data)
        }
        return $values
    }

    [string] ToString() {
        <#
        .DESCRIPTION
        Tostring method to help print out the tree in a nice format for viewing
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