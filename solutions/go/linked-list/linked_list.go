package linkedlist

// Node type holds a value and pointers to the next and previous nodes.
// The tests expect Node to include an exported field with name Value.
type Node struct {
	next  *Node
	prev  *Node
	Value interface{}
}

// List type holds references to the first and last node and offers
// an array-like interface for adding and removing items.
type List struct {
	head *Node
	tail *Node
	size int
}

// NewList creates a new linked list preserving the order of the values.
func NewList(args ...interface{}) *List {
	newList := &List{}
	for _, value := range args {
		newList.Push(value)
	}
	return newList
}

// Next returns a pointer to the next node.
func (n *Node) Next() *Node {
	return n.next
}

// Prev returns a pointer to the previous node.
func (n *Node) Prev() *Node {
	return n.prev
}

// Unshift inserts value at the front of the list.
func (l *List) Unshift(v interface{}) {
	newNode := &Node{Value: v, next: l.head}

	if l.tail == nil {
		// first node added, also set tail to new
		l.tail = newNode
	} else {
		// otherwise, set old head previous to new
		l.head.prev = newNode
	}

	l.head = newNode
	l.size += 1
}

// Push inserts value at the back of the list.
func (l *List) Push(v interface{}) {
	newNode := &Node{Value: v, prev: l.tail}

	if l.head == nil {
		// first node added, also set head to new
		l.head = newNode
	} else {
		// otherwise, set old tail next to new
		l.tail.next = newNode
	}

	l.tail = newNode
	l.size += 1
}

// Shift removes value from the front of the list.
func (l *List) Shift() (interface{}, error) {
	oldNode := l.head
	l.head = oldNode.next
	l.size -= 1

	if l.head == nil {
		// last node removed, also clear tail
		l.tail = nil
	} else {
		// otherwise, clear new head previous
		l.head.prev = nil
	}

	oldNode.next, oldNode.prev = nil, nil
	return oldNode.Value, nil
}

// Pop removes value from the back of the list.
func (l *List) Pop() (interface{}, error) {
	oldNode := l.tail
	l.tail = oldNode.prev
	l.size -= 1

	if l.tail == nil {
		// last node removed, also clear head
		l.head = nil
	} else {
		// otherwise, clear new tail next
		l.tail.next = nil
	}

	oldNode.next, oldNode.prev = nil, nil
	return oldNode.Value, nil
}

// Reverse reverses the order of the linked list.
func (l *List) Reverse() {
	values := make([]interface{}, l.size)

	// remove values from the front of the list
	for i := l.size - 1; i >= 0; i-- {
		value, _ := l.Shift()
		values[i] = value
	}

	// insert same values at the back of the list
	for _, value := range values {
		l.Push(value)
	}
}

// First returns a pointer to the first node (head).
func (l *List) First() *Node {
	return l.head
}

// Last returns a pointer to the last node (tail).
func (l *List) Last() *Node {
	return l.tail
}
