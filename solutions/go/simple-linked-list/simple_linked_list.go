package linkedlist

import "errors"

type List struct {
	head *Element
	size int
}

type Element struct {
	value int
	next  *Element
}

func New(values []int) *List {
	newList := &List{}
	for _, value := range values {
		newList.Push(value)
	}
	return newList
}

func (l *List) Size() int {
	return l.size
}

func (l *List) Push(element int) {
	l.head = &Element{value: element, next: l.head}
	l.size += 1
}

func (l *List) Pop() (int, error) {
	oldHead := l.head
	if oldHead == nil {
		return 0, errors.New("empty list")
	}
	l.head = oldHead.next
	l.size -= 1
	return oldHead.value, nil
}

func (l *List) Array() []int {
	values := l.backwards()
	for left, right := 0, len(values)-1; left < right; left, right = left+1, right-1 {
		values[left], values[right] = values[right], values[left]
	}
	return values
}

func (l *List) Reverse() *List {
	values := l.backwards()
	return New(values)
}

func (l *List) backwards() (values []int) {
	for element := l.head; element != nil; element = element.next {
		values = append(values, element.value)
	}
	return
}
