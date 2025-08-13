package linkedlist

import "errors"

type List struct {
	head *Element
	size int
}

type Element struct {
	next *Element
	data int
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
	l.head = &Element{data: element, next: l.head}
	l.size += 1
}

func (l *List) Pop() (int, error) {
	if l.head == nil {
		return 0, errors.New("empty list")
	}

	oldHead := l.head
	l.head = oldHead.next
	l.size -= 1

	oldHead.next = nil
	return oldHead.data, nil
}

func (l *List) Array() []int {
	values := make([]int, l.size)
	for i, element := l.size-1, l.head; i >= 0; i, element = i-1, element.next {
		values[i] = element.data
	}
	return values
}

func (l *List) Reverse() *List {
	newList := &List{}
	for element := l.head; element != nil; element = element.next {
		newList.Push(element.data)
	}
	return newList
}
