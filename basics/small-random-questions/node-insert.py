class Node(object):
    def __init__(self, data):
        self.data = data
        self.next = None

    def _prnt(self):
        current = self
        s = ""
        while current:
            s += " %s " % current.data
            current = current.next
        print s

class Lst(object):
    def __init__(self):
        self.head = None

    def prnt(self):
        self.head._prnt()

    def cons(self, data):
        newNode = Node(data)
        newNode.next = self.head
        self.head = newNode

    def _to_list(self, arr):
        for a in arr:
            self.cons(a)
        return self

    def first(self):
        return self.head.data

    def rest(self):
        return self.head.next

a = Lst()._to_list([4,3,2,1])
a.prnt()

def insertNodeAtPosition(head, n, pos):
    if pos == 0:
        newNode = Node(n)
        newNode.next = head
        head = newNode
    else:
        head.next = insertNodeAtPosition(head.next, n, pos-1)

    return head

insertNodeAtPosition(a.head, 9, 2)
a.prnt()
a.head = insertNodeAtPosition(a.head, 6, 0)
a.prnt()
