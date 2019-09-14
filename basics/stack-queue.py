# stack with lists
class StackL(object):
    def __init__(self):
        self.data = []

    def push(self, data):
        self.data.append(data)

    def pop(self):
        return self.data.pop()

s = StackL()
s.push(3)
s.push(2)
s.push(1)

assert s.pop() == 1
assert s.pop() == 2
assert s.pop() == 3

# stack with objects
class SNode(object):
    def __init__(self, data, next=None):
        self.data = data
        self.next = next

class StackO(object):
    def __init__(self):
        self.head = None

    def push(self, data):
        if not self.head:
            self.head = SNode(data)
            return True
        self.head = SNode(data, self.head)
        return True

    def pop(self):
        if not self.head:
            return False # could raise an error
        d = self.head.data
        self.head = self.head.next
        return d

s = StackO()
s.push(3)
s.push(2)
s.push(1)

assert s.pop() == 1
assert s.pop() == 2
assert s.pop() == 3

# queue with lists
class QueueL(object):
    def __init__(self):
        self.data = []

    def enqueue(self, data):
        self.data.append(data)

    def dequeue(self):
        return self.data.pop(0)

q = QueueL()
q.enqueue(3)
q.enqueue(2)
q.enqueue(1)

assert q.dequeue() == 3
assert q.dequeue() == 2
assert q.dequeue() == 1

# queue with objects
class QNode(object): # same with SNode
    def __init__(self, data, next=None):
        self.data = data
        self.next = next

class QueueO(object):
    def __init__(self):
        self.head = None
        self.tail = None

    def enqueue(self, data):
        if not self.tail: # nothing yet
            self.head = self.tail = QNode(data)
            return True
        newNode = QNode(data)
        self.tail.next = newNode
        self.tail = newNode
        return True

    def dequeue(self):
        if not self.head: # nothing yet
            return False # could raise an error
        d = self.head.data
        self.head = self.head.next
        return d

q = QueueO()
q.enqueue(3)
q.enqueue(2)
q.enqueue(1)

assert q.dequeue() == 3
assert q.dequeue() == 2
assert q.dequeue() == 1
