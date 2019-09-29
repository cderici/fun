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


class Stack(object):
    def __init__(self):
        self.container = []

    def push(self, data):
        self.container.append(data)

    def pop(self):
        return self.container.pop()

    def is_empty(self):
        return self.container == []

class Queue(object):
    def __init__(self):
        self.stack1 = Stack()
        self.stack2 = Stack()

    def enqueue(self, data):
        # enqueue into stack1
        # check stack full if there's a stack size limit
        self.stack1.push(data)

    def is_empty(self):
        return self.stack1.is_empty() and self.stack2.is_empty()

    def dequeue(self):
        # dequeue from stack2
        if self.is_empty():
            raise Exception("nothing to dequeue")

        if self.stack2.is_empty():
            self.transfer()
        return self.stack2.pop()

    def transfer(self):
        # transfer everything from stack1 to stack2
        while not self.stack1.is_empty():
            self.stack2.push(self.stack1.pop())

""" assumes the stacks don't have limits
  s1
|    |
|    |
|    |
|____|

  s2
|    |
|    |
|    |
|_2__|


"""

q = Queue()
q.enqueue(3)
q.enqueue(5)
assert q.dequeue() == 3
assert q.dequeue() == 5

q.enqueue(7)
q.enqueue(8)

assert q.dequeue() == 7
assert q.dequeue() == 8
