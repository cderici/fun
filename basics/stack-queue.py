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
