# max heap
"""
using arrays

            25
          /    \
        16     24
      /   \   /   \
     5    11 19    1
   /  \   /
  2    3 5

[1, 2, 3, 4,5, 6, 7,8,9,10]
[25,16,24,5,11,19,1,2,3,5]

parent(i) = i/2
left-child(i) = i*2
right-child(i) = i*2+1


push(insert)
peek(get max)
pop(remove max)
"""

class MaxHeap(object):
    def __init__(self):
        self.data = [0]
        self.current_pos = 1 # just to avoid len()ing all the time

    def heapify(self, ls):
        for a in ls:
            self.add(a)

    def add(self, data):
        # put it at the very end
        self.data.append(data)
        # bubble it up
        self._bubble_up(self.current_pos)
        self.current_pos += 1

    def _bubble_up(self, index):
        if index <= 1:
            return
        # max heap
        # swap while the current is bigger than its parent
        while self.data[index] > self.data[index/2]:
            self._swap(index, index/2)
            index = index/2

    def peek(self):
        return self.data[1]

    def _swap(self, i, j):
        self.data[i], self.data[j] = self.data[j], self.data[i]

    def _float_down(self, index):
        # this is a max heap
        # look at the left and right child (there might not be one)
        # swap with the biggest one
        largest = index
        left = index * 2
        right = (index * 2) + 1
        if left < self.current_pos and self.data[left] > self.data[largest]:
            largest = left
        if right < self.current_pos and self.data[right] > self.data[largest]:
            largest = right

        if largest != index:
            self._swap(largest, index)
            self._float_down(largest)

    def pop_max(self):
        # swap the last and first
        self._swap(1,-1)
        # float down the root
        self._float_down(1)
        # pop the last
        self.current_pos -= 1
        return self.data.pop()


h = MaxHeap()
h.heapify([25,16,24,5,11,19,1,2,3,5])
h.add(12)
assert h.data == [0,25,16,24,5,12,19,1,2,3,5,11]
assert h.pop_max() == 25
assert h.data == [0,24,16,19,5,12,11,1,2,3,5]

"""
       24
    16    19
   5  12 11 1
 2 3 5
"""

# we can modify max heap to hold something like a
# DataNode having fields for data and priority
# and make this return the first-in one for a two having the same priority
# for now this is gonna be simply a wrapper
class PriorityQueue(object):
    def __init__(self):
        self.heap = MaxHeap()

    def enqueue(self, data):
        self.heap.add(data)

    def dequeue(self):
        return self.heap.pop_max()
