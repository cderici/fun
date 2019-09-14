# simple ht using a fixed size array
# holding dictionaries (little bit of cheating:)

CONTAINER_SIZE = 64

class HashTable(object):
    def __init__(self):
        self.container = [None]*CONTAINER_SIZE

    def hash_set(self, key, val):
        data_idx = hash(key) % CONTAINER_SIZE
        if not self.container[data_idx]:
            self.container[data_idx] = {key:val}
            return
        self.container[data_idx][key] = val
        return

    def hash_ref(self, key):
        data_idx = hash(key) % CONTAINER_SIZE
        if not self.container[data_idx]:
            return False
        if key not in self.container[data_idx]:
            return False
        return self.container[data_idx][key]

h = HashTable()

h.hash_set("a", 1)
h.hash_set("b", 2)
h.hash_set("c", 3)
h.hash_set("c", 4)
assert h.hash_ref("c") == 4
assert h.hash_ref("a") == 1
