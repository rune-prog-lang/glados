somewhere
{
    def allocate(size: u64) ~> *any;
    def liberate(ptr: *any) -> bool;
    def reallocate(ptr: *any, size: u64) ~> *any;
    def initialize(ptr: *any, size: u64, value: u8) ~> *any;
    
    def assert(condition: bool, message: string) -> bool;
}

struct Vec
{
    data:     *any;
    size:     u64;
    capacity: u64;

    def new() -> Vec
    {
        Vec {
            data:     null,
            size:     0,
            capacity: 0
        }
    }

    override def new(initial_capacity: u64) ~> Vec
    {
        if initial_capacity == 0 {
            new()
        }

        ptr = allocate(initial_capacity * 8)?; 

        Vec {
            data:     ptr,
            size:     0,
            capacity: initial_capacity
        }
    }

    def get(self, index: u64) ~> any
    {
        if index >= self.size {
            error("Index out of bounds")
        }
        self.data[index]
    }

    def push(self, value: any) ~> bool
    {
        if self.size >= self.capacity {
            new_capacity = self.capacity * 2 + 1;
            self.data = reallocate(self.data, new_capacity * 8)?;
            self.capacity = new_capacity;
        }

        self.data[self.size] = value;
        self.size = self.size + 1;
        true
    }

    def pop(self) ~> any
    {
        if self.size == 0 {
            error("Cannot pop from an empty vector")
        }
        self.size = self.size - 1;
        self.data[self.size]
    }

    def delete(self) -> bool
    {
        if self.data != null {
            liberate(self.data);
        }
        self.data = null;
        self.size = 0;
        self.capacity = 0;
        true
    }

}

def main() ~> null
{
    v = Vec.new(4)?;

    v.push(10)?;
    v.push(20)?;
    v.push(30)?;

    val = v.get(1)?;
    assert(val == 20, "Le deuxième élément doit être 20");

    v.push(40)?;
    v.push(50)?;

    dernier = v.pop()?;

    v.delete(); // normalement, delete() doit être appelé à la fin du scope de v. comme le C++.
}
