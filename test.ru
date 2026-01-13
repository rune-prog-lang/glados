struct Vec2f
{
    private x: f32;
    private y: f32;

    public def new() -> Vec2f
    {
        Vec2f { x: 0.0, y: 0.0 }
    }

    public def add(self, other: Vec2f) -> Vec2f
    {
        Vec2f {
            x: self.x + other.x,
            y: self.y + other.y
        }
    }
}

def dummy_func(a: i32, a: i32) -> i32
{
    return a + a;
}

def main() -> null
{
    show(dummy_func(1, 2));
}