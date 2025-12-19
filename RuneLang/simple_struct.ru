struct Vec2f
{
    x: i32;
    y: i32;

    def add(self, other: Vec2f) -> Vec2f
    {
        Vec2f {
            x: self.x + other.x,
            y: self.y + other.y
        }
    }
    def add(self, scale: i32) -> Vec2f
    {
        Vec2f {
            x: self.x * scale,
            y: self.y * scale
        }
    }

    def del(self) -> null
    {
        show("Vec2f deleted");
    }
}

override def show(vec: Vec2f, value: i32) -> null
{
    show(value);
    show('\n');
}

override def show(vec: Vec2f, msg: string) -> null
{
    show(msg);
    show("Vec2f(");
    show(vec.x);
    show(", ");
    show(vec.y);
    show(")\n");
}

def main() -> null
{
    v: Vec2f = Vec2f { x: 10, y: 20 };
    show(v, 42);
}
