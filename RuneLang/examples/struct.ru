struct Vec2f
{
    x: f32;
    y: f32;

    def add(self, other: Vec2f) -> Vec2f
    {
        show("Adding two Vec2f\n");
        Vec2f {
            x: self.x + other.x,
            y: self.y + other.y
        }
    }

    override def add(self, f: f32) -> Vec2f
    {
        show("Adding f32 to Vec2f\n");
        Vec2f {
            x: self.x + f,
            y: self.y + f
        }
    }
}

override def show(v: Vec2f) -> null
{
    show("Vec2f(x: ");
    show(v.x);
    show(", y: ");
    show(v.y);
    show(")\n");
}

def main() -> null
{
    a = Vec2f { x: 1.0, y: 2.0 };
    b = Vec2f { x: 3.0, y: 4.0 };
    c = a.add(b);
    d = a.add(5.0);

    show(c);
}
