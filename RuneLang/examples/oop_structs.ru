somewhere
{
    def showln(message: string) -> null;
    def assert(condition: bool, message: string) -> bool;
}

abstract struct Shape
{
    private x: f32;
    private y: f32;

    private   id: i32;
    private static count: i32 = 0;

    public static def get_total_shapes() -> i32
    {
        Shape.count
    }

    public abstract def get_area(self) -> f32 {}

    public def move(self, dx: f32, dy: f32) -> null
    {
        self.x += dx;
        self.y += dy;
        showln("Shape moved.");
    }

    protected def add(self, other: Shape) -> Shape
    {
        Shape.new(self.x + other.x, self.y + other.y)
    }

    protected def new(x: f32, y: f32) -> Shape
    {
        Shape.count += 1;
        Shape {
            x: x,
            y: y,
            id: Shape.count
        }
    }
}

struct Circle extends Shape
{
    private radius: f32;

    public static def new(x: f32, y: f32, r: f32) -> Circle
    {
        Circle {
            __base: Shape.new(x, y),
            radius: r
        }
    }

    public def get_area(self) -> f32
    {
        3.14159 * self.radius * self.radius
    }

    public def move(self, dx: f32, dy: f32) -> null
    {
        showln("Circle is rolling...");
        __base.move(dx, dy);
    }
}

def main() -> null
{
    c = Circle.new(0.0, 0.0, 5.0);
    area = c.get_area();
    showln("Circle area: " + area);
    c.move(2.0, 3.0);
    total = Shape.get_total_shapes();
    showln("Total shapes created: " + total);
}
