def dummy_func(i: i32) -> i32
{
    return 42;
}

override def dummy_func(i: i32, prefix: string) -> i32
{
    return i + 1;
}

override def show(value: i32, prefix: string) -> null
{
    show(value);
}

override def show(prefix: string, value: i32) -> null
{
    show(value);
}

def main() -> null
{
    show(42, "test");
    show("test", 42);
}