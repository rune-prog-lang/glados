def showln(value: any) -> null
{
    show(value);
    show('\n');
}

def many_args(args: string...) -> null
{
    for arg in args {
        showln(arg);
    }
}

def main() -> null
{
    many_args("hello", "world", "from", "many_strings");
}
