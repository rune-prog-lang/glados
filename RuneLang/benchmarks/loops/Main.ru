def main() -> null
{
    result: i64 = 0;

    for i: i64 = 0 to 1_000_000_000 {
        result += i;
        ++i;
    }
    show(result)
}
