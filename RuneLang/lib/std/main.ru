somewhere
{
    def compare(a: string, b: string) -> i32;
}

def main() -> i32
{
    result1: i32 = compare("apple", "banana");
    result2: i32 = compare("grape", "grape");
    result3: i32 = compare("orange", "apple");
    final_result: i32 = result1 + result2 + result3;
    show(final_result);
    0
}
