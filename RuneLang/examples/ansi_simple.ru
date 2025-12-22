def showln(value: string) -> null
{
    show(value);
    show('\n');
}

def main() -> null
{
    showln("\033[31mRouge (octal)\033[0m");
    showln("\x1b[32mVert (hex)\x1b[0m");
    showln("\u{1b}[34mBleu (unicode)\u{1b}[0m");
    showln("\033[1;33mJaune Gras\033[0m");
}
