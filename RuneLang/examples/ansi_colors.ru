def showln(value: string) -> null
{
    show(value);
    show('\n');
}

def main() -> null
{
    showln("\033[31mRed text(octal)\033[0m");
    showln("\x1b[32mGreen text (hex)\x1b[0m");
    showln("\u{1b}[34mBlue text (unicode)\u{1b}[0m");
    showln("\033[1;31mBold Red text\033[0m");
    showln("\x1b[38;5;196m256 color Red\x1b[0m");
    showln("\033[48;5;46m256 color Green background\033[0m");
    showln("\x1b[38;2;255;105;180mRose True Color\x1b[0m");
    showln("\u{1b}[48;2;0;255;255mFond Cyan True Color\u{1b}[0m");
    showln("\033[1;38;2;255;255;0;48;5;19mYellow Bold on Blue Background\033[0m");
}
