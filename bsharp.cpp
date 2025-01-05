#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <fstream>
#include <unordered_map>

enum TokenType {
    Keyword,
    Identifier,
    Literal,
    Punctuation,
    EoF
};

enum ErrorType {
    // Lexer
    PunctuationNotMatched,
    UnexpectedEOF,
    // Preprocessor
    InvalidPreprocessingDirective,
    BadMacroName,
    FileNotFound
};

struct Token {
    TokenType type;
    std::string value;
    unsigned int line;
    unsigned int column;
};

void error(ErrorType err, Token token) {
    std::cerr << "Error at line " << token.line << ", column " << token.column << ": " << std::endl;
    switch (err) {
        case PunctuationNotMatched:
            std::cerr << "Punctuation not matched: " << token.value << " not closed" << std::endl;
            break;
        case UnexpectedEOF:
            std::cerr << "Unexpected EoF" << std::endl;
            break;
        case InvalidPreprocessingDirective:
            std::cerr << "Invalid preprocessing directive: " << token.value << std::endl;
            break;
        case BadMacroName:
            std::cerr << "Macro names must be identifiers" << std::endl;
            break;
        case FileNotFound:
            std::cerr << token.value << ": No such file or directory" << std::endl;
            break;
    }
    exit(1);
}

class Lexer {
public:
    Lexer(const std::string& source) : source(source), currentPos(0), line(0), column(0) {}
    
    std::vector<Token> tokenize() {
        std::vector<Token> tokens;
        while (currentPos < source.size()) {
            if (isspace(source[currentPos])) {
                if (source[currentPos] == '\n') {
                    line++;
                    column = 0;
                } else {
                    column++;
                }
                currentPos++;
            } else if (isalpha(source[currentPos]) || source[currentPos] == '_') {
                tokens.push_back(identifier());
            } else if (isdigit(source[currentPos])) {
                tokens.push_back(number());
            } else if (source[currentPos] == ';') {
                tokens.push_back({Punctuation, ";", line, column});
                column++; currentPos++;
            } else if (source[currentPos] == ':') {
                tokens.push_back({Punctuation, ":", line, column});
                column++; currentPos++;
            } else if (source[currentPos] == '.') {
                tokens.push_back({Punctuation, ".", line, column});
                column++; currentPos++;
            } else if (source[currentPos] == '#') {
                column++; currentPos++;
                preprocess();
            } else if (source[currentPos] == '?') {
                tokens.push_back({Punctuation, "?", line, column});
                column++; currentPos++;
            } else if (source[currentPos] == ',') {
                tokens.push_back({Punctuation, ",", line, column});
                column++; currentPos++;
            } else if (source[currentPos] == '(') {
                tokens.push_back({Punctuation, "(", line, column});
                column++; currentPos++;
            } else if (source[currentPos] == ')') {
                tokens.push_back({Punctuation, ")", line, column});
                column++; currentPos++;
            } else if (source[currentPos] == '[') {
                tokens.push_back({Punctuation, "[", line, column});
                column++; currentPos++;
            } else if (source[currentPos] == ']') {
                tokens.push_back({Punctuation, "]", line, column});
                column++; currentPos++;
            } else if (source[currentPos] == '{') {
                tokens.push_back({Punctuation, "{", line, column});
                column++; currentPos++;
            } else if (source[currentPos] == '}') {
                tokens.push_back({Punctuation, "}", line, column});
                column++; currentPos++;
            } else if (source[currentPos] == '\'') {
                tokens.push_back({Punctuation, "'", line, column});
                column++; currentPos++;
                std::string s;
                if (source[currentPos] == '\\') {
                    column++; currentPos++;
                    s = escape[source[currentPos]];
                } else {
                    s = source[currentPos];
                }
                column++; currentPos++;
                if (source[currentPos] != '\'') error(PunctuationNotMatched, {Punctuation, "\'", line, column});
                tokens.push_back({Literal, s, line, column-1});
                tokens.push_back({Punctuation, "'", line, column});
                column++; currentPos++;
            } else if (source[currentPos] == '\"') {
                tokens.push_back({Punctuation, "\"", line, column});
                column++; currentPos++;
                std::string s;
                while (source[currentPos] != '\"') {
                    if (source[currentPos] == '\\') {
                        column++; currentPos++;
                        s += escape[source[currentPos]];
                    } else {
                        s += source[currentPos];
                    }
                    column++; currentPos++;
                    if (currentPos == source.size()) {
                        error(UnexpectedEOF, {Punctuation, "\"", line, column});
                    }
                }
                tokens.push_back({Literal, s, line, (unsigned int)(column-(s.length()+1))});
                tokens.push_back({Punctuation, "\"", line, column});
                column++; currentPos++;
            } else if (source[currentPos] == '=') {
                if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "==", line, column});
                    column++; currentPos++;
                } else {
                    tokens.push_back({Punctuation, "=", line, column});
                }
                column++; currentPos++;
            } else if (source[currentPos] == '+') {
                if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "++", line, column});
                    column++; currentPos++;
                } else if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "+=", line, column});
                    column++; currentPos++;
                } else {
                    tokens.push_back({Punctuation, "+", line, column});
                }
                column++; currentPos++;
            } else if (source[currentPos] == '-') {
                if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "--", line, column});
                    column++; currentPos++;
                } else if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "-=", line, column});
                    column++; currentPos++;
                } else if (isdigit(source[currentPos+1])) {
                    column++; currentPos++;
                    tokens.push_back(number(true));
                    column--; currentPos--;
                } else {
                    tokens.push_back({Punctuation, "-", line, column});
                }
                column++; currentPos++;
            } else if (source[currentPos] == '>') {
                if (source[currentPos+1] == '>' && source[currentPos+2] == '=') {
                    tokens.push_back({Punctuation, ">>=", line, column});
                    column++; currentPos++;
                } else if (source[currentPos+1] == '>') {
                    tokens.push_back({Punctuation, ">>", line, column});
                    column++; currentPos++;
                } else if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, ">=", line, column});
                    column++; currentPos++;
                } else {
                    tokens.push_back({Punctuation, ">", line, column});
                }
                column++; currentPos++;
            } else if (source[currentPos] == '<') {
                if (source[currentPos+1] == '<' && source[currentPos+2] == '=') {
                    tokens.push_back({Punctuation, "<<=", line, column});
                    column++; currentPos++;
                } else if (source[currentPos+1] == '<') {
                    tokens.push_back({Punctuation, "<<", line, column});
                    column++; currentPos++;
                } else if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "<=", line, column});
                    column++; currentPos++;
                } else {
                    tokens.push_back({Punctuation, "<", line, column});
                }
                column++; currentPos++;
            } else if (source[currentPos] == '/') {
                if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "/=", line, column});
                    column++; currentPos++;
                } else if (source[currentPos+1] == '/') {
                    while (source[currentPos] != '\n' && source[currentPos] != 0) {
                        column++; currentPos++;
                    }  
                } else if (source[currentPos+1] == '*') {
                    column+=2; currentPos+=2;
                    while (source[currentPos] != '*' && source[currentPos+1] != '/') {
                        column++; currentPos++;
                        if (currentPos == source.size()) {
                            error(UnexpectedEOF, {Punctuation, "\"", line, column});
                        }
                    }  
                    column++; currentPos++;
                } else {
                    tokens.push_back({Punctuation, "/", line, column});
                }
                column++; currentPos++;
            } else if (source[currentPos] == '*') {
                if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "*=", line, column});
                    column++; currentPos++;
                } else {
                    tokens.push_back({Punctuation, "*", line, column});
                }
                column++; currentPos++;
            } else if (source[currentPos] == '%') {
                if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "%=", line, column});
                    column++; currentPos++;
                } else {
                    tokens.push_back({Punctuation, "%", line, column});
                }
                column++; currentPos++;
            } else if (source[currentPos] == '^') {
                if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "^=", line, column});
                    column++; currentPos++;
                } else {
                    tokens.push_back({Punctuation, "^", line, column});
                }
                column++; currentPos++;
            } else if (source[currentPos] == '&') {
                if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "&&", line, column});
                    column++; currentPos++;
                } else if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "&=", line, column});
                    column++; currentPos++;
                } else {
                    tokens.push_back({Punctuation, "&", line, column});
                }
                column++; currentPos++;
            } else if (source[currentPos] == '|') {
                if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "||", line, column});
                    column++; currentPos++;
                } else if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "|=", line, column});
                    column++; currentPos++;
                } else {
                    tokens.push_back({Punctuation, "|", line, column});
                }
                column++; currentPos++;
            } else if (source[currentPos] == '!') {
                if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "!=", line, column});
                    column++; currentPos++;
                } else {
                    tokens.push_back({Punctuation, "!", line, column});
                }
                column++; currentPos++;
            } else if (source[currentPos] == '~') {
                if (source[currentPos+1] == '=') {
                    tokens.push_back({Punctuation, "~=", line, column});
                    column++; currentPos++;
                } else {
                    tokens.push_back({Punctuation, "~", line, column});
                }
                column++; currentPos++;
            } else {
                column++; currentPos++;
            }
        }
        tokens.push_back({EoF, "", line, column});
        return tokens;
    }

private:
    std::string source;
    size_t currentPos;
    unsigned int line, column;
    std::string keywords[29] = {
        // Structures
        "if", "else", "while", "for", "do", "switch", "case", "default", "break", "continue", "fn", "return",
        // Data types (reserved)
        "int", "float", "bool", "char", "short", "long", "double", "enum",
        // Storage duration
        "auto", "static", "extrn",
        // Other
        "import", "alloc", "realloc", "destroy", "struct",
        // VERY important
        "gooning"
        };

    std::unordered_map<char, char> escape = {
        {'a', 0x07},
        {'b', 0x08},
        {'e', 0x1B},
        {'f', 0x0C},
        {'n', 0x0A},
        {'r', 0x0D},
        {'t', 0x09},
        {'v', 0x0B},
    };

    std::unordered_map<std::string, std::string> macros;

    Token identifier() {
        std::string id;
        while (currentPos < source.size() && (isalnum(source[currentPos]))) {
            id += source[currentPos++];
            column++;
        }

        if (macros.size() > 0) {
            if (macros.find(id) != macros.end()) {
                id = macros[id];
            }
        }

        for (int i = 0; i < 29; i++) {
            if (keywords[i] == id) {
                return {Keyword, id};
            }
        }

        return {Identifier, id, line, (unsigned int)(column-(id.length()))};
    }

    Token number(bool negative = false) {
        std::string num = negative ? "-" : "";
        while (currentPos < source.size() && isdigit(source[currentPos])) {
            num += source[currentPos++];
            column++;
            if (source[currentPos] == 'x' || source[currentPos] == 'X') {
                column++; currentPos++;
                num = "";
                while (currentPos < source.size() && (isalnum(source[currentPos]))) {
                    num += source[currentPos++];
                    column++;
                }
                return {Literal, std::to_string(stoi(num, nullptr, 16)), line, column};
            } else if (source[currentPos] == 'b' || source[currentPos] == 'B') {
                column++; currentPos++;
                num = "";
                while (currentPos < source.size() && (isdigit(source[currentPos]))) {
                    num += source[currentPos++];
                    column++;
                }
                return {Literal, std::to_string(stoi(num, nullptr, 2)), line, column};
            }
        }
        return {Literal, num, line, column};
    }

    void preprocess() {
        std::vector<std::string> directives;
        std::string directive;
        while (currentPos < source.size()) {
            directive += source[currentPos++];
            column++;
            if (isspace(source[currentPos]) || currentPos == source.size()) {
                directives.push_back(directive);
                directive = "";
                if (currentPos == source.size()) break;
                currentPos++;
                if (source[currentPos-1] == '\n') {
                    column = 0;
                    line++;
                    break;
                } else {
                    column++;
                }
            }
        }
        if (directives[0] == "define") {
            if (directives.size() < 2) {
                    error(BadMacroName, {Identifier, "", line, 0});
            }
            if (isalpha(directives[1][0]) || directives[1][0] == '_') {
                if (directives.size() > 2) {
                    macros[directives[1]] = directives[2];
                }
            } else {
                error(BadMacroName, {Identifier, directives[1], line, 0});
            }
        } else if (directives[0] == "undef") {
            if (directives.size() < 2) {
                    error(BadMacroName, {Identifier, "", line, 0});
            }
            if (macros.size() > 0) {
                if (macros.find(directives[1]) != macros.end()) {
                    macros.erase(directives[1]);
                }
            }
        } else if (directives[0] == "include") {
            if (directives.size() < 2) {
                error(FileNotFound, {Identifier, "", line, 0});
            }
            std::ifstream include(directives[1]);
            if (!include.is_open()) {
                error(FileNotFound, {Identifier, directives[1], line, 0});
            }
            std::stringstream buffer;
            buffer << include.rdbuf(); 
            source.insert(currentPos, buffer.str());
        } else {
            // TODO: conditional!
            error(InvalidPreprocessingDirective, {Keyword, directives[0], line, 0});
        }
    }
};

int main(int argc, char* argv[]) {
    std::string sourceCode = "#include std.h\nauto xoxo = nuh;";
    Lexer lexer(sourceCode);
    std::vector<Token> tokens = lexer.tokenize();
    int it = 0;
    for (const auto& token : tokens) {
        std::cout << it << " Token: " << token.value << " Type: " << static_cast<int>(token.type) 
                << " Line: " << token.line << " Column: " << token.column << std::endl;
        it++;
    }
}