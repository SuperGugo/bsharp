#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <fstream>
#include <unordered_map>
#include <map>
#include <memory>

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

/*
    My philosophy regarding expressions and statements is simple: if it has a result in translation, 
    it is included (as either). This is the reason many such as Declaration Statements are not included: 
    they do not have an effect on translation. Technically speaking neither do Blocks, but they are
    included because they affect the way jumps are calculated during translation.
    Another very simple and obvious point is that Expressions always return something (which can be null)
    while statements never return anything. I also did not adopt the Rust way of separating them: where
    statements such as if, while, for, loop can all return something and be used in expressions.
*/

struct LiteralExpression; struct VariableReference; struct Assignment; struct Operation; struct TernaryOperation; struct FunctionCall;

struct Expression {
    enum Type {
        LiteralExpressionType,
        VariableReferenceType, // includes member access: array position ([]), template access (.), and future struct access (.)
        AssignmentType,
        OperationType,
        TernaryOperationType,
        FunctionCallType
    } type;

    LiteralExpression* literalExpression;
    VariableReference* variableReference;
    Assignment* assignment;
    Operation* operation;
    TernaryOperation* ternaryOperation;
    FunctionCall* functionCall;

    Expression(LiteralExpression* literalExpression) : type(LiteralExpressionType), literalExpression(literalExpression) {};
    Expression(VariableReference* variableReference) : type(VariableReferenceType), variableReference(variableReference) {};
    Expression(Assignment* assignment) : type(AssignmentType), assignment(assignment) {};
    Expression(Operation* operation) : type(OperationType), operation(operation) {};
    Expression(TernaryOperation* ternaryOperation) : type(TernaryOperationType), ternaryOperation(ternaryOperation) {};
    Expression(FunctionCall* functionCall) : type(FunctionCallType), functionCall(functionCall) {};
};

struct LiteralExpression {
    enum Type {
        Integer,
        Floating,
        Char,
        Array
    } type;

    int integer = 0; // also handles char
    float floating = 0.0;
    std::vector<Expression> array;
};

struct VariableReference {
    enum Type {
        Value,
        Reference,
        Pointer,
        Member // what i called NoVar in the other compiler. it's whenever the position is an expression (pointed by to AX/R1, i'd assume)
    } type;

    std::string identifier;
    std::vector<Expression> position; // it's usually a literal for Values and Pointers! it's another VariableReference of type Value for References, and something else for Members.
};

struct Assignment { // remember! assignments ALWAYS return the variable after assignment, even x++ (it is no different from ++x)
    bool augmentedAssignment = false; // if true, no registers are involved with the first part of the assignment. e.g., x+=1 directly does ADD X, 1.
    VariableReference variable;
    Expression value;
};

struct Operation { // it also includes conditions. e.g. x>y is an operation with > as the operation. handled different in translation, though.
    Expression a;
    Expression b;
    std::string operation;
};

struct TernaryOperation {
    Expression a;
    Expression b;
    Expression condition; // C is for condition!
};

struct FunctionCall {
    std::string identifier;
    std::vector<Expression> arguments;
};

struct Declaration; struct Block; struct FunctionDefinition; struct IfStatement; struct SwitchStatement; struct WhileLoop; struct ForLoop; struct ReturnCall; struct BreakCall; struct ContinueCall;

struct Statement {
    enum Type {
        ExpressionType,
        BlockType,
        FunctionDefinitionType,
        IfStatementType,
        SwitchStatementType,
        WhileLoopType,
        ForLoopType,
        ReturnCallType,
        BreakCallType,
    } type;

    Expression* expression;
    Block* block;
    FunctionDefinition* functionDefinition;
    IfStatement* ifStatement;
    SwitchStatement* switchStatement;
    WhileLoop* whileLoop;
    ForLoop* forLoop;
    ReturnCall* returnCall;
    BreakCall* breakCall;

    Statement(Expression* expression) : type(ExpressionType), expression(expression) {};
    Statement(Block* block) : type(BlockType), block(block) {};
    Statement(FunctionDefinition* functionDefinition) : type(FunctionDefinitionType), functionDefinition(functionDefinition) {};
    Statement(IfStatement* ifStatement) : type(IfStatementType), ifStatement(ifStatement) {};
    Statement(SwitchStatement* switchStatement) : type(SwitchStatementType), switchStatement(switchStatement) {};
    Statement(WhileLoop* whileLoop) : type(WhileLoopType), whileLoop(whileLoop) {};
    Statement(ForLoop* forLoop) : type(ForLoopType), forLoop(forLoop) {};
    Statement(ReturnCall* returnCall) : type(ReturnCallType), returnCall(returnCall) {};
    Statement(BreakCall* breakCall) : type(BreakCallType), breakCall(breakCall) {};
};

struct Block {
    std::vector<Statement> statements;
};

struct FunctionDefinition {
    std::string identifier;
    std::vector<std::string> parameters; // each parameter includes a declaration (local) during parsing.
    Statement body;
};

struct IfStatement {
    Expression condition;
    Statement thenBranch;
    Statement elseBranch;
};

struct SwitchStatement {
    Expression condition;
    std::map<int, Statement> branches;
};

struct WhileLoop {
    Expression condition;
    Statement body;
};

struct ForLoop {
    Statement init;
    Statement increment;
    Statement body;
    Expression condition;
};

struct ReturnCall {
    Expression toReturn;
};

struct BreakCall {
    bool abrupt; // false is continue, true is break
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
    std::string keywords[26] = {
        // Structures
        "if", "else", "while", "for", "do", "switch", "case", "default", "break", "continue", "fn", "return",
        // Data types (reserved)
        "int", "float", "bool", "char", "short", "long", "double", "enum", "struct", "template"
        // Storage duration
        "auto", "static", "extrn",
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

        for (int i = 0; i < 26; i++) {
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
    std::string sourceCode = "#include std.h\nauto xoxo = 3.5;";
    Lexer lexer(sourceCode);
    std::vector<Token> tokens = lexer.tokenize();
    int it = 0;
    for (const auto& token : tokens) {
        std::cout << it << " Token: " << token.value << " Type: " << static_cast<int>(token.type) 
                << " Line: " << token.line << " Column: " << token.column << std::endl;
        it++;
    }
}