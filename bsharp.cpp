#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <sstream>
#include <fstream>
#include <unordered_map>
#include <map>
#include <memory>
#define print std::cout<<tokens[current].value<<std::endl;
enum TokenType {
    Keyword,
    Identifier,
    Literal,
    Punctuation,
    EoF
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

    Expression() {};
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

    LiteralExpression() {};
    LiteralExpression(int integer) : type(Integer), integer(integer) {};
    LiteralExpression(char character) : type(Char), integer((int)character) {};
    LiteralExpression(float floating) : type(Floating), floating(floating) {};
};

struct VariableReference {
    enum Type {
        Value,
        Reference,
        Pointer,
        Member // what i called NoVar in the other compiler. it's whenever the position is an expression (pointed by to AX/R1, i'd assume)
    } type;

    enum Scope {
        Global,
        Local
    } scope;

    std::string identifier;
    Expression position; // it's usually a literal for Values and Pointers! it's another VariableReference of type Value for References, and something else for Members.
    VariableReference() {};
    VariableReference(std::string identifier, Expression position, Scope scope) : type(Member), identifier(identifier), position(position), scope(scope) {};
    VariableReference(std::string identifier, Type type, Scope scope) : type(type), identifier(identifier), position(position), scope(scope) {};
};

struct Assignment { // remember! assignments ALWAYS return the variable after assignment, even x++ (it is no different from ++x)
    bool augmentedAssignment = false; // if true, no registers are involved with the first part of the assignment. e.g., x+=1 directly does ADD X, 1.
    VariableReference variable;
    Expression value;
    Assignment() {};
};

struct Operation { // it also includes conditions. e.g. x>y is an operation with > as the operation. handled different in translation, though.
    Expression a;
    Expression b;
    std::string operation;
    Operation() {};
    Operation(Expression a, Expression b, std::string operation) : a(a), b(b), operation(operation) {};
};

struct TernaryOperation {
    Expression a;
    Expression b;
    Expression condition; // C is for condition!
    TernaryOperation() {};
    TernaryOperation(Expression a, Expression b, Expression condition) : a(a), b(b), condition(condition) {};
};

struct FunctionCall {
    std::string identifier;
    std::vector<Expression> arguments;
    FunctionCall() {};
};

struct Block; struct FunctionDefinition; struct IfStatement; struct SwitchStatement; struct WhileLoop; struct ForLoop; struct ReturnCall; struct BreakCall; struct ContinueCall;

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
        Other
    } type;

    Expression expression;
    Block* block;
    FunctionDefinition* functionDefinition;
    IfStatement* ifStatement;
    SwitchStatement* switchStatement;
    WhileLoop* whileLoop;
    ForLoop* forLoop;
    ReturnCall* returnCall;
    BreakCall* breakCall;

    Statement() : type(Other) {};
    Statement(Expression expression) : type(ExpressionType), expression(expression) {};
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
    bool elsePresent = false;
};

struct SwitchStatement {
    Expression condition;
    std::map<int, Statement> branches;
    Statement defaultBranch;
    bool defaultPresent = false;
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

enum ErrorType {
    // Lexer
    PunctuationNotMatched,
    UnexpectedEOF,
    // Preprocessor
    InvalidPreprocessingDirective,
    BadMacroName,
    FileNotFound,
    // Parser
    InvalidCase,
    ReservedIdentifier,
    UnexpectedToken // literally heaven and earth
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
            std::cerr << token.value << ": no such file or directory" << std::endl;
            break;
        case InvalidCase:
            std::cerr << token.value << ": invalid case for switch statement" << std::endl;
            break;
        case ReservedIdentifier:
            std::cerr << token.value << ": identifier is a reserved keyword" << std::endl;
            break;
        case UnexpectedToken:
            std::cerr << "Unexpected token: " << token.value << std::endl;
            break;
    }
    exit(1);
}

std::map<std::string, int> vartable; // only global variables. for extrn, the int is -1, and is solved at linking time.
std::vector<std::string> funtable;


// DEBUG PURPOSES ONLY!
void printExpression(Expression exp, int indent) {
    switch (exp.type) {
        case 0:
        {
            LiteralExpression x = *exp.literalExpression;
            switch (x.type) {
                case 0:
                {
                    std::cout << std::string(indent, ' ') << "Integer: " << x.integer << std::endl;
                    break;
                }
                case 1:
                {
                    std::cout << std::string(indent, ' ') << "Floating: " << x.floating << std::endl;
                    break;
                }
                case 2:
                {
                    std::cout << std::string(indent, ' ') << "Char: " << (char)x.integer << std::endl;
                    break;
                }
                case 3:
                {
                    std::cout << std::string(indent, ' ') << "Array: " << std::endl;
                    for (int i = 0; i < x.array.size(); i++) {
                        printExpression(x.array[i], indent+2);
                    }
                    break;
                }
            }
            break;
        }
        case 1:
        {
            VariableReference x = *exp.variableReference;
            switch (x.type) {
                case 0:
                {
                    std::cout << std::string(indent, ' ') << "Variable: " << x.identifier << std::endl;
                    std::cout << std::string(indent+2, ' ') << "Scope: " << x.scope << std::endl;
                    break;
                }
                case 1:
                {
                    std::cout << std::string(indent, ' ') << "Reference: " << x.identifier << std::endl;
                    break;
                }
                case 2:
                {
                    std::cout << std::string(indent, ' ') << "Pointer: " << x.identifier << std::endl;
                    break;
                }
                case 3:
                {
                    std::cout << std::string(indent, ' ') << "Pointer: " << std::endl;
                    printExpression(x.position, indent+2);
                    break;
                }
            }
            break;
        }
        case 2:
        {
            Assignment x = *exp.assignment;
            if (x.augmentedAssignment) {
                std::cout << std::string(indent, ' ') << "Augmented assignment: " << std::endl;
            } else {
                std::cout << std::string(indent, ' ') << "Assignment: " << std::endl;
            }
            printExpression(Expression(&x.variable), indent+2);
            printExpression(x.value, indent+2);
            
            break;
        }
        case 3:
        {
            Operation x = *exp.operation;
            std::cout << std::string(indent, ' ') << "Operation: " << x.operation << std::endl;
            printExpression(x.a, indent+2);
            printExpression(x.b, indent+2);
            break;
        }
        case 4:
        {
            TernaryOperation x = *exp.ternaryOperation;
            std::cout << std::string(indent, ' ') << "Ternary Operation: " << std::endl;
            std::cout << std::string(indent+2, ' ') << "Condition: " << std::endl;
            printExpression(x.condition, indent+4);
            std::cout << std::string(indent+2, ' ') << "True value: " << std::endl;
            printExpression(x.a, indent+4);
            std::cout << std::string(indent+2, ' ') << "False value: " << std::endl;
            printExpression(x.b, indent+4);
            break;
        }
        case 5:
        {
            FunctionCall x = *exp.functionCall;
            std::cout << std::string(indent, ' ') << "Function call: " << x.identifier << std::endl;
            std::cout << std::string(indent+2, ' ') << "Arguments: " << std::endl;
            for (int i = 0; i < x.arguments.size(); i++) {
                printExpression(x.arguments[i], indent+4);
            }
            break;
        }
    }
}

void printNode(Statement statement, int indent) {
    switch (statement.type) {
        case 0:
        {
            Expression exp = statement.expression;
            printExpression(exp, indent);
            break;
        }
        case 1:
        {
            Block* x = statement.block;
            std::cout << std::string(indent, ' ') << "Block: " << std::endl;
            for (int i = 0; i < x->statements.size(); i++) {
                printNode(x->statements[i], indent+2);
            }
            break;
        }
        case 2:
        {
            FunctionDefinition* x = statement.functionDefinition;
            std::cout << std::string(indent, ' ') << "Function definition: " << x->identifier << std::endl;
            std::cout << std::string(indent+2, ' ') << "Parameters: ";
            for (int i = 0; i < x->parameters.size(); i++) {
                std::cout << x->parameters[i] << ", ";
            }
            std::cout << "\b\b \n";
            std::cout << std::string(indent+2, ' ') << "Body: " << std::endl;
            printNode(x->body, indent+4);
            break;
        }
        case 3:
        {
            IfStatement* x = statement.ifStatement;
            std::cout << std::string(indent, ' ') << "If statement: " << std::endl;
            std::cout << std::string(indent+2, ' ') << "Condition: " << std::endl;
            printExpression(x->condition, indent+4);
            std::cout << std::string(indent+2, ' ') << "Then Branch: " << std::endl;
            printNode(x->thenBranch, indent+4);
            if (x->elsePresent) {
                std::cout << std::string(indent+2, ' ') << "Else Branch: " << std::endl;
                printNode(x->elseBranch, indent+4);
            }
            break;
        }
        case 4:
        {
            SwitchStatement* x = statement.switchStatement;
            std::cout << std::string(indent, ' ') << "Switch statement: " << std::endl;
            std::cout << std::string(indent+2, ' ') << "Condition: " << std::endl;
            printExpression(x->condition, indent+4);
            for (const auto& pair : x->branches) {
                std::cout << std::string(indent+2, ' ') << "Case " << pair.first << ":" << std::endl;
                printNode(pair.second, indent+4);
            }
            if (x->defaultPresent) {
                std::cout << std::string(indent+2, ' ') << "Default: " << std::endl;
                printNode(x->defaultBranch, indent+4);
            }
            break;
        }
        case 5:
        {
            WhileLoop* x = statement.whileLoop;
            std::cout << std::string(indent, ' ') << "While loop: " << std::endl;
            std::cout << std::string(indent+2, ' ') << "Condition: " << std::endl;
            printExpression(x->condition, indent+4);
            std::cout << std::string(indent+2, ' ') << "Body: " << std::endl;
            printNode(x->body, indent+4);
            break;
        }
        case 6:
        {
            ForLoop* x = statement.forLoop;
            std::cout << std::string(indent, ' ') << "For loop: " << std::endl;
            std::cout << std::string(indent+2, ' ') << "Condition: " << std::endl;
            printExpression(x->condition, indent+4);
            std::cout << std::string(indent+2, ' ') << "Initializer: " << std::endl;
            printNode(x->init, indent+4);
            std::cout << std::string(indent+2, ' ') << "Increment: " << std::endl;
            printNode(x->increment, indent+4);
            std::cout << std::string(indent+2, ' ') << "Body: " << std::endl;
            printNode(x->body, indent+4);
            break;
        }
        case 7:
        {
            ReturnCall* x = statement.returnCall;
            std::cout << std::string(indent, ' ') << "Return call: " << std::endl;
            printExpression(x->toReturn, indent+2);
            break;
        }
        case 8:
        {
            BreakCall* x = statement.breakCall;
            if (x->abrupt) {
                std::cout << std::string(indent, ' ') << "Break call";
            } else {
                std::cout << std::string(indent, ' ') << "Continue call";
            }
            break;
        }
    }
}

class Parser {
public:
    Parser(const std::vector<Token>& tokens) : tokens(tokens), current(0) {}
    std::vector<Statement> parse() {
        std::vector<Statement> ast;
        while (current < tokens.size() && tokens[current].type != EoF) {
            ast.push_back(parseStatement());
        }
        return ast;
    }
private:
    const std::vector<Token>& tokens;
    size_t current;
    
    // If the current token is of the right type, its value is returned. Otherwise, an error is thrown.
    std::string sanitize(TokenType type) {
        if (tokens[current].type != type) {
            if (type == Identifier) {
                error(ReservedIdentifier, tokens[current]);
            } else {
                error(UnexpectedToken, tokens[current]);
            }
        }
        return tokens[current].value;
    }

    // If a token is the expected one, it is skipped.
    void skip(std::string token) {
        if (tokens[current].value != token) {
                error(UnexpectedToken, tokens[current]);
        }
        current++;
    }

    Statement parseStatement() {
        std::cout << "Parsing token: " << tokens[current].value << std::endl;
        if      (tokens[current].value == "if")         return parseIfStatement();
        else if (tokens[current].value == "switch")     return parseSwitchStatement();
        else if (tokens[current].value == "while")      return parseWhileLoop();
        else if (tokens[current].value == "for")        return parseForLoop();
        else if (tokens[current].value == "{")          return parseBlock();
        else if (tokens[current].value == "return")     return parseReturnCall();
        else if (tokens[current].value == "break")      return parseBreakCall();
        else if (tokens[current].value == "continue")   return parseContinueCall();
        else if (tokens[current].value == "auto" ||
                 tokens[current].value == "static" ||
                 tokens[current].value == "extrn")      return parseDeclaration();
        else if (tokens[current].value == "fn")         return parseFunctionDefinition();
        else if (tokens[current].value == ";")          {current++; return Statement(new Block());}
        else if (tokens[current].type == EoF)           error(UnexpectedEOF, tokens[current]);
        else                                            return parseExpression();
        return Statement(); // to make gcc SHUT THE FUCK UP ABOUT CONTROL REACHING END OF NON-VOID FUNCTION. ITS NOT GONNA HAPPEN.
    }

    const std::string augment[15] = {"+=","-=",">>=","<<=","/=","*=","%=","^=","&=","|=","!=","~=", "++", "--", "!!"};

    VariableReference parseIdentifier();

    LiteralExpression parseArray();

    Statement parseExpression() {
        std::vector<Expression> expressions;
        std::string op = "";
        
        while (tokens[current].value != ";" && tokens[current].value != ")" && tokens[current].value != "," && tokens[current].value != "]" && current < tokens.size()-1) {
            if (tokens[current].type == Identifier && tokens[current+1].value == "(") {
                std::cout<<"Function call"<<std::endl;
                FunctionCall* ret = new FunctionCall();
                ret->identifier = tokens[current++].value;
                current++; // Skip "("
                while (tokens[current].value != ")") {
                    ret->arguments.push_back(parseExpression().expression);
                    if (tokens[current].value == ",") current++; // Skip the commas
                }
                current++; // Skip ")"
                current++; // Skip ";"
                expressions.push_back(ret);
            } else if (tokens[current+1].value == "=") {
                // Assignment
                VariableReference var = VariableReference(tokens[current++].value, VariableReference::Value, VariableReference::Global);
                current++; // Skip "="
                Expression value = parseExpression().expression;
                
                Assignment* ret = new Assignment();
                ret->variable = var;
                ret->value = value;
                if (tokens[current].value == ";") current++;
                expressions.push_back(ret);
                break;
            } else if (std::find(std::begin(augment), std::end(augment), tokens[current+1].value) != std::end(augment)) {
                // Augmented assignment
                VariableReference* var = new VariableReference(tokens[current++].value, VariableReference::Value, VariableReference::Global);
                std::string operation(1, tokens[current].value[0]);
                Operation* value = new Operation();
                value->a = Expression(var);

                if (tokens[current].value == "++" || tokens[current].value == "--" || tokens[current].value == "!!") {
                    current++; // Skip the operation
                    value->b = Expression(new LiteralExpression(1));
                } else {
                    if (operation == ">") operation = ">>";
                    if (operation == "<") operation = "<<";
                    current++; // Skip the operation
                    Expression exp = parseExpression().expression;
                    value->b = exp;
                }
                Assignment* ret = new Assignment();
                ret->augmentedAssignment = true;
                ret->variable = *var;
                value->operation = operation;
                ret->value = Expression(value);
                if (tokens[current].value == ";") current++;
                expressions.push_back(ret);
                break;
            } else if (tokens[current].value == "auto" || tokens[current].value == "static") {
                // Assignment coming from declaration
                current++; // Skip storage duration
                VariableReference var = VariableReference(tokens[current++].value, VariableReference::Value, VariableReference::Global);
                while (tokens[current].value != "=") current++;
                current++; // Skip "="
                Expression value = parseExpression().expression;

                Assignment* ret = new Assignment();
                ret->variable = var;
                ret->value = value;
                expressions.push_back(ret);
                break;
            } else if (tokens[current].type == Identifier) {
                // Variable
                VariableReference* var = new VariableReference(parseIdentifier());
                expressions.push_back(Expression(var));
            } else if (tokens[current].value == "[" || tokens[current].value == "\"") {
                // Array
                LiteralExpression* arr = new LiteralExpression(parseArray());
                expressions.push_back(Expression(arr));
            } else if (tokens[current].type == Literal || tokens[current].value == "'") {
                // Literal
                LiteralExpression* lit = new LiteralExpression();

                if (isdigit(tokens[current].value[0])) {
                    *lit = LiteralExpression(stoi(tokens[current].value)); // TODO: floats
                } else {
                    skip("'");
                    *lit = LiteralExpression(tokens[current].value[0]);
                    current++; // Skip "'"
                }

                current++; // Skip self
                expressions.push_back(Expression(lit));
            } else if (tokens[current].value == "(") {
                current++; // Skip "("
                expressions.push_back(parseExpression().expression);
                current++; // Skip ")"
            } else if (tokens[current].type == Punctuation) {
                op = tokens[current++].value;
            }
        }
    
        if (expressions.size() == 1) {
            return Statement(expressions[0]);
        } else if (expressions.size() == 2) {
            Operation* ret = new Operation(expressions[0], expressions[1], op);
            return Statement(Expression(ret));
        } else {
            if (op == ":") {
                TernaryOperation* ret = new TernaryOperation(expressions[1], expressions[2], expressions[0]);
                return Statement(Expression(ret));
            }
            error(UnexpectedToken, tokens[current-1]); // TODO: fix i think
            return Statement();
        }
    }

    Statement parseBlock() {
        Block* ret = new Block();
        current++; // Skip "{"
        while (tokens[current].value != "}") {
            ret->statements.push_back(parseStatement());
        }
        current++; // Skip "}"
        return Statement(ret);
    }

    Statement parseFunctionDefinition() {
        FunctionDefinition* ret = new FunctionDefinition();
        current++; // Skip "fn"
        ret->identifier = tokens[current++].value;
        skip("("); // Skip "("
        while (tokens[current].value != ")") {
            ret->parameters.push_back(tokens[current++].value);
            if (tokens[current].value == ",") {
                current++; // Skip ","
            }
        }
        current++; // Skip ")"
        ret->body = parseStatement();
        return Statement(ret);
    }

    Statement parseIfStatement() {
        IfStatement* ret = new IfStatement();
        current++; // Skip "if"
        skip("("); // Skip "("
        ret->condition = parseExpression().expression;
        current++; // Skip ")"
        ret->thenBranch = parseStatement();
        if (tokens[current].value == "else") {
            current++; // Skip "else"
            ret->elsePresent = true;
            ret->elseBranch = parseStatement();
        }
        return Statement(ret);
    }

    Statement parseSwitchStatement() {
        SwitchStatement* ret = new SwitchStatement();
        current++; // Skip "switch"
        skip("("); // Skip "("
        ret->condition = parseExpression().expression;
        current++; // Skip ")"
        skip("{"); // Skip "{"
        while (tokens[current].value != "}") {
            if (tokens[current].value == "case") {
                current++; // Skip "case"
                if (tokens[current].type != Literal) error(InvalidCase, tokens[current]);
                int cs = stoi(tokens[current++].value);
                current++; // Skip ":"
                Block* switchblock = new Block();
                while (tokens[current].value != "case" && tokens[current].value != "default" && tokens[current].value != "}") {
                    switchblock->statements.push_back(parseStatement());
                }
                ret->branches[cs] = Statement(switchblock);
            } else if (tokens[current].value == "default") {
                current++; // Skip "default"
                current++; // Skip ":"
                Block* switchblock = new Block();
                while (tokens[current].value != "case" && tokens[current].value != "}") {
                    switchblock->statements.push_back(parseStatement());
                }
                ret->defaultPresent = true;
                ret->defaultBranch = Statement(switchblock);
            }
        }
        current++; // Skip "}"
        return Statement(ret);
    }

    Statement parseWhileLoop() {
        WhileLoop* ret = new WhileLoop();
        current++; // Skip "while"
        skip("("); // Skip "("
        ret->condition = parseExpression().expression;
        current++; // Skip ")"
        ret->body = parseStatement();
        return Statement(ret);
    }

    Statement parseForLoop() {
        ForLoop* ret = new ForLoop();
        current++; // Skip "for"
        skip("("); // Skip "("
        ret->init = parseStatement();
        ret->condition = parseExpression().expression;
        current++; // Skip ";"
        ret->increment = parseStatement();
        current++; // Skip ")"
        ret->body = parseStatement();
        return Statement(ret);
    }

    Statement parseReturnCall() {
        ReturnCall* ret = new ReturnCall();
        current++; // Skip "return"
        if (tokens[current].value == ";") {
            ret->toReturn = Expression(new LiteralExpression(0));
        } else {
            ret->toReturn = parseExpression().expression;
        }
        current++; // Skip ";"
        return Statement(ret);
    }

    Statement parseBreakCall() {
        BreakCall* ret = new BreakCall();
        ret->abrupt = true;
        current++; // Skip "break"
        current++; // Skip ";"
        return Statement(ret);
    }

    Statement parseContinueCall() {
        BreakCall* ret = new BreakCall();
        ret->abrupt = false;
        current++; // Skip "continue"
        current++; // Skip ";"
        return Statement(ret);
    }

    Statement parseDeclaration() {
        std::string duration = tokens[current].value;
        int startpos = current;
        current++; // Skip "auto"/"static"/"extrn"
        if (duration == "extrn") {
            if (tokens[current].value == "fn") {
                current++; // Skip "fn"
                funtable.push_back(sanitize(Identifier));
                current++; // Skip identifier
                skip(";");
            } else {
                if (tokens[current+1].value == "[") {
                    std::string id = sanitize(Identifier);
                    vartable[id] = -1;
                    current++; // Skip identifier 
                    current++; // Skip "["
                    for (int i = 1; i < stoi(sanitize(Literal)); i++) {
                        vartable[id + std::to_string(i)] = -1;
                    }
                    current++; // Skip the number inside the brackets
                    current++; // Skip "]"
                    skip(";");
                } else {
                    vartable[sanitize(Identifier)] = -1;
                    current++; // Skip identifier
                    skip(";");
                }
            }
        } else if (duration == "static") {
            if (tokens[current+1].value == "[") {
                std::string id = sanitize(Identifier);
                vartable[id] = -1;
                current++; // Skip identifier 
                current++; // Skip "["'
                
                if (tokens[current].value == "]") {
                    current++; // Skip "]"
                    if (tokens[current].value != "=") error(UnexpectedToken, tokens[current]);
                    current++; // Skip "="
                    LiteralExpression arr = parseArray();
                    for (int i = 1; i < arr.array.size(); i++) {
                        vartable[id + std::to_string(i)] = -1;
                    }
                    current = startpos;
                    print
                    Statement ret = parseExpression();
                    current++; // Skip ";"
                    return ret;
                } else {
                    for (int i = 1; i < stoi(sanitize(Literal)); i++) {
                        vartable[id + std::to_string(i)] = -1;
                    }
                    current++; // Skip the number inside the brackets
                    current++; // Skip "]"
                    if (tokens[current].value != ";") {
                        current = startpos;
                        Statement ret = parseExpression();
                        current++; // Skip ";"
                        return ret;
                    }
                }
                
                
                current++; // Skip ";"
            } else {
                vartable[sanitize(Identifier)] = -1;
                current++; // Skip identifier
                if (tokens[current].value != ";") {
                        current = startpos;
                        Statement ret = parseExpression();
                        current++; // Skip ";"
                        return ret;
                    }
                current++; // Skip ";"
            }
        } else if (duration == "auto") {
            if (tokens[current+1].value == "[") {
                std::string id = sanitize(Identifier);
                vartable[id] = -1;
                current++; // Skip identifier 
                current++; // Skip "["'
                
                if (tokens[current].value == "]") {
                    current++; // Skip "]"
                    if (tokens[current].value != "=") error(UnexpectedToken, tokens[current]);
                    current++; // Skip "="
                    LiteralExpression arr = parseArray();
                    for (int i = 1; i < arr.array.size(); i++) {
                        vartable[id + std::to_string(i)] = -1;
                    }
                    current = startpos;
                    print
                    Statement ret = parseExpression();
                    current++; // Skip ";"
                    return ret;
                } else {
                    for (int i = 1; i < stoi(sanitize(Literal)); i++) {
                        vartable[id + std::to_string(i)] = -1;
                    }
                    current++; // Skip the number inside the brackets
                    current++; // Skip "]"
                    if (tokens[current].value != ";") {
                        current = startpos;
                        Statement ret = parseExpression();
                        current++; // Skip ";"
                        return ret;
                    }
                }
                
                
                current++; // Skip ";"
            } else {
                vartable[sanitize(Identifier)] = -1;
                current++; // Skip identifier
                if (tokens[current].value != ";") {
                        current = startpos;
                        Statement ret = parseExpression();
                        current++; // Skip ";"
                        return ret;
                    }
                current++; // Skip ";"
            }
            // TODO: handle auto variables (stack, also heap)
        }
        return Statement();
    }
};

VariableReference Parser::parseIdentifier() {
    if (tokens[current].value == "*") {
        // pointers, all of pointers.
        current++; // Skip "*"
        if (tokens[current].type == Identifier) {
            if (vartable.find(tokens[current].value) != vartable.end()) {
                return VariableReference(tokens[current++].value, VariableReference::Pointer, VariableReference::Global);
            } else {
                // Either local or doesn't exist. TODO: handle when i handle local (stack) variables.
            }
        } else {
            // "member"
            return VariableReference(tokens[current++].value, parseExpression().expression, VariableReference::Global);
        }
    } else if (tokens[current].value == "&") {
        // reference
        current++; // Skip "&"
        if (vartable.find(tokens[current].value) != vartable.end()) {
                return VariableReference(tokens[current++].value, VariableReference::Reference, VariableReference::Global);
            } else {
                // Either local or doesn't exist. TODO: handle when i handle local (stack) variables.
            }
    } else {
        // value
        std::string id = sanitize(Identifier);
        return VariableReference(tokens[current++].value, VariableReference::Value, VariableReference::Global);
    }
    return VariableReference(); // to make gcc shut the fuck up here too
}

LiteralExpression Parser::parseArray() {
    LiteralExpression lit;
    lit.type = LiteralExpression::Array;
    if (tokens[current].value == "[") {
        current++; // Skip "["
        while (tokens[current].value != "]") {
            lit.array.push_back(parseExpression().expression);
            if (tokens[current].value == ",") current++; // Skip the commas
        }
        current++; // Skip "]"
    } else if (tokens[current].value == "\"") {
        current++; // Skip "\""
        for (int i = 0; i < tokens[current].value.length(); i++) {
            lit.array.push_back(new LiteralExpression(tokens[current].value[i]));
        }
        current++; // Skip "\""
    } else {
        error(UnexpectedToken, tokens[current]);
    }
    return lit;
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
                if (source[currentPos+1] == '+') {
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
                if (source[currentPos+1] == '-') {
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
                } else if (source[currentPos+1] == '!') {
                    tokens.push_back({Punctuation, "!!", line, column});
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
        "int", "float", "bool", "char", "short", "long", "double", "enum", "struct", "template",
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

    std::string sourceCode = "switch (x) {case 1: {x = 1;} case 2: {z = 2;} default: {z = 6;}}";

    Lexer lexer(sourceCode);
    std::vector<Token> tokens = lexer.tokenize();
    int it = 0;
    for (const auto& token : tokens) {
        std::cout << it << " Token: " << token.value << " Type: " << static_cast<int>(token.type) 
                << " Line: " << token.line << " Column: " << token.column << std::endl;
        it++;
    }

    Parser parser(tokens);
    std::vector<Statement> ast = parser.parse();
    for (int i = 0; i < ast.size(); i++) {
        printNode(ast[i], 0);
    }
}