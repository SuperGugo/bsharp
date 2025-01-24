#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <sstream>
#include <fstream>
#include <unordered_map>
#include <map>
#include <memory>
#include <iomanip>

#define print std::cout<<tokens[current].value<<std::endl;



enum TokenType {
    Keyword,
    Identifier,
    Literal,
    Punctuation,
    Unknown,
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

struct TemplateItem; struct DataType; struct VariableData; struct LiteralExpression; struct VariableReference; struct Assignment; struct Operation; struct TernaryOperation; struct FunctionCall; struct Cast;

struct Expression {
    enum Type {
        LiteralExpressionType,
        VariableReferenceType, // includes member access: array position ([]), template access (.), and future struct access (.)
        AssignmentType,
        OperationType,
        TernaryOperationType,
        FunctionCallType,
        CastType,
        Other
    } type;

    std::shared_ptr<LiteralExpression> literalExpression;
    std::shared_ptr<VariableReference> variableReference;
    std::shared_ptr<Assignment> assignment;
    std::shared_ptr<Operation> operation;
    std::shared_ptr<TernaryOperation> ternaryOperation;
    std::shared_ptr<FunctionCall> functionCall;
    std::shared_ptr<Cast> cast;

    Expression() : type(Other) {};
    Expression(LiteralExpression* literalExpression) : type(LiteralExpressionType), literalExpression(literalExpression) {};
    Expression(VariableReference* variableReference) : type(VariableReferenceType), variableReference(variableReference) {};
    Expression(Assignment* assignment) : type(AssignmentType), assignment(assignment) {};
    Expression(Operation* operation) : type(OperationType), operation(operation) {};
    Expression(TernaryOperation* ternaryOperation) : type(TernaryOperationType), ternaryOperation(ternaryOperation) {};
    Expression(FunctionCall* functionCall) : type(FunctionCallType), functionCall(functionCall) {};
    Expression(Cast* cast) : type(CastType), cast(cast) {};
};

struct TemplateItem {
    std::string id = "";
    DataType* dataType;
    Expression defaultValue;
    bool hasDefaultValue = false;

    bool operator==(const TemplateItem& other) const {
        return id == other.id && dataType == other.dataType;
    }
};

struct DataType {
    enum Type {
        Char,
        Short,
        Int,
        Long,
        Float,          
        Double,
        AmbiguousInteger, // literal. doesnt do anything on its own but can match with anything
        Template,
        Array,
        Union,
        Enum
    } type = Int;
    DataType* arrayType;
    size_t arraySize = 1;
    std::vector<TemplateItem>* templ;
    bool pointer = false;
    bool sign = true;

    size_t size() const {
        if (pointer) {return Int;}
        switch (type) {
            case Array:
                return arraySize*arrayType->size();
            case Template:
            {
                size_t s = 0;
                for (const auto& item : *(templ)) {
                    s += item.dataType->size();
                }
                return s;
            }
            case Union:
            {
                size_t s = 0;
                for (const auto& item : *(templ)) {
                    if ( item.dataType->size() > s) {
                        s = item.dataType->size();
                    }
                }
                return s;
            }
            case Char:
                return 1;
            case Short:
                return 2;
            case Int:
                return 4;
            case Long:
                return 8;
            case Float:
                return 4;
            case Double:
                return 8;
            case AmbiguousInteger:
                return 8;
        }        
        return 0;
    }

    DataType(std::vector<TemplateItem>* templ) : type(Template), templ(templ) {};
    DataType(DataType* arrayType, size_t arraySize) : type(Array), arrayType(arrayType), arraySize(arraySize) {};
    DataType(Type type) : type(type) {};
    DataType() {};
};

struct VariableData {
    DataType dataType;
    std::string identifier;
    std::vector<int> scope;
    VariableData() {};
    VariableData(DataType dataType) : dataType(dataType) {};
};

struct LiteralExpression {
    enum Type {
        Integer,
        Floating,
        Array,
        TemplateInit
    } type;

    int integer = 0; // also handles char
    float floating = 0.0;
    std::vector<Expression> array;

    LiteralExpression() {};
    LiteralExpression(int integer) : type(Integer), integer(integer) {};
    LiteralExpression(float floating) : type(Floating), floating(floating) {};
};

struct VariableReference {
    enum Type {
        Value,
        Reference,
        Pointer
    } type;
    
    VariableData var;
    Expression position; // it's usually a literal for Values and Pointers! it's another VariableReference of type Value for References, and something else for Members.
    bool preserveArray = false;

    VariableReference() {};
    VariableReference(Expression position, VariableData var, bool preserveArray = false) : type(Pointer), var(var), preserveArray(preserveArray), position(position) {};
    VariableReference(Type type, VariableData var) : type(type), var(var) {};
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
    bool unary = false;
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

struct Cast {
    Expression from;
    DataType to;
    Cast() {};
};

struct Block; struct FunctionDefinition; struct IfStatement; struct SwitchStatement; struct WhileLoop; struct ForLoop; struct ReturnCall; struct BreakCall; struct ContinueCall; struct InlineAsm;

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
        InlineAsmType,
        Other
    } type;

    Expression expression;
    std::shared_ptr<Block> block;
    std::shared_ptr<FunctionDefinition> functionDefinition;
    std::shared_ptr<IfStatement> ifStatement;
    std::shared_ptr<SwitchStatement> switchStatement;
    std::shared_ptr<WhileLoop> whileLoop;
    std::shared_ptr<ForLoop> forLoop;
    std::shared_ptr<ReturnCall> returnCall;
    std::shared_ptr<BreakCall> breakCall;
    std::shared_ptr<InlineAsm> inlineAsm;

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
    Statement(InlineAsm* inlineAsm) : type(InlineAsmType), inlineAsm(inlineAsm) {};
};

struct Block {
    std::vector<Statement> statements;
};

struct FunctionDefinition {
    std::string identifier;
    std::vector<VariableData> parameters; // each parameter includes a declaration (local) during parsing.
    std::vector<Expression> defaults;
    Statement body;
    DataType returnType;
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
    bool doWhile = false;
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

struct InlineAsm {
    std::string code;
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
    UnexpectedToken, // literally heaven and earth
    UndefinedVariable,
    UndefinedFunction,
    VariableAlreadyDefined,
    FunctionAlreadyDefined, // overloading is not allowed
    ParameterMismatch,
    TypeMismatch,
    OperationNotAllowed,
    NonexistingMember,
    NonexistingTemplate
};

std::map<std::string, VariableData> vartable; // only global variables. for extrn, the int is -1, and is solved at linking time.
std::map<std::string, FunctionDefinition> funtable;
std::map<std::string, std::vector<TemplateItem>> templatetable;
std::map<std::string, std::map<std::string, FunctionDefinition>> methodtable;

std::string sourceCode;
std::string filename;

void error(ErrorType err, Token token) {
    std::cerr<<"\033[3;94m"<<filename<<"\033[0;97m at \033[1;93m"<<token.line+1<<":"<<token.column+1<<"\033[1;91m Fatal error: \033[0;97m";
    bool highlight = true;

    switch (err) {
        case PunctuationNotMatched:
            std::cerr << token.value << ": unmatched punctuation" << std::endl;
            break;
        case UnexpectedEOF:
            std::cerr << "unexpected EoF" << std::endl;
            highlight = false;
            break;
        case InvalidPreprocessingDirective:
            std::cerr << token.value << ": invalid preprocessing directive" << std::endl;
            break;
        case BadMacroName:
            if (token.value != "") {
                std::cerr << token.value << ": macro names must be identifiers" << std::endl;
            } else {
                std::cerr << "macro names must be identifiers" << std::endl;
                highlight = false;
            }
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
            std::cerr << "unexpected token: " << token.value << std::endl;
            break;
        case UndefinedVariable:
            std::cerr << "undefined variable: " << token.value << std::endl;
            break;
        case UndefinedFunction:
            std::cerr << "undefined function: " << token.value << std::endl;
            break;
        case VariableAlreadyDefined:
            std::cerr << "variable is already defined: " << token.value << std::endl;
            break;
        case FunctionAlreadyDefined:
            std::cerr << "function is already defined: " << token.value << std::endl;
            break;
        case ParameterMismatch:
            std::cerr << "wrong number of parameters" << std::endl;
            break;
        case TypeMismatch:
            std::cerr << "type mismatch" << std::endl;
            break;
        case OperationNotAllowed:
            std::cerr << "operation not allowed on this data type" << std::endl;
            highlight = false;
            break;
        case NonexistingMember:
            std::cerr << "member doesn't exist: " << token.value << std::endl;
            break;
        case NonexistingTemplate:
            std::cerr << "template doesn't exist: " << token.value << std::endl;
            break;
    }
    std::istringstream stream(sourceCode);
    std::string s;
    for (int i = 0; i <= token.line; i++) {
        std::getline(stream, s);
    }
    std::cerr<<"\033[93m"<<std::setw(5)<<token.line+1<<"\033[97m | ";
    for (int i = 0; i < s.length(); i++) {
        if (i == token.column && highlight) {
            std::cerr<<"\033[1;4m";
        }
        if (i == token.column + token.value.length()) {
            std::cerr<<"\033[0m";
        }
        std::cerr<<s[i];
    }
    std::cerr<<std::endl;
    exit(1);
}

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
                    std::cout << std::string(indent, ' ') << "Array: " << std::endl;
                    for (int i = 0; i < x.array.size(); i++) {
                        printExpression(x.array[i], indent+2);
                    }
                    break;
                }
                case 3:
                {
                    std::cout << std::string(indent, ' ') << "Template initialization: " << std::endl;
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
                    std::cout << std::string(indent, ' ') << "Variable: " << x.var.identifier << std::endl;
                    break;
                }
                case 1:
                {
                    std::cout << std::string(indent, ' ') << "Reference: " << x.var.identifier << std::endl;
                    break;
                }
                case 2:
                {
                    std::cout << std::string(indent, ' ') << "Pointer: " << std::endl;
                    break;
                }
            }
            if (x.type != 2 && x.var.scope.size() != 0) {
                std::cout << std::string(indent+2, ' ') << "Scope: ";
                for (int i = 0; i < x.var.scope.size(); i++) {
                    std::cout << x.var.scope[i] << " -> ";
                }
            } else {
                printExpression(x.position, indent+2);
            }
            std::cout<<"\b\b\b\b   \n";
            std::cout << std::string(indent+2, ' ') << "Data Type: " << x.var.dataType.type<< std::endl;
            std::cout << std::string(indent+2, ' ') << "Size: " << x.var.dataType.size() << std::endl;
            std::cout << std::string(indent+2, ' ') << "Array: " << x.var.dataType.arraySize << std::endl;
            std::cout << std::string(indent+2, ' ') << "Pointer: " << x.var.dataType.pointer << std::endl;
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
            printExpression(Expression(new VariableReference(x.variable)), indent+2);
            printExpression(x.value, indent+2);
            break;
        }
        case 3:
        {
            Operation x = *exp.operation;
            std::cout << std::string(indent, ' ') << "Operation: " << x.operation << std::endl;
            printExpression(x.a, indent+2);
            if (!x.unary) {
                printExpression(x.b, indent+2);
            }
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
        case 6:
        {
            Cast x = *exp.cast;
            std::cout << std::string(indent, ' ') << "Cast: " << std::endl;
            std::cout << std::string(indent+2, ' ') << "From: " << std::endl;
            printExpression(x.from, indent+4);
            std::cout << std::string(indent+2, ' ') << "To: " << std::endl;
            std::cout << std::string(indent+4, ' ') << "Data Type: " << x.to.type<< std::endl;
            std::cout << std::string(indent+4, ' ') << "Size: " << x.to.size() << std::endl;
            std::cout << std::string(indent+4, ' ') << "Pointer: " << x.to.pointer << std::endl;

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
            std::shared_ptr<Block> x = statement.block;
            std::cout << std::string(indent, ' ') << "Block: " << std::endl;
            for (int i = 0; i < x->statements.size(); i++) {
                printNode(x->statements[i], indent+2);
            }
            break;
        }
        case 2:
        {
            std::shared_ptr<FunctionDefinition> x = statement.functionDefinition;
            std::cout << std::string(indent, ' ') << "Function definition: " << x->identifier << std::endl;
            std::cout << std::string(indent+2, ' ') << "Parameters: ";
            for (int i = 0; i < x->parameters.size(); i++) {
                std::cout << x->parameters[i].identifier << ", ";
            }
            std::cout << "\b\b \n";
            std::cout << std::string(indent+2, ' ') << "Body: " << std::endl;
            printNode(x->body, indent+4);
            break;
        }
        case 3:
        {
            std::shared_ptr<IfStatement> x = statement.ifStatement;
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
            std::shared_ptr<SwitchStatement> x = statement.switchStatement;
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
            std::shared_ptr<WhileLoop> x = statement.whileLoop;
            if (x->doWhile) {
                std::cout << std::string(indent, ' ') << "Do-While loop: " << std::endl;
            } else {
                std::cout << std::string(indent, ' ') << "While loop: " << std::endl;
            }
            std::cout << std::string(indent+2, ' ') << "Condition: " << std::endl;
            printExpression(x->condition, indent+4);
            std::cout << std::string(indent+2, ' ') << "Body: " << std::endl;
            printNode(x->body, indent+4);
            break;
        }
        case 6:
        {
            std::shared_ptr<ForLoop> x = statement.forLoop;
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
            std::shared_ptr<ReturnCall> x = statement.returnCall;
            std::cout << std::string(indent, ' ') << "Return call: " << std::endl;
            printExpression(x->toReturn, indent+2);
            break;
        }
        case 8:
        {
            std::shared_ptr<BreakCall> x = statement.breakCall;
            if (x->abrupt) {
                std::cout << std::string(indent, ' ') << "Break call" << std::endl;
            } else {
                std::cout << std::string(indent, ' ') << "Continue call" << std::endl;
            }
            break;
        }
        case 9:
        {
            std::shared_ptr<InlineAsm> x = statement.inlineAsm;
            std::cout << std::string(indent, ' ') << "Inline assembly: " << std::endl;
            std::cout << std::string(indent+2, ' ') << x->code << std::endl;
            break;
        }
    }
}

class Parser {
public:
    Parser(const std::vector<Token>& tokens) : tokens(tokens), current(0), scope(0), scopeTree({0}) {}
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
    int scope;
    std::vector<int> scopeTree;
    DataType returnType;

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
    
    bool matchScope(std::vector<int> toMatch) {
        if (toMatch.size() > scopeTree.size()) {
            return false;
        }
        for (int i = 0; i < scopeTree.size(); ++i) {
            if (toMatch[i] != scopeTree[i]) {
                return false;
            }
        }
        return true;
    }

    DataType getType(Expression expr) {
        if (expr.type == Expression::TernaryOperationType) return (getType(expr.ternaryOperation->a).type == DataType::AmbiguousInteger ? getType(expr.ternaryOperation->b) : getType(expr.ternaryOperation->a));
        if (expr.type == Expression::AssignmentType) return expr.assignment->variable.var.dataType;
        if (expr.type == Expression::VariableReferenceType) return expr.variableReference->var.dataType;
        if (expr.type == Expression::FunctionCallType) return funtable[expr.functionCall->identifier].returnType;
        if (expr.type == Expression::CastType) return expr.cast->to;
        if (expr.type == Expression::OperationType) return (getType(expr.operation->a).type == DataType::AmbiguousInteger ? getType(expr.operation->b) : getType(expr.operation->a));
        if (expr.type == Expression::LiteralExpressionType) {
            if (expr.literalExpression->type == LiteralExpression::Array) {
                DataType* dt = new DataType(getType(expr.literalExpression->array[0]));
                DataType array = DataType(dt, expr.literalExpression->array.size());
                return array;
            } else if (expr.literalExpression->type == LiteralExpression::TemplateInit) {
                std::vector<TemplateItem> templ;
                for (int i = 0; i <expr.literalExpression->array.size(); i++) {
                    templ.push_back({"", new DataType(getType(expr.literalExpression->array[i])), Expression(), false});
                }
                return DataType(new std::vector<TemplateItem>(templ));;
            } else if (expr.literalExpression->type == LiteralExpression::Floating) {
                return DataType(DataType::Float);
            } else {
                return DataType(DataType::AmbiguousInteger);
            }
        }
        return DataType();
    }

    void matchType(DataType dta, DataType dtb) {
        
        std::cout << dta.type;
        if (dta.type == DataType::Array) {
            std::cout << " : " << dta.arraySize;
            std::cout << " : " << dta.arrayType->type;
        }
        std::cout << " -> " << dta.pointer;
        std::cout << " / " << dtb.type;
        if (dtb.type == DataType::Array) {
            std::cout << " : " << dtb.arraySize;
            std::cout << " : " << dtb.arrayType->type;
        }
        std::cout << " -> " << dtb.pointer;
        std::cout<<"\n"<<std::endl;

        if (dta.type == DataType::Array && dtb.pointer) {dtb.pointer = 0; matchType(*dta.arrayType, dtb); return;};
        if (dtb.type == DataType::Array && dta.pointer) {dta.pointer = 0; matchType(*dtb.arrayType, dta); return;
        };
        if ((dta.type == DataType::Array) != (dtb.type == DataType::Array)) error(TypeMismatch, tokens[current]);
        if (dta.type == DataType::Array && (dta.arraySize != dtb.arraySize)) error(TypeMismatch, tokens[current]);
        if (dta.type == DataType::AmbiguousInteger || dtb.type == DataType::AmbiguousInteger) return;
        if (dta.pointer != dtb.pointer) error(TypeMismatch, tokens[current]);
        if (dta.type != dtb.type) error(TypeMismatch, tokens[current]);
        if (dta.type == DataType::Array) matchType(*dta.arrayType, *dtb.arrayType);
        if (dta.type == DataType::Template) {
            if (dta.templ->size() != dtb.templ->size()) error(TypeMismatch, tokens[current]);
            auto it1 = dta.templ->begin();
            auto it2 = dtb.templ->begin();
            while (it1 != dta.templ->end() && it2 != dtb.templ->end()) {
                matchType(*it1->dataType, *it2->dataType);
                it1++; it2++;
            }
        }
    }

    void matchType(Expression a, Expression b) {
        DataType dta = getType(a);
        DataType dtb = getType(b);
        matchType(dta, dtb);
    }

    void checkVariable(bool definition = true) {
        if (definition) {
            if (vartable.find(tokens[current].value) != vartable.end()) {
                std::vector<int> s = vartable[tokens[current].value].scope;
                if (matchScope(s)) {
                    error(VariableAlreadyDefined, tokens[current]);
                }
            }
        } else {
            if (vartable.find(tokens[current].value) == vartable.end()) {
                error(UndefinedVariable, tokens[current]);
            }
            std::vector<int> s = vartable[tokens[current].value].scope;
            if (!matchScope(s)) {
                error(UndefinedVariable, tokens[current]);
            }
        }
    }

    Statement parseStatement() {
        std::cout << "Parsing token: " << tokens[current].value << std::endl;
        if      (tokens[current].value == "if")         return parseIfStatement();
        else if (tokens[current].value == "switch")     return parseSwitchStatement();
        else if (tokens[current].value == "while" ||
                 tokens[current].value == "do")         return parseWhileLoop();
        else if (tokens[current].value == "for")        return parseForLoop();
        else if (tokens[current].value == "{")          return parseBlock();
        else if (tokens[current].value == "return")     return parseReturnCall();
        else if (tokens[current].value == "break")      return parseBreakCall();
        else if (tokens[current].value == "continue")   return parseContinueCall();
        else if (tokens[current].value == "auto" ||
                 tokens[current].value == "static" ||
                 tokens[current].value == "extrn" ||
                 tokens[current].value == "int" ||
                 tokens[current].value == "short" ||
                 tokens[current].value == "long" ||
                 tokens[current].value == "char" ||
                 tokens[current].value == "double" ||
                 tokens[current].value == "float" ||
                 templatetable.find(tokens[current].value) != templatetable.end()
                                                 )      return parseDeclaration();
        else if (tokens[current].value == "fn")         return parseFunctionDefinition();
        else if (tokens[current].value == "template")   return parseTemplateDefinition();
        else if (tokens[current].value == "asm")        return parseInlineAsm();
        else if (tokens[current].value == ";")          {current++; return Statement();}
        else if (tokens[current].type == EoF)           error(UnexpectedEOF, tokens[current]);
        else                                            {Statement ret = parseExpression(); if (tokens[current].value==";") current++; return ret;}
        return Statement(); // to make gcc SHUT THE FUCK UP ABOUT CONTROL REACHING END OF NON-VOID FUNCTION. ITS NOT GONNA HAPPEN.
    }

    bool theAllegedMemberWasActuallyRevealedToBeAFunctionInTheEnd = false;
    FunctionCall* saidAllegedMember;

    VariableReference parseMember(VariableReference var) {
        VariableReference ret;
        if (tokens[current].value == "[") {
            current++; // Skip "["
            Expression exp = parseExpression().expression;
            current++; // Skip "]"
            if (getType(exp).type == DataType::Template || getType(exp).type == DataType::Array) error(TypeMismatch, tokens[current-1]);
            // member: array
            if (var.type == VariableReference::Value) {
                if (var.var.dataType.type != DataType::Array) {
                    error(TypeMismatch, tokens[current]);
                }
                ret = VariableReference(Expression(new Operation(Expression(new VariableReference(VariableReference::Reference, var.var)), exp, "+")), *var.var.dataType.arrayType);
            } else if (var.position.type == Expression::VariableReferenceType){
                ret = VariableReference(Expression(new Operation(Expression(new VariableReference(var)), exp, "+")), *var.var.dataType.arrayType);
            } else {
                ret = VariableReference(Expression(new Operation(var.position, exp, "+")), *var.var.dataType.arrayType);
            }
        } else if (tokens[current].value == ".") {
            // template method/member access
            if (var.var.dataType.type != DataType::Template || var.var.dataType.pointer) {
                error(TypeMismatch, tokens[current]);
            }
            current++; // Skip "."
            // check if method or member
            if (tokens[current+1].value == "(") {
                //handle function
                std::string key = "";
                for (const auto& pair : templatetable) {
                    if (pair.second == *var.var.dataType.templ) {
                        key = pair.first;
                        break;
                    }
                }
                saidAllegedMember = new FunctionCall();
                saidAllegedMember->identifier = tokens[current++].value;
                if (methodtable[key].find(saidAllegedMember->identifier) == methodtable[key].end()) {
                    error(UndefinedFunction, tokens[current-1]);
                }
                saidAllegedMember->arguments.push_back(Expression(new VariableReference(var)));
                current++; // Skip "("
                while (tokens[current].value != ")") {
                    saidAllegedMember->arguments.push_back(parseExpression().expression);
                    if (tokens[current].value == ",") current++; // Skip the commas
                }
                current++; // Skip ")"
                
                if (saidAllegedMember->arguments.size() != methodtable[key][saidAllegedMember->identifier].parameters.size()) {
                    if (saidAllegedMember->arguments.size()>methodtable[key][saidAllegedMember->identifier].parameters.size()) {
                        error(ParameterMismatch, tokens[current-1]);
                    }
                    for (int i = saidAllegedMember->arguments.size(); i < methodtable[key][saidAllegedMember->identifier].parameters.size(); i++) {
                        if (methodtable[key][saidAllegedMember->identifier].defaults[i].type != Expression::Other) {
                            saidAllegedMember->arguments.push_back(methodtable[key][saidAllegedMember->identifier].defaults[i]);
                        } else {
                            error(ParameterMismatch, tokens[current-1]);
                        }
                    }
                }
               
                theAllegedMemberWasActuallyRevealedToBeAFunctionInTheEnd = true;
                return var;
            } else {
                // handle member
                DataType type;
                int s = 0;
                bool found = 0;
                for (int i = 0; i < var.var.dataType.templ->size(); i++) {
                    if (var.var.dataType.templ->at(i).id == tokens[current].value) {
                        type = *var.var.dataType.templ->at(i).dataType;
                        found = true;
                        break;
                    }
                    s += var.var.dataType.templ->at(i).dataType->size();
                }
                if (!found) {
                    error(NonexistingMember, tokens[current]);
                }
                current++;
                ret = VariableReference(Expression(new Operation(Expression(new VariableReference(VariableReference::Reference, var.var)), Expression(new LiteralExpression(s)), "+")), type, true);
            }
        } else if (tokens[current].value == "->") {
            // template member access for pointers
            if (var.var.dataType.type != DataType::Template || !var.var.dataType.pointer) {
                error(TypeMismatch, tokens[current]);
            }
            current++; // Skip "->"
            // check if method or member
            if (tokens[current+1].value == "(") {
                //handle function
                std::string key = "";
                for (const auto& pair : templatetable) {
                    if (pair.second == *var.var.dataType.templ) {
                        key = pair.first;
                        break;
                    }
                }
                saidAllegedMember = new FunctionCall();
                saidAllegedMember->identifier = tokens[current++].value;
                if (methodtable[key].find(saidAllegedMember->identifier) == methodtable[key].end()) {
                    error(UndefinedFunction, tokens[current-1]);
                }
                var.position = Expression(new VariableReference(var));
                var.var.dataType.pointer = false;
                var.type = VariableReference::Pointer;
                saidAllegedMember->arguments.push_back(Expression(new VariableReference(var)));
                current++; // Skip "("
                while (tokens[current].value != ")") {
                    saidAllegedMember->arguments.push_back(parseExpression().expression);
                    if (tokens[current].value == ",") current++; // Skip the commas
                }
                current++; // Skip ")"
                
                if (saidAllegedMember->arguments.size() != methodtable[key][saidAllegedMember->identifier].parameters.size()) {
                    if (saidAllegedMember->arguments.size()>methodtable[key][saidAllegedMember->identifier].parameters.size()) {
                        error(ParameterMismatch, tokens[current-1]);
                    }
                    for (int i = saidAllegedMember->arguments.size(); i < methodtable[key][saidAllegedMember->identifier].parameters.size(); i++) {
                        if (methodtable[key][saidAllegedMember->identifier].defaults[i].type != Expression::Other) {
                            saidAllegedMember->arguments.push_back(methodtable[key][saidAllegedMember->identifier].defaults[i]);
                        } else {
                            error(ParameterMismatch, tokens[current-1]);
                        }
                    }
                }
               
                theAllegedMemberWasActuallyRevealedToBeAFunctionInTheEnd = true;
                return var;
            } else {
                DataType type;
                int s = 0;
                bool found = 0;
                for (int i = 0; i < var.var.dataType.templ->size(); i++) {
                    if (var.var.dataType.templ->at(i).id == tokens[current].value) {
                        type = *var.var.dataType.templ->at(i).dataType;
                        found = true;
                        break;
                    }
                    s += var.var.dataType.templ->at(i).dataType->size();
                }
                if (!found) {
                    error(NonexistingMember, tokens[current]);
                }
                current++;
                ret = VariableReference(Expression(new Operation(Expression(new VariableReference(var)), Expression(new LiteralExpression(s)), "+")), type, true);
            }
        } else {
            return var;
        }
        return parseMember(ret);
    }

    Expression parseIdentifier(bool declaration) {
        VariableReference* var = new VariableReference();
        if (tokens[current].value == "*" && !declaration) {
            // dereference pointer
            current++; // Skip "*"
            Expression exp = parseExpression(true).expression;
            if (exp.type == Expression::VariableReferenceType) {
                if (!exp.variableReference->var.dataType.pointer) {
                    error(TypeMismatch, tokens[current]);
                }
                var = new VariableReference(*exp.variableReference);
                var->var.dataType.pointer = false;
                var->position = exp;
            } else {
                var = new VariableReference(exp, VariableData(DataType(DataType::Int)));
            }
            var->type = VariableReference::Pointer;
        } else if (tokens[current].value == "&" && !declaration) {
            // reference
            current++; // Skip "&"
            checkVariable(false);
            var = new VariableReference(VariableReference::Value, vartable[tokens[current].value]);
            current++;
            var = new VariableReference(parseMember(*var));
            var->var.dataType.pointer = true;
            var->type = VariableReference::Reference;
            return var;
        } else {
            // value
            std::string id = sanitize(Identifier);
            checkVariable(false);
            var = new VariableReference(VariableReference::Value, vartable[id]);
            current++; // Skip self
        }
        if (declaration) {
            return Expression(var);
        } else {
            VariableReference ret = parseMember(*var);
            if (theAllegedMemberWasActuallyRevealedToBeAFunctionInTheEnd) {
                return Expression(saidAllegedMember);
            } else {
                return Expression(new VariableReference(ret));
            }
        }
    }

    LiteralExpression parseArray() {
        LiteralExpression lit;
        lit.type = LiteralExpression::Array;
        if (tokens[current].value == "[") {
            current++; // Skip "["
            while (tokens[current].value != "]") {
                Expression exp = parseExpression().expression;
                lit.array.push_back(exp);
                matchType(lit.array[0], exp);
                if (tokens[current].value == ",") current++; // Skip the commas
            }
            current++; // Skip "]"
        } else if (tokens[current].value == "\"") {
            current++; // Skip "\""
            for (int i = 0; i < tokens[current].value.length(); i++) {
                lit.array.push_back(new LiteralExpression(tokens[current].value[i]));
            }
            current++; // Skip the string itself
            current++; // Skip "\""
        } else {
            error(UnexpectedToken, tokens[current]);
        }
        return lit;
    }

    LiteralExpression parseTemplateLiteral() {
        LiteralExpression lit;
        lit.type = LiteralExpression::TemplateInit;
        current++; // Skip "<"
        while (tokens[current].value != ">") {
            lit.array.push_back(parseExpression().expression);
            if (tokens[current].value == ",") current++; // Skip the commas
        }
        current++; // Skip ">"
        return lit;
    }

    Statement parseExpression(bool single = false) {
        std::vector<Expression> expressions;
        std::vector<std::string> op;
        
        while (tokens[current].value != ";" && tokens[current].value != ")" && tokens[current].value != "," && tokens[current].value != "]" && !(tokens[current].value == ">" && (tokens[current+1].value == ";" || tokens[current+1].value == "," || tokens[current+1].value == ">" || tokens[current+1].value == "]")) && current < tokens.size()-1) {
            if (tokens[current].type == Identifier && tokens[current+1].value == "(") {
                FunctionCall* ret = new FunctionCall();
                ret->identifier = tokens[current++].value;
                if (funtable.find(ret->identifier) == funtable.end()) {
                    error(UndefinedFunction, tokens[current-1]);
                }
                current++; // Skip "("
                while (tokens[current].value != ")") {
                    ret->arguments.push_back(parseExpression().expression);
                    if (tokens[current].value == ",") current++; // Skip the commas
                }
                if (ret->arguments.size() != funtable[ret->identifier].parameters.size()) {
                    if (ret->arguments.size()>funtable[saidAllegedMember->identifier].parameters.size()) {
                        error(ParameterMismatch, tokens[current-1]);
                    }
                    for (int i = ret->arguments.size(); i < funtable[ret->identifier].parameters.size(); i++) {
                        if (funtable[ret->identifier].defaults[i].type != Expression::Other) {
                            ret->arguments.push_back(funtable[ret->identifier].defaults[i]);
                        } else {
                            error(ParameterMismatch, tokens[current-1]);
                        }
                    }
                }
                for (int i = 0; i < ret->arguments.size(); i++) {
                    matchType(getType(ret->arguments[i]), funtable[ret->identifier].parameters[i].dataType);
                }
                current++; // Skip ")"
                expressions.push_back(ret);
            } else if (tokens[current].value == "auto" || tokens[current].value == "static") {
                // Assignment coming from declaration
                current++; // Skip storage duration
                if (tokens[current].value == "int" || tokens[current].value == "short" || tokens[current].value == "long"  || tokens[current].value == "float"  || tokens[current].value == "double"  || tokens[current].value == "char" || templatetable.find(tokens[current].value) != templatetable.end()) current++; // Skip data type
                if (tokens[current].value == "*") current++; // Skip pointer thingy
                expressions.push_back(parseIdentifier(true));
                while (tokens[current].value != "=") current++;
            } else if (tokens[current].value == "sizeof") {
                current++; // Skip self
                int size = getType(parseExpression(true).expression).size();
                LiteralExpression* ret = new LiteralExpression(size);
                expressions.push_back(Expression(ret));
            } else if (tokens[current].type == Identifier || (tokens[current].value == "*" && (current > 0 || tokens[current-1].type != Identifier)) || (tokens[current].value == "&" && tokens[current+1].type == Identifier)) {
                // Variable
                 expressions.push_back(parseIdentifier(false));
            } else if (tokens[current].value == "[" || tokens[current].value == "\"") {
                // Array
                LiteralExpression* arr = new LiteralExpression(parseArray());
                expressions.push_back(Expression(arr));
            } else if (tokens[current].value == "<" && current != 0 && (tokens[current-1].value == "," || tokens[current-1].value == "<" || tokens[current-1].value == "=" || tokens[current-1].value == ";" || tokens[current-1].value == "[")) {
                // Template init
                LiteralExpression* templ = new LiteralExpression(parseTemplateLiteral());
                expressions.push_back(Expression(templ));
            } else if (tokens[current].type == Literal || tokens[current].value == "'") {
                // Literal
                LiteralExpression* lit = new LiteralExpression();

                if (isdigit(tokens[current].value[0]) || tokens[current].value[0] == '-') {
                    *lit = LiteralExpression(stoi(tokens[current].value)); // TODO: floats
                    if (tokens[current+1].value == ".") {
                        current++;
                        std::string fl = std::to_string(lit->integer) + "." + tokens[current+1].value;
                        lit->floating = std::stof(fl);
                        lit->type = LiteralExpression::Floating;
                    }
                } else {
                    skip("'");
                    *lit = LiteralExpression(tokens[current].value[0]);
                    current++; // Skip "'"
                }
                current++; // Skip self
                expressions.push_back(Expression(lit));
            } else if (tokens[current].value == "int" || tokens[current].value == "short" || tokens[current].value == "long"  || tokens[current].value == "float"  || tokens[current].value == "double"  || tokens[current].value == "char") {
                Cast* ret = new Cast();
                DataType dt;
                if (tokens[current].value == "int") {
                    dt.type = DataType::Int;
                    current++;
                } else if (tokens[current].value == "short") {
                    dt.type = DataType::Short;
                    current++;
                } else if (tokens[current].value == "long") {
                    dt.type = DataType::Long;
                    current++;
                } else if (tokens[current].value == "char") {
                    dt.type = DataType::Char;
                    current++;
                } else if (tokens[current].value == "float") {
                    dt.type = DataType::Float;
                    current++;
                } else if (tokens[current].value == "double") {
                    dt.type = DataType::Double;
                    current++;
                }
                if (tokens[current].value == "*") { dt.pointer = true; current++; }
                ret->to = dt;
                ret->from = parseExpression().expression;
                if (!getType(ret->from).pointer && (getType(ret->from).type == DataType::Array || getType(ret->from).type == DataType::Template)) error(OperationNotAllowed, tokens[current-1]);
                expressions.push_back(Expression(ret));
            } else if (tokens[current].value == "(") {
                current++; // Skip "("
                expressions.push_back(parseExpression().expression);
                current++; // Skip ")"
            }
            if (tokens[current].type == Punctuation && tokens[current].value != ";" && tokens[current].value != ")" && tokens[current].value != "," && tokens[current].value != "]" && !(tokens[current].value == ">" && (tokens[current+1].value == ";" || tokens[current+1].value == "," || tokens[current+1].value == ">" || tokens[current+1].value == "]"))) {
                if (single) {
                    return expressions[0];
                }
                if (expressions.size() != op.size()+1) {
                    expressions.push_back(Expression());
                }
                op.push_back(tokens[current++].value);
            } else {
                op.push_back("");
            }
        }  
        return Statement(parseOperation(expressions, op));
    }

    const std::string augment[15] = {"+=","-=",">>=","<<=","/=","*=","%=","^=","&=","|=","!=","~=", "++", "--", "!!"};
    
    Expression parseOperation(std::vector<Expression> expressions, std::vector<std::string> op) {
        if (op[0] == "" || op[0] == ":") {
            return expressions[0];
        } else if (op[0] == "=") {
            // Assignment
            Assignment* ret = new Assignment();
            ret->variable = *expressions[0].variableReference;
            expressions.erase(expressions.begin());
            op.erase(op.begin());
            ret->value = parseOperation(expressions, op);

            // Empty template inits
            if (ret->variable.var.dataType.type == DataType::Template) {
                if (getType(ret->value).type == DataType::Template) {
                    if (getType(ret->value).size() == 0) {
                        LiteralExpression* lit = new LiteralExpression();
                        lit->type = LiteralExpression::TemplateInit;
                        for (int i = 0; i < ret->variable.var.dataType.templ->size(); i++) {
                            if (ret->variable.var.dataType.templ->at(i).hasDefaultValue) {
                                lit->array.push_back(ret->variable.var.dataType.templ->at(i).defaultValue);
                            } else {
                                lit->array.push_back(Expression(new LiteralExpression(0)));
                            }
                        }
                        ret->value = Expression(lit);
                    }
                }
            }

            matchType(ret->value, Expression(new VariableReference(ret->variable)));
            return Expression(ret);
        } else if (std::find(std::begin(augment), std::end(augment), op[0]) != std::end(augment)) {
            // Augmented assignment
            Assignment* ret = new Assignment();
            ret->augmentedAssignment = true;
            ret->variable = *expressions[0].variableReference;
            if (ret->variable.var.dataType.type == DataType::Array || ret->variable.var.dataType.type == DataType::Template) error(OperationNotAllowed, tokens[current-1]);
            Operation* value = new Operation();
            std::string operation(1, op[0][0]);
            value->a = expressions[0];
            if (op[0] == "++" || op[0] == "--" || op[0] == "!!") {
                value->b = Expression(new LiteralExpression(1));
            } else {
                if (operation == ">") operation = ">>";
                if (operation == "<") operation = "<<";
                expressions.erase(expressions.begin());
                op.erase(op.begin());
                value->b = parseOperation(expressions, op);
                matchType(value->a, value->b);
            }
            value->operation = operation;
            ret->value = Expression(value);
            return (Expression(ret));
        } else if (op[0] == "?") {
            TernaryOperation* ret = new TernaryOperation();
            ret->condition = expressions[0];
            expressions.erase(expressions.begin());
            op.erase(op.begin());
            ret->a = parseOperation(expressions, op);
            while (op[0] != ":") {
                expressions.erase(expressions.begin());
                op.erase(op.begin());
            }
            expressions.erase(expressions.begin());
            op.erase(op.begin());
            ret->b = parseOperation(expressions, op);
            matchType(ret->a, ret->b);
            return Expression(ret);
        } else if (op[0] == "~" || op[0] == "!" || op[0] == "-") {
            Operation* ret = new Operation();
            ret->operation = op[0];
            expressions.erase(expressions.begin());
            op.erase(op.begin());
            ret->a = parseOperation(expressions, op);
            if (getType(ret->a).type == DataType::Template || getType(ret->a).type == DataType::Array) error(OperationNotAllowed, tokens[current]);
            ret->unary = true;
            return Expression(ret);
        } else {
            Operation* ret = new Operation();
            ret->operation = op[0];
            ret->a = expressions[0];
            expressions.erase(expressions.begin());
            op.erase(op.begin());
            ret->b = parseOperation(expressions, op);
            if (getType(ret->a).type == DataType::Template || getType(ret->a).type == DataType::Array) error(OperationNotAllowed, tokens[current]);
            if (getType(ret->b).type == DataType::Template || getType(ret->b).type == DataType::Array) error(OperationNotAllowed, tokens[current]);
            matchType(ret->a, ret->b);
            return Expression(ret);
        }
        return Expression();
    }

    Statement parseBlock() {
        Block* ret = new Block();
        current++; // Skip "{"
        scope++;
        scopeTree.push_back(scope);
        while (tokens[current].value != "}") {
            ret->statements.push_back(parseStatement());
        }
        scopeTree.pop_back();
        current++; // Skip "}"
        return Statement(ret);
    }

    Statement parseFunctionDefinition(bool header = false) {
        FunctionDefinition* ret = new FunctionDefinition();
        current++; // Skip "fn"
        if (tokens[current].value == "int") {
            ret->returnType.type = DataType::Int;
            current++;
        } else if (tokens[current].value == "short") {
            ret->returnType.type = DataType::Short;
            current++;
        } else if (tokens[current].value == "long") {
            ret->returnType.type = DataType::Long;
            current++;
        } else if (tokens[current].value == "char") {
            ret->returnType.type = DataType::Char;
            current++;
        } else if (tokens[current].value == "float") {
            ret->returnType.type = DataType::Float;
            current++;
        } else if (tokens[current].value == "double") {
            ret->returnType.type = DataType::Double;
            current++;
        } else if (templatetable.find(tokens[current].value) != templatetable.end()) {
            ret->returnType.type = DataType::Template;
            ret->returnType.templ = &templatetable[tokens[current].value];
            current++;
        } else {
            ret->returnType.type = DataType::Int;
        }
        if (tokens[current].value == "*") {ret->returnType.pointer = true; current++;}
        returnType = ret->returnType.type;
        
        std::string templ = "";
        if (tokens[current+1].value == "::") {
            templ = tokens[current++].value;
            if (templatetable.find(templ) == templatetable.end()) {
                error(NonexistingTemplate, tokens[current-1]);
            }
            current++; // Skip "::"
        }
        ret->identifier = tokens[current++].value;
        if (funtable.find(ret->identifier) != funtable.end()) {
            error(FunctionAlreadyDefined, tokens[current-1]);
        }
        skip("("); // Skip "("

        scope++;
        scopeTree.push_back(scope);

        if (templ != "") {
            VariableData self;
            self.dataType = DataType(&templatetable[templ]);
            self.identifier = "self";
            self.scope = scopeTree;
            ret->defaults.push_back(Expression());
            ret->parameters.push_back(self);
            vartable["self"] = self;
        }

        while (tokens[current].value != ")") {
            ret->parameters.push_back(parseVariable(false, false));
            ret->defaults.push_back(Expression());
            if (tokens[current].value == "=") {
                current++; // Skip "="
                Expression exp = parseExpression(true).expression;
                if (exp.type != Expression::LiteralExpressionType) {
                    error(OperationNotAllowed, tokens[current]);
                }
                matchType(ret->parameters[ret->parameters.size()-1].dataType, getType(exp));
                ret->defaults[ret->defaults.size()-1] = exp;
            } else {
                if (ret->defaults.size() > 1 && ret->defaults[ret->defaults.size()-2].type != Expression::Other) {
                    error(UnexpectedToken, tokens[current]);
                }
            }
            if (tokens[current].value == ",") current++; // Skip the commas
        }

        scopeTree.pop_back();
        scope--;

        current++; // Skip ")"
        if (!header) {
            ret->body = parseStatement();
        }
        if (templ == "") {
            funtable[ret->identifier] = *ret;
        } else {
            methodtable[templ][ret->identifier] = *ret;
        }
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
                scope++;
                scopeTree.push_back(scope);
                while (tokens[current].value != "case" && tokens[current].value != "default" && tokens[current].value != "}") {
                    switchblock->statements.push_back(parseStatement());
                }
                scopeTree.pop_back();
                ret->branches[cs] = Statement(switchblock);
            } else if (tokens[current].value == "default") {
                current++; // Skip "default"
                current++; // Skip ":"
                Block* switchblock = new Block();
                scope++;
                scopeTree.push_back(scope);
                while (tokens[current].value != "case" && tokens[current].value != "}") {
                    switchblock->statements.push_back(parseStatement());
                }
                scopeTree.pop_back();
                ret->defaultPresent = true;
                ret->defaultBranch = Statement(switchblock);
            }
        }
        current++; // Skip "}"
        return Statement(ret);
    }

    Statement parseWhileLoop() {
        WhileLoop* ret = new WhileLoop();
        if (tokens[current].value == "do") {
            current++; // Skip "do"
            ret->doWhile = true;
            ret->body = parseStatement();
            skip("while"); // Skip "("
            skip("("); // Skip "("
            ret->condition = parseExpression().expression;
            current++; // Skip ")"
        } else {
            current++; // Skip "while"
            skip("("); // Skip "("
            ret->condition = parseExpression().expression;
            current++; // Skip ")"
            ret->body = parseStatement();
        }
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
            matchType(getType(ret->toReturn), returnType);
        }
        
        current++; // Skip ";"
        return Statement(ret);
    }

    Statement parseBreakCall() {
        BreakCall* ret = new BreakCall();
        ret->abrupt = true;
        current++; // Skip "break"
        skip(";");
        return Statement(ret);
    }

    Statement parseContinueCall() {
        BreakCall* ret = new BreakCall();
        ret->abrupt = false;
        current++; // Skip "continue"
        skip(";");
        return Statement(ret);
    }

    Statement parseInlineAsm() {
        InlineAsm* ret = new InlineAsm();
        current++; // Skip "asm"
        skip("\"");
        ret->code = tokens[current++].value;
        skip("\"");
        skip(";");
        return Statement(ret);
    }

    VariableData parseVariable(bool autoSize, bool global, bool abstract = false) {
        VariableData var;
        if (global) var.scope = {0};
        else var.scope = scopeTree;
        if (tokens[current].value == "int") {
            var.dataType.type = DataType::Int;
            current++;
        } else if (tokens[current].value == "short") {
            var.dataType.type = DataType::Short;
            current++;
        } else if (tokens[current].value == "long") {
            var.dataType.type = DataType::Long;
            current++;
        } else if (tokens[current].value == "char") {
            var.dataType.type = DataType::Char;
            current++;
        } else if (tokens[current].value == "float") {
            var.dataType.type = DataType::Float;
            current++;
        } else if (tokens[current].value == "double") {
            var.dataType.type = DataType::Double;
            current++;
        } else if (templatetable.find(tokens[current].value) != templatetable.end()) {
            var.dataType.type = DataType::Template;
            var.dataType.templ = &templatetable[tokens[current].value];
            current++;
        } else {
            var.dataType.type = DataType::Int;
        }

        if (tokens[current].value == "*") {
            var.dataType.pointer = true;
            current++;
        }
        
        var.identifier = sanitize(Identifier);
        if (!abstract) {
            checkVariable();
        }
        current++; // Skip identifier
        while (tokens[current].value == "[") {
            var.dataType.arrayType = new DataType(var.dataType);
            
            var.dataType.type = DataType::Array;
            current++; // Skip "["
            if (tokens[current].value == "]" && autoSize) {
                current++; // Skip "]"
                int startpos = current;
                if (tokens[current].value != "=") error(UnexpectedToken, tokens[current]);
                current++; // Skip "="
                LiteralExpression arr = parseArray();
                var.dataType.arraySize = arr.array.size();
                current = startpos;
            } else {
                var.dataType.arraySize = stoi(sanitize(Literal));
                current++; // Skip literal
                current++; // Skip "]"
            }
        }
        if (!abstract) {
            vartable[var.identifier] = var;
        }
        return var;
    }

    Statement parseDeclaration() {
        std::string duration = tokens[current].value;
        int startpos = current;
        current++; // Skip "auto"/"static"/"extrn"
        if (duration == "extrn") {
            if (tokens[current].value == "fn") {
                parseFunctionDefinition();
                goto end;
            }
            parseVariable(false, true);
            skip(";");
        } else if (duration == "static") {
            parseVariable(true, true);
            if (tokens[current].value == "=") {
                current = startpos;
                Statement ret = parseExpression();
                current++; // Skip ";"
                return ret;
            }
            if (tokens[current].value != ";") {
                error(UnexpectedToken, tokens[current]);
            }
            current++; // Skip ";"
        } else if (duration == "auto") {
            parseVariable(true, false);
            if (tokens[current].value == "=") {
                current = startpos;
                Statement ret = parseExpression();
                current++; // Skip ";"
                return ret;
            }
            if (tokens[current].value != ";") {
                error(UnexpectedToken, tokens[current]);
            }
            current++; // Skip ";"
        }
        end:
        return Statement();
    }

    Statement parseTemplateDefinition() {
        current++; // Skip "template"/"union"
        std::string id = sanitize(Identifier);
        current++; // Skip identifier
        std::vector<TemplateItem> ret;
        skip("<");
        while (tokens[current].value != ">") {
            std::string varName = tokens[current+1].value;
            DataType* dt = new DataType(parseVariable(false, true, true).dataType);
            if (tokens[current].value == "=") {
                current++; // Skip "="
                Expression exp = parseExpression().expression;
                if (exp.type != Expression::LiteralExpressionType) {
                    error(OperationNotAllowed, tokens[current]);
                }
                matchType(*dt, getType(exp));
                ret.push_back({varName, dt, exp, true});
            } else {
                ret.push_back({varName, dt, Expression(), false});    
            }
            if (tokens[current].value == ",") current++; // Skip the commas
        }
        templatetable[id] = ret;
        current++; // Skip ">"
        skip(";");
        return Statement();
    }
};


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
                if (source[currentPos+1] == ':') {
                    tokens.push_back({Punctuation, "::", line, column});
                    column++; currentPos++;
                } else {
                    tokens.push_back({Punctuation, ":", line, column});
                }
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
                } else if (source[currentPos+1] == '>') {
                    tokens.push_back({Punctuation, "->", line, column});
                    column++; currentPos++;
                } else if (isdigit(source[currentPos+1]) && tokens[tokens.size()-1].type != Identifier) {
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
                        currentPos++;
                    }  
                    line++; column = 0;
                } else if (source[currentPos+1] == '*') {
                    column+=2; currentPos+=2;
                    while (!(source[currentPos] == '*' && source[currentPos+1] == '/')) {
                        currentPos++;
                        if (currentPos == source.size()) {
                            error(UnexpectedEOF, {Punctuation, "\"", line, column});
                        }
                        if (source[currentPos] == '\n') {
                            line++;
                            column = 0;
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
                tokens.push_back({Unknown, std::to_string(source[currentPos]), line, column});
                column++; currentPos++;
            }
            
        }
        tokens.push_back({EoF, "", line, column});
        sourceCode = source;
        return tokens;
    }

private:
    std::string source;
    size_t currentPos;
    unsigned int line, column;
    std::string keywords[27] = {
        // Structures
        "if", "else", "while", "for", "do", "switch", "case", "default", "break", "continue", "fn", "return",
        // Data types
        "char", "short", "int", "long", "float", "double", "template",
        // Storage duration
        "auto", "static", "extrn",
        // Other  
        "sizeof", "asm",
        // Reserved (not in present use)
        "bool", "union", "enum",
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
        while (currentPos < source.size() && (isalnum(source[currentPos]) || source[currentPos] == '_')) {
            id += source[currentPos++];
            column++;
        }

        if (macros.size() > 0) {
            if (macros.find(id) != macros.end()) {
                id = macros[id];
            }
        }

        for (int i = 0; i < 27; i++) {
            if (keywords[i] == id) {
                return {Keyword, id};
            }
        }

        return {Identifier, id, line, (unsigned int)(column-(id.length()))};
    }

    Token number(bool negative = false) {
        unsigned int startcol = column;
        std::string num = negative ? "-" : "";
        while (currentPos < source.size() && isdigit(source[currentPos])) {
            num += source[currentPos++];
            column++;
            if ((source[currentPos] == 'x' || source[currentPos] == 'X') && num == "0") {
                column++; currentPos++;
                num = "";
                while (currentPos < source.size() && (isalnum(source[currentPos]))) {
                    num += source[currentPos++];
                    column++;
                }
                return {Literal, std::to_string(stoi(num, nullptr, 16)), line, startcol};
            } else if ((source[currentPos] == 'b' || source[currentPos] == 'B') && num == "0") {
                column++; currentPos++;
                num = "";
                while (currentPos < source.size() && (isdigit(source[currentPos]))) {
                    num += source[currentPos++];
                    column++;
                }
                return {Literal, std::to_string(stoi(num, nullptr, 2)), line, startcol};
            }
        }
        return {Literal, num, line, startcol};
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
                    error(BadMacroName, {Identifier, "", line-1, 0});
            }
            if (isalpha(directives[1][0]) || directives[1][0] == '_') {
                if (directives.size() > 2) {
                    macros[directives[1]] = directives[2];
                }
            } else {
                error(BadMacroName, {Identifier, directives[1], line-1, 0});
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
            if (directives.size() < 2 || directives[1][0] == '\n' || directives[1][0] == ' ') {
                error(FileNotFound, {Identifier, "", line-1, 0});
            }
            std::ifstream include(directives[1]);
            if (!include.is_open()) {
                error(FileNotFound, {Identifier, directives[1], line-1, 0});
            }
            std::stringstream buffer;
            buffer << include.rdbuf(); 
            source.insert(currentPos, buffer.str()+"\n");
        } else {
            // TODO: conditional!
            error(InvalidPreprocessingDirective, {Keyword, directives[0], line-1, 1});
        }
    }
};

int main(int argc, char* argv[]) {
    if (argc > 1) {
        filename = argv[1];
        std::ifstream f(filename);
        if (!f.is_open()) {
            std::cerr<<filename<<": No such file or directory."<<std::endl;
            exit(1);
        }
        std::stringstream buffer;
        buffer << f.rdbuf(); 
        sourceCode = buffer.str();
    }
    
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

    std::cout<< "Parsing over." << std::endl;
    for (int i = 0; i < ast.size(); i++) {
        printNode(ast[i], 0);
    }

    // debug!
    for (const auto& pair : vartable) {
        std::cout<<"Variable name: "<<pair.first<<"\t";
        std::cout << "Scope: ";
        for (int i = 0; i < pair.second.scope.size(); i++) {
            std::cout << pair.second.scope[i] << " -> ";
        }
        std::cout<<"\b\b\b\b   \t";
        std::cout << "Size: " << pair.second.dataType.size()<< "\t";
        std::cout << "Type: " << pair.second.dataType.type<< "\n";
    }

    for (const auto& templ : templatetable) {
        std::cout<<"Template: "<<templ.first<<"\n";
        for (const auto& item : templ.second) {
            std::cout<<"  Variable name: "<<item.id<<"\t";
            std::cout<<"\b\b\b\b   \t";
            std::cout << "  Size: " << item.dataType->size()<< "\t";
            std::cout << "  Type: " << item.dataType->type<< "\t";
            std::cout << "  Array: " << item.dataType->arraySize;
            if (item.hasDefaultValue) {
                std::cout << "  Default: ";
                printExpression(item.defaultValue,0);
            }
            std::cout << "\n";
        }
    }
}